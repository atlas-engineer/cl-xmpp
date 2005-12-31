;;;; $Id: cxml.lisp,v 1.9 2005/11/18 23:14:35 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/cxml.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

(defclass stanza-handler (cxml:sax-proxy)
  ((depth
    :initform 0
    :accessor depth)))

(defun start-sax-document (handler)
  (let ((dom-builder (cxml-dom:make-dom-builder)))
    (setf (cxml:proxy-chained-handler handler) dom-builder)
    (sax:start-document dom-builder)
    dom-builder))

(defmethod sax:start-element ((handler stanza-handler) uri lname qname attrs)
  (declare (ignore uri lname))
  (when (eql (depth handler) 0)
    (if (eq :stream\:stream (ensure-keyword qname))
        ;; Create an element for DOM-TO-EVENT so we don't have to have
        ;; any specialized code just to handle stream:stream.
        (let* ((document (cxml-dom:create-document))
               (element (dom:create-element document qname)))
          (dom:append-child document element)
          (dolist (attribute attrs)
            (let ((name (sax::attribute-qname attribute))
                  (value (sax::attribute-value attribute)))
              (dom:set-attribute element name value)))
          (throw 'stanza document))
        (start-sax-document handler)))
  (incf (depth handler))
  (call-next-method))

;;; END-ELEMENT will try and call the stanza-callback at every time
;;; it sees depth reach 0 and there is a callback to be called.
;;; This means that we can keep reading from the stream and as we
;;; close out elements we parse them and return them to users
;;; using callbacks (the one supplied to RECEIVE-STANZA-LOOP).
(defmethod sax:end-element ((handler stanza-handler) uri lname qname)
  (declare (ignore uri lname qname))
  (decf (depth handler))
  (call-next-method)
  (when (eql (depth handler) 0)
    (throw 'stanza
      (cxml-dom::document (cxml:proxy-chained-handler handler)))))

;;; Perform single-byte reads to avoid blocking on the socket.
(defstruct (slow-stream (:constructor make-slow-stream (target)))
  (target nil :type stream))

(defmethod runes::figure-encoding ((stream slow-stream))
  (runes::figure-encoding (slow-stream-target stream)))

(defmethod runes::read-octets (seq (stream slow-stream) start end)
  (when (< start end)
    (let ((byte (read-byte (slow-stream-target stream) nil)))
      (when byte
	(setf (elt seq start) byte)
	(incf start))))
  start)

;; I'd like to see what CXML is reading from the stream
;; and this code helps us in that regard by printing it
;; to the *debug-stream*
(defun runes::write-xstream-buffer (xstream &optional (stream *debug-stream*))
  (when stream
    (write-string (map 'string
		       #'code-char
		       (remove runes::+end+
			       (subseq (runes::xstream-buffer xstream) 0
				       (runes::xstream-read-ptr xstream))))
		  stream)
    (force-output stream)))

(defmethod runes::xstream-underflow :before ((input runes:xstream))
  (runes::write-xstream-buffer input))