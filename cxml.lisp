;;;; cxml-stanza.lisp -- parser helper for RFC 3920 XML streams
;;;; Copyright (c) 2004 David Lichteblau, BSD-style license

;;; These are modifications to CXML which helps us deal with the
;;; incremental-style parsing required for the XML stanzas.

(in-package :xmpp)

(defclass stanza-handler (cxml:sax-proxy)
  ((depth
    :initform 0
    :accessor depth)))

(defun start-sax-document (handler)
  (let ((dom-builder (dom:make-dom-builder)))
    (setf (cxml:proxy-chained-handler handler) dom-builder)
    (sax:start-document dom-builder)
    dom-builder))

(defmethod sax:start-element ((handler stanza-handler) uri lname qname attrs)
  (declare (ignore uri lname))
  (when (eql (depth handler) 0)
;    (if (and qname (string-equal "stream:stream" qname))
;	; Create an element for DOM-TO-EVENT so we don't have to have
;	; any specialized code just to handle stream:stream.
;	(let* ((document (dom:create-document))
;	       (element (dom:create-element document qname))
;	       (callback (init-callback handler)))
;	  (dolist (attribute attrs)
;	    (let ((name (sax::attribute-qname attribute))
;		  (value (sax::attribute-value attribute)))
;	      (dom:set-attribute element name value)))
;	  (when callback
;	    (funcall callback element)))
;      (start-sax-document handler)))
    (if (string-equal "stream:stream" qname)
        ;; Create an element for DOM-TO-EVENT so we don't have to have
        ;; any specialized code just to handle stream:stream.
        (let* ((document (dom:create-document))
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
;  (let ((callback (stanza-callback handler)))
;    (when (and (eql (depth handler) 0) callback)
;      (funcall callback (dom-impl::document
;			 (cxml:proxy-chained-handler handler))))))
  (when (eql (depth handler) 0)
    (throw 'stanza
      (dom-impl::document (cxml:proxy-chained-handler handler)))))

;;; The default implementation of this function in CXML does not
;;; check whether or not the nodelist is NIL and dom:length et al
;;; assumes it will be a vector.  This will result in problems
;;; because I wanted to use this with return value of DOM:ATTRIBUTES
;;; which may be NIL.  David Lichteblau said a specialized map
;;; function for namednodelists (which is what the return value of
;;; DOM:ATTRIBUTES) is could be added he just hadn't needed one
;;; yet.  So, if you want to you can write one and send him a
;;; patch.
(defun dom:map-node-list (fn nodelist)
  (when nodelist
    (dotimes (i (dom:length nodelist))
      (funcall fn (dom:item nodelist i)))))

;;; XXX: because of READ-SEQUENCE's blocking on the stream
;;; (in RUNES::READ-OCTETS) we do not call SET-TO-FULL-SPEED
;;; so that we avoid the CXML buffering layer.  I think perhaps
;;; this would work if READ-N-BYTES worked properly but I
;;; don't really know at this point.
;;;
;;; Should probably email the SBCL list about this.
(defun cxml::set-full-speed (input)
  (declare (ignore input))
  nil)
