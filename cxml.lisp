;;;; cxml-stanza.lisp -- parser helper for RFC 3920 XML streams

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
    (if (eq :stream\:stream (ensure-keyword qname))
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

(defun runes::xstream-underflow (input)
  (declare (type runes::xstream input))
  ;; we are about to fill new data into the buffer, so we need to
  ;; adjust buffer-start.
  (runes::write-xstream-buffer input)
  (incf (runes::xstream-buffer-start input)
	(- (runes::xstream-fill-ptr input) 0))
  (let (n m)
    ;; when there is something left in the os-buffer, we move it to
    ;; the start of the buffer.
    (setf m (- (runes::xstream-os-left-end input) (runes::xstream-os-left-start input)))
    (unless (zerop m)
      (replace (runes::xstream-os-buffer input) (runes::xstream-os-buffer input)
               :start1 0 :end1 m
               :start2 (runes::xstream-os-left-start input)
               :end2 (runes::xstream-os-left-end input))
      ;; then we take care that the buffer is large enough to carry at
      ;; least 100 bytes (a random number)
      (unless (>= (length (runes::xstream-os-buffer input)) 100)
        (error "You lost")
        ;; todo: enlarge buffer
        ))
    (setf n
      (runes::read-octets (runes::xstream-os-buffer input) (runes::xstream-os-stream input)
			 m (min (1- (length (runes::xstream-os-buffer input)))
				(+ m (runes::xstream-speed input)))))
    (cond ((runes::%= n 0)
           (setf (runes::xstream-read-ptr input) 0
                 (runes::xstream-fill-ptr input) n)
           (setf (aref (runes::xstream-buffer input)
		       (runes::xstream-fill-ptr input)) runes::+end+)
           :eof)
          (t
           (multiple-value-bind (fnw fnr) 
               (encoding:decode-sequence
                (runes::xstream-encoding input) 
                (runes::xstream-os-buffer input) 0 n
                (runes::xstream-buffer input) 0 (1- (length (runes::xstream-buffer input)))
                (= n m))
             (setf (runes::xstream-os-left-start input) fnr
                   (runes::xstream-os-left-end input) n
                   (runes::xstream-read-ptr input) 0
                   (runes::xstream-fill-ptr input) fnw)
             (setf (aref (runes::xstream-buffer input)
			 (runes::xstream-fill-ptr input)) runes::+end+)
             (runes:read-rune input))))))
