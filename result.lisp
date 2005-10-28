;;;; $Id: result.lisp,v 1.1 2005/10/28 13:18:04 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/result.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

;;; This implementation contains two interfaces to the results,
;;; or stanzas, sent to us by the server.  The first is the DOM-ish
;;; representation used with xml-element and xml-attribute.  It is
;;; a very simple representation and doesn't care much about
;;; namespaces or any semantics or the XML it parses other than
;;; trying to represent the structure of it.  It attaches the
;;; original XML element to itself so you can have a peek if you
;;; think something might be missing.  But please, do not start
;;; to rely on the NODE slot of these objects as I would rather
;;; you add to this dumb DOM-ish implementation if something is
;;; missing from it.

;;; The second interface is the collection of classes which all
;;; subclass the EVENT class.  This interface is designed for
;;; programmers who simply wish to use the library to write an
;;; application in a straightforward event-driven manner.  It
;;; hides even more of the gory details but should give you
;;; correct and appropriate events at all times.

;;; Also please note that it's not event-based in that you are
;;; strictly receiving events.  Eg. you will receive a roster
;;; object if you call GET-ROSTER, not a received-roster event.
;;; We'll see if this is confusing or not.

;;; In the end, I don't know if this will be sufficient.  It is
;;; for me at present time as all I really wanted to do was play
;;; around with XMPP in CL.  If you have an idea which you wish
;;; were implemented in this library or perhaps you know a better
;;; way of doing this please don't hesitate to speak up as I
;;; most likely won't have much to do with this library from
;;; now on.

(defclass xml-element ()
  ((name
    :accessor name
    :initarg :name)
   (node
    :accessor node
    :initarg :node
    :initform nil
    :documentation "Attaching CXML DOM node here but please
do not rely on it beyond introspection.  If you find yourself
in need of getting data from it stick it somewhere in the
cl-xmpp-created data and access it that way instead.")
   (attributes
    :accessor attributes
    :initarg :attributes
    :initform nil)
   (data
    :accessor data
    :initarg :data
    :initform nil)
   (elements
    :accessor elements
    :initarg :elements
    :initform nil)))

(defmethod data (object)
  nil)

(defmethod print-object ((object xml-element) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a (~a:~a:~a)"
	    (name object)
	    (length (attributes object))
	    (length (elements object))
	    (length (data object)))))

(defmethod get-attribute ((element xml-element) name &key (test 'string-equal))
  (dolist (attribute (attributes element))
    (when (funcall test name (name attribute))
      (return-from get-attribute attribute))))

(defmethod get-element ((element xml-element) name &key (test 'string-equal))
  (dolist (subelement (elements element))
    (when (funcall test name (name subelement))
      (return-from get-element subelement))))

(defclass xml-attribute ()
  ((name
    :accessor name
    :initarg :name)
   (value
    :accessor value
    :initarg :value
    :initform nil)
   (node
    :accessor node
    :initarg :node
    :initform nil)))

(defmethod value (object)
  nil)

(defmethod print-object ((object xml-attribute) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a=~a" (name object) (value object))))

;;
;; Produce DOM-ish structure from the XML DOM returned by cxml.
;;

(defmethod parse-result ((objects list))
  (mapcar #'parse-result objects))

(defmethod parse-result ((document dom-impl::document))
  (let (objects)
    (dom:map-node-list #'(lambda (node)
			   (push (parse-result node) objects))
		       (dom:child-nodes document))
    objects))

(defmethod parse-result ((attribute dom-impl::attribute))
  (let* ((name (dom:node-name attribute))
	 (value (dom:value attribute))
	 (xml-attribute
	  (make-instance 'xml-attribute
			 :name name :value value :node attribute)))
    xml-attribute))

(defmethod parse-result ((node dom-impl::character-data))
  (let* ((name (dom:node-name node))
	 (data (dom:data node))
	 (xml-element (make-instance 'xml-element
				     :name name :data data :node node)))
    xml-element))

(defmethod parse-result ((node dom-impl::node))
  (let* ((name (dom:node-name node))
	 (xml-element (make-instance 'xml-element :name name :node node)))
    (dom:do-node-list (attribute (dom:attributes node))
      (push (parse-result attribute) (attributes xml-element)))
    (dom:do-node-list (child (dom:child-nodes node))
      (push (parse-result child) (elements xml-element)))
    xml-element))

;;
;; Error
;;

(defclass xmpp-protocol-error ()
  ((code
    :accessor code
    :initarg :code)
   (name
    :accessor name
    :initarg :name)))

(defclass xmpp-protocol-error-modify (xmpp-protocol-error) ())
(defclass xmpp-protocol-error-cancel (xmpp-protocol-error) ())
(defclass xmpp-protocol-error-wait (xmpp-protocol-error) ())
(defclass xmpp-protocol-error-auth (xmpp-protocol-error) ())

(defun get-error-data (name)
  (assoc name *errors*))

(defun map-error-type-to-class (type)
  (case type
    (modify (find-class 'xmpp-protocol-error-modify))
    (cancel (find-class 'xmpp-protocol-error-cancel))
    (wait (find-class 'xmpp-protocol-error-wait))
    (auth (find-class 'xmpp-protocol-error-auth))))

;;; If an error element occurs within a, say, message element
;;; do I want to include the error within the message, the
;;; message within the error, or discard the message and just
;;; return the error?  I'm thinking the second option.
(defmethod make-error ((object xml-element))
  (let* ((name (intern (string-upcase (name (car (elements object)))) :keyword))
	 (data (get-error-data name))
	 (type (second data))
	 (code (third data))
	 (class (map-error-type-to-class type)))
    (make-instance class :code code :name name :type type)))

;;
;; Event interface
;;

(defclass event () ())

(defclass message (event)
  ((to
    :accessor to
    :initarg :to
    :initform nil)
   (from
    :accessor from
    :initarg :from
    :initform nil)
   (body
    :accessor body
    :initarg :body
    :initform "")))

(defmethod print-object ((object message) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "to:~a from:~a" (to object) (from object))))

;;; XXX: Add support for the <thread/> element.  Also note that
;;; there may be an XHTML version of the body available in the
;;; original node but as of right now I don't care about it.  If
;;; you do please feel free to submit a patch.
(defmethod xml-element-to-event ((object xml-element) (name (eql :message)))
  (make-instance 'message
		 :from (value (get-attribute object "from"))
		 :to (value (get-attribute object "to"))
		 :body (data (get-element (get-element object "body") "#text"))))

(defclass presence (event)
  ((to
    :accessor to
    :initarg :to
    :initform nil)
   (from
    :accessor from
    :initarg :from
    :initform nil)
   (show
    :accessor show
    :initarg :show
    :initform nil)
   (type-
    :accessor type-
    :initarg :type-
    :initform nil)))

(defmethod print-object ((object presence) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "from:~a show:~a" (from object) (show object))))

;;; XXX: Is the ask attribute of the <presence/> element part of the RFC/JEP?
(defmethod xml-element-to-event ((object xml-element) (name (eql :presence)))
  (let ((show (get-element object "show")))
    (when show
      (setq show (data (get-element show "#text"))))
    (make-instance 'presence
		   :from (value (get-attribute object "from"))
		   :to (value (get-attribute object "to"))
		   :show show
		   :type- (value (get-attribute object "type")))))

(defclass contact ()
  ((jid
    :accessor jid
    :initarg :jid)
   (name
    :accessor name
    :initarg :name
    :initform "")
   (subscription
    :accessor subscription
    :initarg :subscription
    :initform nil)))

(defmethod print-object ((object contact) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a (~a)" (jid object) (name object))))

(defclass roster (event)
  ((items
    :accessor items
    :initarg :items
    :initform nil)))

(defmethod print-object ((object roster) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a contact(s)" (length (items object)))))

(defmethod make-roster ((object xml-element))
  (let ((roster (make-instance 'roster)))
    (dolist (item (elements (get-element object "query")))
      (let ((jid (value (get-attribute item "jid")))
	    (name (value (get-attribute item "name")))
	    (subscription (value (get-attribute item "subscription"))))
	(push (make-instance 'contact :jid jid :name name :subscription subscription)
	      (items roster))))
    roster))

;;; XXX: I think I want to make all IDs keywords.
(defmethod xml-element-to-event ((object xml-element) (name (eql :iq)))
  (let ((id (intern (string-upcase (value (get-attribute object "id"))) :keyword)))
    (case id
      (:roster_1 (make-roster object))
      (t name))))

(defmethod xml-element-to-event ((object xml-element) (name (eql :error)))
  (make-error object))

(defmethod xml-element-to-event ((object xml-element) name)
  name)

(defmethod dom-to-event ((object list))
  (mapcar #'dom-to-event object))

(defmethod dom-to-event ((object xml-element))
  (xml-element-to-event
   object (intern (string-upcase (name object)) :keyword)))

;;
;; Handle
;;

(defmethod handle ((object list))
  (mapc #'handle object))

(defmethod handle (object)
  (format t "~&Received: ~a~%" object))