;;;; $Id: result.lisp,v 1.5 2005/10/29 17:25:04 eenge Exp $
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
  (let* ((name (intern (string-upcase (dom:node-name node)) :keyword))
	 (xml-element (make-instance 'xml-element :name name :node node)))
    (dom:do-node-list (attribute (dom:attributes node))
      (push (parse-result attribute) (attributes xml-element)))
    (dom:do-node-list (child (dom:child-nodes node))
      (push (parse-result child) (elements xml-element)))
    xml-element))

;;
;; Event interface
;;

(defclass event ()
  ((xml-element
    :accessor xml-element
    :initarg :xml-element
    :initform nil)))

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
                 :xml-element object
		 :from (value (get-attribute object :from))
		 :to (value (get-attribute object :to))
		 :body (data (get-element (get-element object :body) :\#text))))

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
  (let ((show (get-element object :show)))
    (when show
      (setq show (data (get-element show :\#text))))
    (make-instance 'presence
                   :xml-element object
		   :from (value (get-attribute object :from))
		   :to (value (get-attribute object :to))
		   :show show
		   :type- (value (get-attribute object :type)))))

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
  (let ((roster (make-instance 'roster :xml-element object)))
    (dolist (item (elements (get-element object :query)))
      (let ((jid (value (get-attribute item :jid)))
	    (name (value (get-attribute item :name)))
	    (subscription (value (get-attribute item :subscription))))
	(push (make-instance 'contact :jid jid :name name :subscription subscription)
	      (items roster))))
    roster))

;;
;; Discovery
;;

(defclass identity- (event)
  ((category
    :accessor category
    :initarg :category)
   (type-
    :accessor type-
    :initarg :type-)
   (name
    :accessor name
    :initarg :name)))

(defmethod make-identity ((object xml-element))
  (make-instance 'identity-
                 :xml-element object 
                 :category (value (get-attribute object :category))
                 :type- (value (get-attribute object :type-))
                 :name (value (get-attribute object :name))))

(defclass disco (event)
  ((identities
    :accessor identities
    :initarg :identities
    :initform nil)))
    
(defclass feature (event)
  ((var
    :accessor var
    :initarg :var
    :initform "")))

(defmethod make-feature ((object xml-element))
  (make-instance 'feature :xml-element object :var (value (get-attribute object :var))))

(defclass disco-info (disco)
  ((features
    :accessor features
    :initarg :features
    :initform nil)))

(defmethod make-disco-info ((object xml-element))
  (let ((disco-info (make-instance 'disco-info :xml-element object)))
    (dolist (element (elements object))
      (case (name element)
        (:identity (push (make-identity element) (identities disco-info)))
        (:feature (push (make-feature element) (features disco-info)))))
    disco-info))

(defclass item (event)
  ((jid
    :accessor jid
    :initarg :jid)
   (name
    :accessor name
    :initarg :name)
   (node
    :accessor node
    :initarg :node
    :initform nil)))

(defmethod make-item ((object xml-element))
  (make-instance 'item
                 :xml-element object 
                 :jid (value (get-attribute object :jid))
                 :node (value (get-attribute object :node))
                 :name (value (get-attribute object :name))))

(defclass disco-items (disco)
  ((items
    :accessor items
    :initarg :items
    :initform nil)))

(defmethod make-disco-items ((object xml-element))
  (let ((disco-items (make-instance 'disco-items :xml-element object)))
    disco-items))

;;
;; Error
;;

(defclass xmpp-protocol-error (event)
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
    (auth (find-class 'xmpp-protocol-error-auth))
    (t (find-class 'xmpp-protocol-error))))

(defmethod make-error ((object xml-element))
  (let* ((name (intern (string-upcase (name (car (elements object)))) :keyword))
	 (data (get-error-data name))
	 (type (second data))
	 (code (third data))
	 (class (map-error-type-to-class type)))
    (make-instance class :code code :name name :xml-element object)))

(defmethod xml-element-to-event ((object xml-element) (name (eql :iq)))
  (let ((id (intern (string-upcase (value (get-attribute object :id))) :keyword)))
    (if (not (string-equal (value (get-attribute object :type)) "result"))
	(make-error (get-element object :error))
      (case id
	(:error (make-error (get-element object :error)))
	(:roster_1 (make-roster object))
	(:reg2 :registration-successful)
	(:unreg_1 :registration-cancellation-successful)
	(:change1 :password-changed-succesfully)
	(:auth2 :authentication-successful)
	(t (cond
	    ((member id '(info1 info2 info3))
	     (make-disco-info (get-element object :query)))
	    ((member id '(items1 items2 items3 items4))
	     (make-disco-items (get-element object :query)))))))))

(defmethod xml-element-to-event ((object xml-element) (name (eql :error)))
  (make-error object))

(defmethod xml-element-to-event ((object xml-element) (name (eql :stream\:error)))
  (make-error object))

(defmethod xml-element-to-event ((object xml-element) name)
  (declare (ignore name))
  object)

(defmethod dom-to-event ((object list))
  (mapcar #'dom-to-event object))

(defmethod dom-to-event ((object xml-element))
  (xml-element-to-event
   object (intern (string-upcase (name object)) :keyword)))

;;
;; Handle
;;

(defmethod handle ((connection connection) (object list))
  (dolist (object list)
    (handle connection object)))

(defmethod handle ((connection connection) object)
  (format t "~&Received: ~a~%" object))
