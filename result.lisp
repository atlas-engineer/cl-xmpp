;;;; $Id: result.lisp,v 1.14 2008/07/09 21:02:40 ehuelsmann Exp $
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
;;; were implemented in this library or perhaps you know of a better
;;; way of doing this please don't hesitate to speak.

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
  (declare (ignore object))
  nil)

(defmethod print-object ((object xml-element) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a (~a:~_~a:~_~a)"
	    (name object)
            (attributes object)
            (elements object)
            (data object))))

(defmethod get-attribute ((element xml-element) name &key (test 'eq))
  (dolist (attribute (attributes element))
    (when (funcall test name (name attribute))
      (return-from get-attribute attribute))))

;; KC: The get-element function is not correct to use as is, because it
;; basically returns the first element that matches in the list.
;; It is possible to have multiple matching elements.
;; The correct solution that I have provided is to provide a get-elements
;; function that returns all matching elements and allows the user to choose
;; which they want.
;; I have not removed the old get-element function or any code that uses it.
(defmethod get-element ((element xml-element) name &key (test 'eq))
  (dolist (subelement (elements element))
    (when (funcall test name (name subelement))
      (return-from get-element subelement))))

(defmethod get-elements ((element xml-element) name &key (test 'eq))
  (loop for subelement in (elements element)
      when (funcall test name (name subelement))
      collect subelement))

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
  (declare (ignore object))
  nil)

(defmethod print-object ((object xml-attribute) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~a=~a" (name object) (value object))))

;;
;; Event interface
;;

(defclass event ()
  ((xml-element
    :accessor xml-element
    :initarg :xml-element
    :initform nil)))

(defmethod print-object ((object event) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~a" (xml-element object))))

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
    :initform "")
   (subject
    :accessor subject
    :initarg :subject
    :initform "")
   (id
    :accessor id
    :initarg :id
    :initform nil)
   (type
    :accessor type-
    :initarg :type
    :initform nil)))

(defmethod print-object ((object message) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "to:~a from:~a id:~a type:~a subject:~a"
            (to object) 
            (from object)
            (id object)
            (type- object)
            (subject object))))


(defmethod make-message ((object xml-element))
  (let ((body-element (or (get-element object :body)
                          (get-element object :x)))
        (subject-element (get-element object :subject)))
    (make-instance 'message
      :xml-element object
      :from (value (get-attribute object :from))
      :to   (value (get-attribute object :to))
      :id   (value (get-attribute object :id))
      :type (value (get-attribute object :type))
      :body (data  (and body-element (get-element body-element :\#text)))
      :subject (data (and subject-element
                          (get-element subject-element :\#text))))))


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

(defmethod print-object ((object disco-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~_~a" (identities object) (features object))))


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
;; Errors
;;

(defclass xmpp-protocol-error (event)
  ((code
    :accessor code
    :initarg :code)
   (name
    :accessor name
    :initarg :name)
   (text
    :accessor text
    :initarg :text)))

(defmethod print-object ((object xmpp-protocol-error) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "~a code:~a name:~a ~_elem:~a"
	    (type-of object)
	    (code object)
            (name object)
            (xml-element object))))

(defclass xmpp-protocol-error-modify (xmpp-protocol-error) ())
(defclass xmpp-protocol-error-cancel (xmpp-protocol-error) ())
(defclass xmpp-protocol-error-wait (xmpp-protocol-error) ())
(defclass xmpp-protocol-error-auth (xmpp-protocol-error) ())

(defun get-legacy-error-data-code (code)
  (rassoc code *legacy-errors* :key #'second))

(defun find-error-data (elements)
  "An error can be of the form <errorname> so this function searches the
   errors that we know about until we find one that's in our element list"
  (or (find-if #'(lambda (elt)
                   (member elt elements :key #'name))
                *legacy-errors* :key #'car)
      (find-if #'(lambda (elt)
                   (member elt elements :key #'name))
                *errors* :key #'car)))

(defun map-error-type-to-class (type)
  (case type
    (:modify (find-class 'xmpp-protocol-error-modify))
    (:cancel (find-class 'xmpp-protocol-error-cancel))
    (:wait (find-class 'xmpp-protocol-error-wait))
    (:auth (find-class 'xmpp-protocol-error-auth))
    (t (format *debug-stream* "~&Unable to find error class for ~w.~%" type)
       (find-class 'xmpp-protocol-error))))

;;; XXX: Handle legacy errors
(defmethod make-legacy-error ((object xml-element))
  (let* ((code-value (value (get-attribute object :code)))
         (code       (parse-integer code-value))
         (data       (get-legacy-error-data-code code))
         (name       (first  data))
         (type       (second data))
         (text       name)
         (class      (map-error-type-to-class type)))
    (make-instance class
      :xml-element object
      :code        code
      :name        name
      :text        text)))  

(defmethod make-error ((object xml-element))
  "Handle errors as defined in:
     XEP-0086 for legacy errors
     RFC-3920 for current standard
   Attempts to provide the proper mappings to bridge the two."
  (if (get-attribute object :code)
      (make-legacy-error object)
    ;; Slightly verbose but there are still cases I have not
    ;; addressed (and have no examples of, any more) so I'm going
    ;; to leave it like this for now.
    (let* ((elements  (elements object))
           ;; KC: Fixed this.   Previous code looked at the first element
           ;; which doesn't have to be the condition.
           (condition (find-error-data    elements))
           (text-elt  (get-element object :\#text))
           (text      (and text-elt (data text-elt)))
           (name      (first condition))
           (type      (second condition))
           (code      (third condition))
           (class     (map-error-type-to-class type)))
      (make-instance class
        :xml-element object
        :code        code
        :name        name
        :text        text))))

;;<iq from='darkcave@macbeth.shakespeare.lit'
;;    id='voice2'
;;    to='crone1@shakespeare.lit/desktop'
;;    type='result'/>

(defclass simple-result (event)
  ((node
    :accessor node
    :initarg :node
    :initform nil)
   (to
    :accessor to
    :initarg :to
    :initform nil)
   (from
    :accessor from
    :initarg :from
    :initform nil)
   (id
    :accessor id
    :initarg :id
    :initform nil)
   (type
    :accessor type-
    :initarg :type
    :initform nil)
   (items
    :accessor items
    :initarg :items
    :initform nil)))

(defmethod print-object ((object simple-result) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "node: ~a to:~a from:~a id:~a type:~a items: ~a"
            (node object)
            (to object)
            (from object)
            (id object)
            (type- object)
            (length (items object)))))

(defmethod make-simple-result ((object xml-element))
  (let* ((query     (first (get-elements object :query)))
         (item-list (and query (get-elements query :item))))
    (make-instance 'simple-result
      :xml-element object
      :node  (value (get-attribute query :node))
      :to    (value (get-attribute object :to))
      :from  (value (get-attribute object :from))
      :id    (value (get-attribute object :id))
      :type  (value (get-attribute object :type))
      :items (mapcar #'make-item item-list))))

#|

<message
    from='darkcave@macbeth.shakespeare.lit'
    to='hecate@shakespeare.lit'>
  <body>You have been invited to darkcave@macbeth by crone1@shakespeare.lit.</body>
  <x xmlns='http://jabber.org/protocol/muc#user'>
    <invite from='crone1@shakespeare.lit'>
      <reason>
        Hey Hecate, this is the place for all good witches!
      </reason>
    </invite>
    <password>cauldronburn</password>
  </x>
</message>

|#

(defclass invitation (message)
  ((chatroom
    :accessor chatroom
    :initarg :chatroom
    :initform nil)
   (password
    :accessor password
    :initarg :password
    :initform nil)
   (reason
    :accessor reason
    :initarg :reason
    :initform nil)))


(defmethod print-object ((object invitation) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "to:~a from:~a id:~a type:~a"
            (to       object)
            (from     object)
            (id       object)
            (type-    object)
            (chatroom object)
            (password object)
            (reason   object))))


(defparameter +invitation-node+ "http://jabber.org/protocol/muc#user")


(defmethod get-invitation ((object xml-element))
  (let ((x-elements (get-elements object :x)))
    (find-if #'(lambda (element)
                 (let ((attr (get-attribute element :xmlns)))
                   (and attr (string-equal (value attr) +invitation-node+))))
              x-elements)))


(defmethod make-invitation ((object xml-element))
  (let* ((x-element (get-invitation object))
         (invite    (and x-element (get-element x-element :invite)))
         (reason    (and invite    (get-element invite    :reason)))
         (password  (and x-element (get-element x-element :password)))
         (body      (get-element object :body)))
    (make-instance 'invitation
      :xml-element object
      :to       (value (get-attribute object :to))
      :from     (or (and invite (value (get-attribute invite :from)))
                    (value (get-attribute object :from)))
      :id       (value (get-attribute object :id))
      :type     (value (get-attribute object :type))
      :body     (and body (data  (get-element body :\#text)))
      :chatroom (value (get-attribute object :from))
      :password (and password (data  (get-element password :\#text)))
      :reason   (and reason   (data  (get-element reason   :\#text))))))
