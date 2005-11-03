;;;; $Id: cl-xmpp.lisp,v 1.7 2005/10/31 21:07:15 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/cl-xmpp.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

(defclass connection ()
  ((server-stream
    :accessor server-stream
    :initarg :server-stream
    :initform nil)
   (socket
    :accessor socket
    :initarg :socket
    :initform nil)
   (server-xstream
    :accessor server-xstream
    :initform nil)
   (stream-id
    :accessor stream-id
    :initarg :stream-id
    :initform nil
    :documentation "Stream ID attribute of the <stream>
element as gotten when we call BEGIN-XML-STREAM.")
   (hostname
    :accessor hostname
    :initarg :hostname
    :initform *default-hostname*)
   (port
    :accessor port
    :initarg :port
    :initform *default-port*))
  (:documentation "A TCP connection between this XMPP client and
an, assumed, XMPP compliant server.  The connection does not
know whether or not the XML stream has been initiated nor whether
there may be any reply waiting to be read from the stream.  These
details are left to the programmer."))

(defmethod print-object ((object connection) stream)
  "Print the object for the Lisp reader."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "to ~A:~A" (hostname object) (port object))
    (if (connectedp object)
	(format stream " (open)")
      (format stream " (closed)"))))

;;; XXX: "not-a-pathname"?  Need it because CXML wants to call
;;; pathname on the stream and without one it returns NIL which
;;; CXML breaks on.
(defun connect (&key (hostname *default-hostname*) (port *default-port*))
  "Open TCP connection to hostname."
  #+sbcl (let ((socket (sb-bsd-sockets:make-inet-socket :stream :tcp))
               (ip-address (car (sb-bsd-sockets:host-ent-addresses
                                 (sb-bsd-sockets:get-host-by-name hostname)))))
           (sb-bsd-sockets:socket-connect socket ip-address port)
           (setf (sb-bsd-sockets:non-blocking-mode socket) t)
           (make-instance 'connection
                          :server-stream (sb-bsd-sockets:socket-make-stream
                                          socket :input t :output t :buffering :none
                                          :element-type '(unsigned-byte 8)
                                          :pathname #p"/tmp/not-a-pathname")
                          :socket socket
                          :hostname hostname
                          :port port))
  #+(or allegro openmcl)
  (let ((socket (socket:make-socket :remote-host hostname :remote-port port)))
              ;; fixme: (setf (sb-bsd-sockets:non-blocking-mode socket) t)
              (make-instance 'connection
                             :server-stream socket
                             :socket socket
                             :hostname hostname
                             :port port))
  #+lispworks (let ((socket (comm:open-tcp-stream hostname port
						  :element-type '(unsigned-byte 8))))
                (make-instance 'connection
                               :server-stream socket
                               :socket socket
                               :hostname hostname
                               :port port)))

(defmethod make-connection-and-debug-stream ((connection connection))
  "Helper function to make a broadcast stream for this connection's
server-stream and the *debug-stream*."
  ;;; Hook onto this if you want the output written by CXML to be
  ;;; sent to one of your streams for debugging or whatever.
  (server-stream connection))

(defmethod connectedp ((connection connection))
  "Returns t if `connection' is connected to a server and is ready for
input."
  (let ((stream (server-stream connection)))
    (and (streamp stream)
         (open-stream-p stream))))

(defmethod disconnect ((connection connection))
  "Disconnect TCP connection."
  #+sbcl (sb-bsd-sockets:socket-close (socket connection))
  #+(or allegro openmcl lispworks) (close (socket connection))
  connection)

;;
;; Handle
;;

(defmethod handle ((connection connection) (list list))
  (dolist (object list)
    (handle connection object)))

(defmethod handle ((connection connection) object)
  (format t "~&Received: ~a~%" object))

;;
;; Produce DOM-ish structure from the XML DOM returned by cxml.
;;

(defmethod parse-result ((connection connection) (objects list))
  (dolist (object objects)
    (parse-result connection object)))

(defmethod parse-result ((connection connection) (document dom-impl::document))
  (let (objects)
    (dom:map-node-list #'(lambda (node)
			   (push (parse-result connection node) objects))
		       (dom:child-nodes document))
    objects))

(defmethod parse-result ((connection connection) (attribute dom-impl::attribute))
  (let* ((name (dom:node-name attribute))
	 (value (dom:value attribute))
	 (xml-attribute
	  (make-instance 'xml-attribute
			 :name name :value value :node attribute)))
    xml-attribute))

(defmethod parse-result ((connection connection) (node dom-impl::character-data))
  (let* ((name (dom:node-name node))
	 (data (dom:data node))
	 (xml-element (make-instance 'xml-element
				     :name name :data data :node node)))
    xml-element))

(defmethod parse-result ((connection connection) (node dom-impl::node))
  (let* ((name (intern (string-upcase (dom:node-name node)) :keyword))
	 (xml-element (make-instance 'xml-element :name name :node node)))
    (dom:do-node-list (attribute (dom:attributes node))
      (push (parse-result connection attribute) (attributes xml-element)))
    (dom:do-node-list (child (dom:child-nodes node))
      (push (parse-result connection child) (elements xml-element)))
    xml-element))


(defmethod xml-element-to-event ((connection connection) (object xml-element) (name (eql :iq)))
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

(defmethod xml-element-to-event ((connection connection)
				 (object xml-element) (name (eql :error)))
  (make-error object))

(defmethod xml-element-to-event ((connection connection)
				 (object xml-element) (name (eql :stream\:error)))
  (make-error object))

(defmethod xml-element-to-event ((connection connection)
				 (object xml-element) (name (eql :stream\:stream)))
  (setf (stream-id connection) (value (get-attribute object :id)))
  object)

(defmethod xml-element-to-event ((connection connection) (object xml-element) name)
  (declare (ignore name))
  object)

(defmethod dom-to-event ((connection connection) (objects list))
  (let (list)
    (dolist (object objects)
      (push (dom-to-event connection object) list))
    list))

(defmethod dom-to-event ((connection connection) (object xml-element))
  (xml-element-to-event
   connection object (intern (string-upcase (name object)) :keyword)))

;;; XXX: Is the ask attribute of the <presence/> element part of the RFC/JEP?
(defmethod xml-element-to-event ((connection connection)
				 (object xml-element) (name (eql :presence)))
  (let ((show (get-element object :show)))
    (when show
      (setq show (data (get-element show :\#text))))
    (make-instance 'presence
                   :xml-element object
		   :from (value (get-attribute object :from))
		   :to (value (get-attribute object :to))
		   :show show
		   :type- (value (get-attribute object :type)))))

;;; XXX: Add support for the <thread/> element.  Also note that
;;; there may be an XHTML version of the body available in the
;;; original node but as of right now I don't care about it.  If
;;; you do please feel free to submit a patch.
(defmethod xml-element-to-event ((connection connection)
				 (object xml-element) (name (eql :message)))
  (make-instance 'message
                 :xml-element object
		 :from (value (get-attribute object :from))
		 :to (value (get-attribute object :to))
		 :body (data (get-element (get-element object :body) :\#text))))

;;
;; Receive stanzas
;;

(defmethod receive-stanza-loop ((connection connection)	&key
                                (stanza-callback 'default-stanza-callback)
                                dom-repr)
  (loop
    (let* ((stanza (read-stanza connection))
           (tagname (dom:tag-name (dom:document-element stanza))))
      (cond
        ((equal tagname "stream:error")
          (when stanza-callback
            (funcall stanza-callback stanza connection :dom-repr dom-repr))
          (error "Received error."))
        (t
          (when stanza-callback
            (funcall stanza-callback stanza connection :dom-repr dom-repr)))))))

(defun read-stanza (connection)
  (unless (server-xstream connection)
    (setf (server-xstream connection)
          (cxml:make-xstream (server-stream connection))))
  (force-output (server-stream connection))
  (catch 'stanza
    (let ((cxml::*default-namespace-bindings*
           (acons "stream"
                  "http://etherx.jabber.org/streams"
                  cxml::*default-namespace-bindings*)))
      (cxml::parse-xstream (server-xstream connection)
                           (make-instance 'stanza-handler)))))

(defmacro with-xml-stream ((stream connection) &body body)
  "Helper macro to make it easy to control outputting XML
to the debug stream.  It's not strictly /with/ xml-stream
so it should probably be renamed."
  `(let ((,stream (make-connection-and-debug-stream ,connection)))
     ,@body))

(defun xml-output (stream string)
  "Write string to stream as a sequence of bytes and not
characters."
  (write-sequence (string-to-array string) stream)
  (finish-output stream)
  string)

(defmethod begin-xml-stream ((connection connection))
  "Begin XML stream.  This should be the first thing to
happen on a newly connected connection."
  (with-xml-stream (stream connection)
   (xml-output stream "<?xml version='1.0'?>")
   (xml-output stream (fmt "<stream:stream to='~a'
xmlns='jabber:client'
xmlns:stream='http://etherx.jabber.org/streams'
version='1.0'>" (hostname connection)))))

(defmethod end-xml-stream ((connection connection))
  "Closes the XML stream.  At this point you'd have to
call BEGIN-XML-STREAM if you wished to communicate with
the server again."
  (with-xml-stream (stream connection)
   (xml-output stream "</stream:stream>")))

(defmacro with-iq ((connection &key id to (type "get")) &body body)
  "Macro to make it easier to write IQ stanzas."
  (let ((stream (gensym)))
    `(let ((,stream (make-connection-and-debug-stream ,connection)))
       (cxml:with-xml-output (cxml:make-octet-stream-sink ,stream)
         (cxml:with-element "iq"
           (cxml:attribute "id" ,id)
           (when ,to
             (cxml:attribute "to" ,to))
           (cxml:attribute "type" ,type)
           ,@body))
       (finish-output ,stream)
       ,connection)))

(defmacro with-iq-query ((connection &key xmlns id to node (type "get")) &body body)
  "Macro to make it easier to write QUERYs."
  `(progn
     (with-iq (connection :id ,id :type ,type :to ,to)
      (cxml:with-element "query"
       (cxml:attribute "xmlns" ,xmlns)
       (when ,node
         (cxml:attribute "node" ,node))
       ,@body))
    ,connection))

;;
;; Discovery
;;

(defmethod discover ((connection connection) &key (type :info) to node)
  (let ((xmlns (case type
                 (:info "http://jabber.org/protocol/disco#info")
                 (:items "http://jabber.org/protocol/disco#items")
                 (t (error "Unknown type: ~a (Please choose between :info and :items)" type)))))
    (with-iq-query (connection :id "info1" :xmlns xmlns :to to :node node))))
  
;;
;; Basic operations
;;

(defmethod registration-requirements ((connection connection))
  (with-iq-query (connection :id "reg1" :xmlns "jabber:iq:register")))

(defmethod register ((connection connection) username password name email)
  (with-iq-query (connection :id "reg2" :type "set" :xmlns "jabber:iq:register")
   (cxml:with-element "username" (cxml:text username))
    (cxml:with-element "password" (cxml:text password))
    (cxml:with-element "name" (cxml:text name))
    (cxml:with-element "email" (cxml:text email))))

(defmethod cancel-registration ((connection connection))
  (with-iq-query (connection :id "unreg1" :type "set" :xmlns "jabber:iq:register")
   (cxml:with-element "remove")))

;;; XXX: connection should know about username?
(defmethod change-password ((connection connection) username new-password)
  (with-iq-query (connection :id "change1" :type "set" :xmlns "jabber:iq:register")
   (cxml:with-element "username"
    (cxml:text username))
   (cxml:with-element "password"
    (cxml:text new-password))))

(defmethod auth-requirements ((connection connection) username)
  (with-iq-query (connection :id "auth1" :xmlns "jabber:iq:auth")
   (cxml:with-element "username" (cxml:text username))))

(defmethod auth ((connection connection) username password resource &key digestp)
  (with-iq-query (connection :id "auth2" :type "set" :xmlns "jabber:iq:auth")
   (cxml:with-element "username" (cxml:text username))
   (if digestp
       (if (stream-id connection)
	   (cxml:with-element "digest" (cxml:text
					(make-digest-password
					 (stream-id connection)
					 password)))
	 (error "stream-id on ~a not set, cannot make digest password" connection))
     (cxml:with-element "password" (cxml:text password)))
   (cxml:with-element "resource" (cxml:text resource))))

(defmethod presence ((connection connection) &key type to)
  (cxml:with-xml-output (cxml:make-octet-stream-sink
			 (make-connection-and-debug-stream connection))
   (cxml:with-element "presence"
    (when type
      (cxml:attribute "type" type))
    (when to
      (cxml:attribute "to" to))))
  connection)
   
(defmethod message ((connection connection) to body)
  (cxml:with-xml-output (cxml:make-octet-stream-sink
			 (make-connection-and-debug-stream connection))
   (cxml:with-element "message"
    (cxml:attribute "to" to)
    (cxml:with-element "body" (cxml:text body))))
  connection)

(defmethod bind ((connection connection) jid resource)
  (with-iq (connection :id "bind_2" :type "set")
   (cxml:with-element "bind"
    (cxml:attribute "xmlns" "urn:ietf:params:xml:ns:xmpp-bind")
    (cxml:with-element "resource"
     (cxml:text resource)))))

;;
;; Subscription
;;

(defmethod request-subscription ((connection connection) to)
  (presence connection :type "subscribe" :to to))

(defmethod approve-subscription ((connection connection) to)
  (presence connection :type "subscribed" :to to))

(defmethod deny/cancel-subscription ((connection connection) to)
  (presence connection :type "unsubscribed" :to to))

(defmethod unsubscribe ((connection connection) to)
  (presence connection :type "unsubscribe" :to to))

;;
;; Roster
;;

(defmethod get-roster ((connection connection))
  (with-iq-query (connection :id "roster_1" :xmlns "jabber:iq:roster")))

;;; Note: Adding and removing from the roster is not the same as
;;; adding and removing subscriptions.  I have not yet decided
;;; if the library should provide convenience methods for doing
;;; both actions at once.
(defmethod roster-add ((connection connection) jid name group)
  (with-iq-query (connection :id "roster_2" :type "set" :xmlns "jabber:iq:roster")
   (cxml:with-element "item"
    (cxml:attribute "jid" jid)
    (cxml:attribute "name" name)
    (cxml:with-element "group" group))))

(defmethod roster-remove ((connection connection) jid)
  (with-iq-query (connection :id "roster_4" :type "set" :xmlns "jabber:iq:roster")
   (cxml:with-element "item"
    (cxml:attribute "jid" jid)
    (cxml:attribute "subscription" "remove"))))

;;
;; Privacy list
;;

;;; Implemented in Jabberd2 and on which I have not tested with.
(defmethod get-privacy-lists ((connection connection))
  (with-iq-query (connection :id "getlist1" :xmlns "jabber:iq:privacy")))

(defmethod get-privacy-list ((connection connection) name)
  (with-iq-query (connection :id "getlist2" :xmlns "jabber:iq:privacy")
   (cxml:with-element "list"
    (cxml:attribute "name" name))))

