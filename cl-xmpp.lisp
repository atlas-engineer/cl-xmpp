;;;; $Id: cl-xmpp.lisp,v 1.1.1.1 2005/10/28 13:16:02 eenge Exp $
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
#+sbcl
(defun connect (&key (hostname *default-hostname*) (port *default-port*))
  "Open TCP connection to hostname."
  (let ((socket (sb-bsd-sockets:make-inet-socket :stream :tcp))
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
		   :port port)))

#+allegro
(defun connect (&key (hostname *default-hostname*) (port *default-port*))
  "Open TCP connection to hostname."
  (let ((socket (socket:make-socket :remote-host hostname :remote-port port)))
    ;; fixme: (setf (sb-bsd-sockets:non-blocking-mode socket) t)
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
  ;(make-broadcast-stream (server-stream connection)))
  ;; FIXME: BROADCAST-STREAM doesn't actually work here because it is a
  ;; character stream, not a binary stream.  Need to come up with a
  ;; replacement.
  (server-stream connection))

(defmethod connectedp ((connection connection))
  "Returns t if `connection' is connected to a server and is ready for
input."
  (let ((stream (server-stream connection)))
    (and (streamp stream)
         (open-stream-p stream))))

#+sbcl
(defmethod disconnect ((connection connection))
  "Disconnect TCP connection."
  (sb-bsd-sockets:socket-close (socket connection))
  connection)

#+allegro
(defmethod disconnect ((connection connection))
  "Disconnect TCP connection."
  (close (socket connection))
  connection)

(defmethod receive-stanza-loop ((connection connection)	&key
				(stanza-callback 'default-stanza-callback)
				(init-callback 'default-init-callback))
;  (let ((handler (make-instance 'stanza-handler)))
;    (when stanza-callback
;      (setf (stanza-callback handler) stanza-callback))
;    (when init-callback
;      (setf (init-callback handler) init-callback))
;    (cxml:parse-stream (server-stream connection) handler)))
  (loop
    (let* ((stanza (read-stanza connection))
           (tagname (dom:tag-name (dom:document-element stanza))))
      (cond
        ((equal tagname "stream:stream")
          (when init-callback
            (funcall init-callback stanza)))
        ((equal tagname "stream:error")
          (default-stanza-callback stanza) ;print it
          (error "received error"))
        (t
          (when stanza-callback
            (funcall stanza-callback stanza)))))))

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

;;; This is mostly useful for debugging output from servers.
(defmethod get-stream-reply ((connection connection))
  "Read reply from connection's socket into a new stream
and return this stream.  This is just a way to deal with
not getting EOFs or anything like that and should probably
be replaced with more appropriate usage of the sockets."
  (let* ((output-stream (make-string-output-stream))
	 (broadcast-stream (make-broadcast-stream
			    output-stream
			    *debug-stream*)))
    (do ((line (sb-bsd-sockets:socket-receive (socket connection) nil 1)
	       (sb-bsd-sockets:socket-receive (socket connection) nil 1)))
	((or (null line)
	     (eq (aref line 0) #\Null)))
      (write-string line broadcast-stream))
    output-stream))

;;; XXX: this one should go away, too
(defmethod get-string-reply ((connection connection))
  "Read reply from connection's socket and return it as a string."
  (get-output-stream-string (get-stream-reply connection)))

(defmethod receive-stanzas ((connection connection) &key dom-repr)
  "Read reply from connection's socket and parse the result
as XML data.  Return DOM object.  If dom-repr is T the return
value will be a DOM-ish structure of xml-element/xml-attribute
objects."
  (let ((objects nil)
	(xml-string (get-string-reply connection)))
    (handler-case (push (cxml::parse-string xml-string
                         (make-instance 'stanza-handler))
			objects)
     (type-error () objects)
     (sb-kernel::arg-count-error () objects))
    (let ((result (remove nil (flatten (parse-result objects)))))
      (if dom-repr
	  result
	(dom-to-event result)))))

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

(defmacro with-iq ((connection &key id (type "get")) &body body)
  "Macro to make it easier to write IQ stanzas."
;  `(progn
;     (cxml:with-xml-output (cxml:make-octet-stream-sink
;			    (make-connection-and-debug-stream ,connection))
;      (cxml:with-element "iq"
;       (cxml:attribute "id" ,id)
;       (cxml:attribute "type" ,type)
;       ,@body))
;    ,connection))
  (let ((stream (gensym)))
    `(let ((,stream (make-connection-and-debug-stream ,connection)))
       (cxml:with-xml-output (cxml:make-octet-stream-sink ,stream)
         (cxml:with-element "iq"
           (cxml:attribute "id" ,id)
           (cxml:attribute "type" ,type)
           ,@body))
       (finish-output ,stream)
       ,connection)))

(defmacro with-iq-query ((connection &key xmlns id (type "get")) &body body)
  "Macro to make it easier to write QUERYs."
  `(progn
     (with-iq (connection :id ,id :type ,type)
      (cxml:with-element "query"
       (cxml:attribute "xmlns" ,xmlns)
       ,@body))
    ,connection))

;;
;; Basic operations
;;

;;; XXX: Add support for handling an XMPP server which announces
;;; its features.

(defmethod registration-requirements ((connection connection))
  (with-iq-query (connection :id "reg1" :xmlns "jabber:iq:register")))

(defmethod register ((connection connection) username password name email)
  (with-iq-query (connection :id "reg2" :type "set" :xmlns "jabber:iq:register")
   (cxml:with-element "username" (cxml:text username))
    (cxml:with-element "password" (cxml:text password))
    (cxml:with-element "name" (cxml:text name))
    (cxml:with-element "email" (cxml:text email))))

(defmethod auth-requirements ((connection connection) username)
  (with-iq-query (connection :id "auth1" :xmlns "jabber:iq:auth")
   (cxml:with-element "username" (cxml:text username))))

;;; XXX: Add support for digest authentication.
(defmethod auth ((connection connection) username password resource)
  (with-iq-query (connection :id "auth2" :type "set" :xmlns "jabber:iq:auth")
   (cxml:with-element "username" (cxml:text username))
   (cxml:with-element "password" (cxml:text password))
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

;;; XXX: this one doesn't seem to work with Jabberd 1.4
;;; (not insinuating that I've tested it with anything else).
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

;;; XXX: Adding and removing from the roster is not the same as
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
