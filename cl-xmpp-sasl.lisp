;;;; $Id: cl-xmpp-sasl.lisp,v 1.12 2008/07/09 19:53:19 ehuelsmann Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/cl-xmpp-sasl.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

(defmethod if-successful-restart-stream ((connection connection) reply)
  (if (eq reply :authentication-successful)
      (progn
	(begin-xml-stream connection :xml-identifier nil)
        ;; Clean the server-source.
        ;; See https://support.process-one.net/browse/EJAB-455
        (setf (server-source connection) nil)
	(receive-stanza connection) ; stream
	(receive-stanza connection) ; features
	reply) 
    reply))

(defmethod %sasl-plain% ((connection connection) username password resource)
  (let* ((mechanism "PLAIN")
	 (sasl-client (make-instance (sasl:get-mechanism mechanism)
				     :authentication-id username
				     :password password
				     :service "xmpp"
				     :host (hostname connection))))
    (format *debug-stream* "~&SASL state: ~a~&" (sasl::state sasl-client))
    (initiate-sasl-authentication connection mechanism sasl-client)
    (if-successful-restart-stream connection (receive-stanza connection))))

(add-auth-method :sasl-plain '%sasl-plain%)

(defmethod %sasl-digest-md5% ((connection connection) username password resource)
  (if-successful-restart-stream
   connection
   (handle-challenge-response connection username password resource "DIGEST-MD5")))

(add-auth-method :sasl-digest-md5 '%sasl-digest-md5%)

(defmethod handle-challenge-response ((connection connection) username password
				      resource mechanism)
  "Helper method to the sasl authentication methods.  Goes through the
entire SASL challenge/response chain.  Returns two values, the first
is a keyword symbol (:success or :failure) and the second is the last
stanza received from the server."
  (let ((sasl-client (make-instance (sasl:get-mechanism mechanism)
                                    :authentication-id username
                                    :password password
                                    :service "xmpp"
                                    :host (hostname connection))))
    (format *debug-stream* "~&SASL state: ~a~&" (sasl::state sasl-client))
    (initiate-sasl-authentication connection mechanism sasl-client)
    (let ((initial-challenge (receive-stanza connection)))
      (if (eq (name initial-challenge) :challenge)
          (let* ((challenge-string (base64:base64-string-to-string
                                    (data (get-element initial-challenge :\#text))))
                 (usb8-response (sasl:client-step 
                                 sasl-client 
                                 (ironclad:ascii-string-to-byte-array challenge-string))))
	    (format *debug-stream* "~&SASL state: ~a~&" (sasl::state sasl-client))
            (format *debug-stream* "challenge-string: ~a~%" challenge-string)
            (if (eq usb8-response :failure)
                (values :failure initial-challenge)
              (let ((base64-response (base64:usb8-array-to-base64-string usb8-response)))
                (format *debug-stream* "response: ~a~%"
			(map 'string #'code-char usb8-response))
                (force-output *debug-stream*)
                (send-challenge-response connection base64-response)
                (let ((second-challenge (receive-stanza connection)))
		  (format *debug-stream* "second-challenge: ~a~&" second-challenge)
                  (if (eq (name second-challenge) :challenge)
                      (progn
                        (send-second-response connection)
			(when (eq (receive-stanza connection)
				  :authentication-successful)
			  (begin-xml-stream connection)
			  (reset-stream connection)))
		    second-challenge)))))
	initial-challenge))))

(defun reset-stream (connection)
  (setf (server-source connection)
	(cxml:make-source
	 (cxml::source-xstream (server-source connection))
	 :buffering nil)))

(defmethod initiate-sasl-authentication ((connection connection) mechanism sasl-client)
  (with-xml-stream (stream connection)
   (xml-output
    stream
    (if (string-equal mechanism "plain")
	(fmt "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='~a'>~a</auth>"
	     mechanism
	     (base64:usb8-array-to-base64-string (sasl:client-step sasl-client nil)))
      (fmt "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='~a'/>" mechanism)))))

(defmethod send-challenge-response ((connection connection) response)
  (with-xml-stream (stream connection)
   (xml-output stream
    (fmt "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>~a</response>" response))))

(defmethod send-second-response ((connection connection))
  (with-xml-stream (stream connection)
   (xml-output stream "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>")))
