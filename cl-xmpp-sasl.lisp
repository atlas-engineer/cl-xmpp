;;;; $Id: cl-xmpp-sasl.lisp,v 1.8 2005/11/14 19:21:06 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/cl-xmpp-sasl.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

;;; XXX: Remember to BIND after this, I think.
(defmethod %sasl-digest-md5% ((connection connection) username password resource)
  (handle-challenge-response connection username password "DIGEST-MD5"))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (add-auth-method :sasl-digest-md5 #'%sasl-digest-md5%))

(defmethod handle-challenge-response ((connection connection) username password mechanism)
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
                  (if (eq (name second-challenge) :challenge)
                      (progn
                        (send-second-response connection)
                        (let ((final-reply (receive-stanza connection)))
		          ; name should be either :success or :failure.
                          (values (name final-reply) final-reply)))
                    (values :failure second-challenge))))))
        (values :failure initial-challenge)))))

(defmethod initiate-sasl-authentication ((connection connection) mechanism sasl-client)
  (with-xml-stream (stream connection)
   (xml-output
    stream
    (fmt "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='~a'/>" mechanism))))

(defmethod send-challenge-response ((connection connection) response)
  (with-xml-stream (stream connection)
   (xml-output stream
    (fmt "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>~a</response>" response))))

(defmethod send-second-response ((connection connection))
  (with-xml-stream (stream connection)
   (xml-output stream "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>")))
