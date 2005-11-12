;;;; $Id: cl-xmpp-sasl.lisp,v 1.4 2005/11/12 02:29:51 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/cl-xmpp-sasl.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

;;; XXX: Remember to BIND after these, I think.
(defmethod %sasl-plain% ((connection connection) username password resource)
  (handle-challenge-response connection username password "PLAIN"))

(add-auth-method :sasl-plain #'%sasl-plain%)

(defmethod %sasl-digest-md5% ((connection connection) username password resource)
  (handle-challenge-response connection username (digestify-string password) "DIGEST-MD5"))

(add-auth-method :sasl-digest-md5 #'%sasl-digest-md5%)

(defmethod handle-challenge-response ((connection connection) username password mechanism)
  "Helper method to the sasl authentication methods.  Goes through the
entire SASL challenge/response chain.  Returns two values, the first
is a keyword symbol (:success or :failure) and the second is the last
stanza received from the server."
  (initiate-sasl-authentication connection mechanism)
  (let ((initial-challenge (receive-stanza connection)))
    (if (eq (name initial-challenge) :challenge)
	(let* ((challenge-string (base64:base64-string-to-string
				  (data (get-element initial-challenge :\#text))))
	       (sasl-client (make-instance (sasl:get-mechanism mechanism)
					   :authentication-id username
					   :password password
					   :service "xmpp"
					   :realm (hostname connection)
					   :host (hostname connection)))
	       (response (sasl:client-step sasl-client (ironclad:ascii-string-to-byte-array challenge-string)))
	       (base64-response (base64:string-to-base64-string response)))
	  (format *debug-stream* "~&challenge-string: ~a~%" challenge-string)
	  (format *debug-stream* "response: ~a~%" response)
	  (if (eq response :failure)
              (values :failure initial-challenge)
	    (progn
	      (send-challenge-response connection base64-response)
	      (let ((second-challenge (receive-stanza connection)))
		(if (eq (name second-challenge) :challenge)
		    (progn
		      (send-second-response connection)
                      (let ((final-reply (receive-stanza connection)))
		        ; This should return either :success or :failure.
                        (values (name final-reply) final-reply)))
                  (values :failure second-challenge))))))
      (values :failure initial-challenge))))

(defmethod initiate-sasl-authentication ((connection connection) mechanism)
  (with-xml-stream (stream connection)
   (xml-output stream (fmt "<auth mechanism='~a'
xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>" mechanism))))

(defmethod send-challenge-response ((connection connection) response)
  (with-xml-stream (stream connection)
   (xml-output stream
    (fmt "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>~a</response>" response))))

(defmethod send-second-response ((connection connection))
  (with-xml-stream (stream connection)
   (xml-output stream "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>")))

