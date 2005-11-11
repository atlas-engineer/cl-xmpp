;;;; $Id: cl-xmpp-sasl.lisp,v 1.1 2005/11/11 17:21:56 eenge Exp $
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
  (initiate-sasl-authentication connection mechanism)
  (let ((initial-challenge (receive-stanza connection)))
    (if (eq (name initial-challenge) :challenge)
	(let* ((challenge-string (base64:base64-string-to-string
				  (data (get-element initial-challenge :\#text))))
	       (sasl-client (make-instance (sasl:get-mechanism mechanism)
					   :authentication-id username
					   :password password
					   :service "xmpp"
					   :host (hostname connection)))
	       (response (sasl:client-step sasl-client challenge-string))
	       (base64-response (base64:string-to-base64-string response)))
	  (format *debug-stream* "~&challenge-string: ~a~%" challenge-string)
	  (format *debug-stream* "response: ~a~%" response)
	  (if (eq response :failure)
	      (error "SASL failure: ~a." challenge-string)
	    (progn
	      (send-challenge-response connection base64-response)
	      (let ((second-challenge (receive-stanza connection)))
		(if (eq (name second-challenge) :challenge)
		    (progn
		      (send-second-response connection)
		      ; This should return either :success or :failure.
		      (name (receive-stanza connection)))
		  (error "Expected second challenge, got: ~a." second-challenge))))))
      (error "Expected initial challenge, got: ~a." initial-challenge))))

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

