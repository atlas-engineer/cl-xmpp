;;;; $Id: cl-xmpp-tls.lisp,v 1.5 2005/11/16 19:06:12 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/cl-xmpp-tls.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

(defun connect-tls (&rest args)
  "Connect to the host and start a TLS stream."
  (let ((connection (apply #'connect args)))
    (send-starttls connection)
    (let ((reply (receive-stanza connection)))
      (case (name reply)
	(:proceed 
	 (let ((begin-xml-stream (if (member :begin-xml-stream args)
				     (getf args :begin-xml-stream)
				   t))
	       (receive-stanzas (if (member :begin-xml-stream args)
				    (getf args :begin-xml-stream)
				  t)))
	   (convert-to-tls-stream connection
				  :begin-xml-stream begin-xml-stream
				  :receive-stanzas receive-stanzas)
		  (values connection :proceed reply)))
	(:failure (values connection :failure reply))
	(t (error "Unexpected reply from TLS negotiation: ~a." reply))))))

(defmethod send-starttls ((connection connection))
  "Sends a request to start a TLS stream with the server."
  (with-xml-stream (stream connection)
   (xml-output stream "<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>")))

(defmethod convert-to-tls-stream ((connection connection) &key
				  (begin-xml-stream t)
				  (receive-stanzas t))
  "Convert the existing stream to a TLS stream and issue
a stream:stream open tag to start the XML stream.

Turn off sending XML stream start with :begin-xml-stream nil."
  (setf (server-stream connection)
	(cl+ssl:make-ssl-client-stream (server-stream connection)))
  (setf (server-xstream connection) nil)
   (when begin-xml-stream
    (begin-xml-stream connection))
   (when receive-stanzas
     (receive-stanza connection)
     (receive-stanza connection)))