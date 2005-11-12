;;;; $Id: cl-xmpp-tls.lisp,v 1.2 2005/11/12 04:20:21 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/cl-xmpp-tls.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

(defun connect-tls (&rest args)
  "Connect to the host and start a TLS stream."
  (let ((connection (apply #'connect args)))
    (send-starttls connection)
    (convert-to-tls-stream connection)
    connection))

(defmethod send-starttls ((connection connection))
  "Sends a request to start a TLS stream with the server."
  (with-xml-stream (stream connection)
   (xml-output stream "<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>")))

(defmethod convert-to-tls-stream ((connection connection))
  "Convert the existing stream to a TLS stream and issue
a stream:stream open tag to start the XML stream."
  (setf (server-stream connection)
	(cl+ssl:make-ssl-client-stream (server-stream connection)))
  (begin-xml-stream connection))
