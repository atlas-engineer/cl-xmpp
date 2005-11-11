;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

(defmethod send-starttls ((connection connection))
  "Sends a request to start a TLS stream with the server.
There are some things you as a user of this library need
to know about this:

  1) You should test for the presence of a starttls element
     in the features slot of the connection and only call this
     method if it is present.

  2) Following your call to this method you should look for
     either a proceed or a failure from the server.

     a) If you get a proceed you may call begin-tls-stream and
        your connection is now secure (though read step 3).

     b) If you get a failure your connection is automatically
        torn down by the server and you lose.

  3) After begin-tls-stream you must proceed with sasl-auth
     instead of the regular auth."
  (with-xml-stream (stream connection)
   (xml-output stream "<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>")))

(defmethod begin-tls-stream ((connection connection))
  "Convert the existing stream to a TLS stream and issue
a stream:stream open tag to start the XML stream."
  (setf (server-stream connection)
	(cl+ssl:make-ssl-client-stream (server-stream connection)))
  (begin-xml-stream connection))

(defmethod sasl-auth ((connection) username password resource)
  nil)