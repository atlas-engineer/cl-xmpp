;;;; -*- mode: lisp -*-
;;;; $Id: cl-xmpp.asd,v 1.8 2008/07/09 19:51:17 ehuelsmann Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/cl-xmpp.asd,v $

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-xmpp-system
    (:use #:cl #:asdf))

(in-package #:cl-xmpp-system)

(defsystem cl-xmpp
    :name "cl-xmpp"
    :author "Erik Enge"
    :licence "MIT"
    :description "Common Lisp XMPP client implementation"
    :depends-on (:usocket :cxml :ironclad)
    :components ((:file "package")
                 (:file "variable"
                        :depends-on ("package"))
                 (:file "utility"
                        :depends-on ("variable"))
		 (:file "result"
			:depends-on ("utility"))
                 (:file "cl-xmpp"
                        :depends-on ("result"))))

(defmethod perform ((operation test-op) (component (eql (find-system 'cl-xmpp))))
  (operate 'load-op 'cl-xmpp-test)
  (operate 'test-op 'cl-xmpp-test :force t))

