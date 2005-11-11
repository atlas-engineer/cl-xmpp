;;;; -*- mode: lisp -*-
;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-xmpp-tls-system
    (:use #:cl #:asdf))

(in-package #:cl-xmpp-tls-system)

(defsystem cl-xmpp-tls
    :name "cl-xmpp-tls"
    :author "Erik Enge"
    :version "0.0.1"
    :licence "MIT"
    :description "Common Lisp XMPP client implementation with TLS+SASL support"
    :depends-on (#+sbcl :sb-bsd-sockets :trivial-sockets :cxml :ironclad :cl+ssl :sasl)
    :components ((:file "package")
                 (:file "variable"
                        :depends-on ("package"))
                 (:file "utility"
                        :depends-on ("variable"))
		 (:file "cxml"
			:depends-on ("utility"))
		 (:file "result"
			:depends-on ("cxml"))
                 (:file "cl-xmpp"
                        :depends-on ("result"))
		 (:file "cl-xmpp-sasl"
			:depends-on ("cl-xmpp"))
                 (:file "cl-xmpp-tls"
                        :depends-on ("cl-xmpp-sasl"))))

