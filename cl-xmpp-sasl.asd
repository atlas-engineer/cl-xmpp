;;;; -*- mode: lisp -*-
;;;; $Id: cl-xmpp-sasl.asd,v 1.1 2005/11/11 17:21:56 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/cl-xmpp-sasl.asd,v $

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-xmpp-sasl-system
    (:use #:cl #:asdf))

(in-package #:cl-xmpp-sasl-system)

(defsystem cl-xmpp-sasl
    :name "cl-xmpp-sasl"
    :author "Erik Enge"
    :version "0.0.1"
    :licence "MIT"
    :description "Common Lisp XMPP client implementation with SASL support"
    :depends-on (#+sbcl :sb-bsd-sockets :trivial-sockets :cxml :ironclad :sasl :cl-base64)
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
                        :depends-on ("cl-xmpp"))))

