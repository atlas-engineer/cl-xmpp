;;;; -*- mode: lisp -*-
;;;; $Id: cl-xmpp.asd,v 1.5 2005/11/11 17:21:56 eenge Exp $
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
    :depends-on (#+sbcl :sb-bsd-sockets :trivial-sockets :cxml :ironclad)
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
                        :depends-on ("result"))))

