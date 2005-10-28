;;;; -*- mode: lisp -*-
;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-xmpp-system
    (:use #:cl #:asdf))

(in-package #:cl-xmpp-system)

(defsystem cl-xmpp
    :name "cl-xmpp"
    :author "Erik Enge"
    :version "0.0.1"
    :licence "MIT"
    :description "Common Lisp XMPP client implementation"
    :depends-on (:sb-bsd-sockets :cxml)
    :components ((:file "package")
                 (:file "variable"
                        :depends-on ("package"))
                 (:file "utility"
                        :depends-on ("variable"))
		 (:file "cxml"
			:depends-on ("utility"))
		 (:file "result-parsing"
			:depends-on ("cxml"))
                 (:file "cl-xmpp"
                        :depends-on ("result-parsing"))))

