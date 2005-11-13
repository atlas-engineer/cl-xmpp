;;;; -*- mode: lisp -*-
;;;; $Id: cl-xmpp-test.asd,v 1.1 2005/11/13 02:36:11 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/test/cl-xmpp-test.asd,v $

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-xmpp-test-system
    (:use #:cl #:asdf))

(in-package #:cl-xmpp-test-system)

(defsystem cl-xmpp-test
    :name "cl-xmpp-test"
    :author "Erik Enge"
    :licence "MIT"
    :description "Common Lisp XMPP client implementation"
    :depends-on (:cl-xmpp :rt)
    :components ((:file "package")
                 (:file "utility-test"
                  :depends-on ("package"))
                 (:file "result-test"
                  :depends-on ("package"))))   
                 (:file "cl-xmpp-test"
                  :depends-on ("package"))))   

