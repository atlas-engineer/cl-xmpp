;;;; -*- mode: lisp -*-
;;;; $Id: cl-xmpp-sasl.asd,v 1.3 2005/11/13 02:36:10 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/cl-xmpp-sasl.asd,v $

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-xmpp-sasl-system
    (:use #:cl #:asdf))

(in-package #:cl-xmpp-sasl-system)

(defsystem cl-xmpp-sasl
    :name "cl-xmpp-sasl"
    :author "Erik Enge"
    :licence "MIT"
    :description "Common Lisp XMPP client implementation with SASL support"
    :depends-on (:cl-xmpp :cl-base64 :cl-sasl)
    :components ((:file "cl-xmpp-sasl")))

