;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :cl-xmpp-test
      (:use :cl :rtest)
      (:nicknames :xmpp-test)
    (:export :do-tests)))
