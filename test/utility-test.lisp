;;;; $Id: utility-test.lisp,v 1.1 2005/11/13 02:36:11 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/test/utility-test.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp-test)

(deftest flatten.1 (xmpp::flatten '(1 2 3)) (1 2 3))
(deftest flatten.2 (xmpp::flatten '(1 (2 3) 4)) (1 2 3 4))

(deftest digestify-string.1 (xmpp::digestify-string "test") "a94a8fe5ccb19ba61c4c0873d391e987982fbbd3")

