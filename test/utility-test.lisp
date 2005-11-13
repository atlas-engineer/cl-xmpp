;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :xmpp-test)

(deftest flatten.1 (xmpp::flatten '(1 2 3)) (1 2 3))
(deftest flatten.2 (xmpp::flatten '(1 (2 3) 4)) (1 2 3 4))

(deftest string-to-array.1 (xmpp::string-to-array "test") #(116 101 115 116))

(deftest digestify-string.1 (xmpp::digestify-string "test") "a94a8fe5ccb19ba61c4c0873d391e987982fbbd3")

