;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :xmpp-test)

(defvar *empty-element* #.(make-instance 'xmpp:xml-element :name :test))

(deftest get-element.1 (xmpp:get-element *empty-element* :x) nil)