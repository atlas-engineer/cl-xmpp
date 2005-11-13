;;;; $Id: result-test.lisp,v 1.1 2005/11/13 02:36:11 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/test/result-test.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp-test)

(defvar *empty-element* (make-instance 'xmpp:xml-element :name :test))
(defvar *sub-element* (make-instance 'xmpp:xml-element :name :subtest))
(defvar *element+subelement* 
  (make-instance 'xmpp:xml-element 
                 :name :test
                 :elements (list *sub-element*)))

(deftest get-element.1 (xmpp:get-element *empty-element* :x) nil)
(deftest get-element.2 (xmpp:get-element *element+subelement* :x) nil)
(deftest get-element.3 (xmpp:get-element *element+subelement* :subtest) #.*sub-element*)
