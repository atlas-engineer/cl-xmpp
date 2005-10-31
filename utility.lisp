;;;; $Id: utility.lisp,v 1.3 2005/10/29 03:58:04 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/utility.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

(defun flatten (list)
  (cond
   ((typep list 'atom) list)
   ((typep (car list) 'atom) (cons (car list)
				   (flatten (cdr list))))
   ((typep (car list) 'list) (flatten (append (car list) (cdr list))))))

(defun string-to-array (string)
  (let ((array (make-array (length string))))
    (dotimes (position (length string))
      (setf (aref array position) (char-code (aref string position))))
    array))

(defun default-stanza-callback (stanza connection &key dom-repr)
  (let ((result (parse-result stanza)))
    (if dom-repr
	(handle connection result)
      (handle connection (dom-to-event result)))))

;; um, refactor?
(defun default-init-callback (stanza connection &key dom-repr)
  (let ((result (parse-result stanza)))
    (if dom-repr
	(handle connection result)
      (handle connection (dom-to-event result)))))

(defmacro fmt (string &rest args)
  `(format nil ,string ,@args))

