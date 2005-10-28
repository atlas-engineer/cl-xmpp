;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

(defun add-stream-namespace-binding ()
  (push '(#"stream" "http://etherx.jabber.org/streams")
	cxml::*default-namespace-bindings*))
;(add-stream-namespace-binding)

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

(defun default-stanza-callback (stanza)
  (format t "default-stanza-callback:~a~%" stanza))

(defun default-init-callback (stanza)
  (format t "default-init-callback:~a~%" stanza))

(defmacro fmt (string &rest args)
  `(format nil ,string ,@args))

