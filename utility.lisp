;;;; $Id: utility.lisp,v 1.5 2005/10/31 21:07:15 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/utility.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

(defmacro fmt (string &rest args)
  `(format nil ,string ,@args))

(defun flatten (list)
  (cond
   ((typep list 'atom) list)
   ((typep (car list) 'atom) (cons (car list)
				   (flatten (cdr list))))
   ((typep (car list) 'list) (flatten (append (car list) (cdr list))))))

(defun string-to-array (string &rest args)
  (let ((array (apply #'make-array (length string) args)))
    (dotimes (position (length string))
      (setf (aref array position) (char-code (aref string position))))
    array))

(defun digestify-string (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha1 (ironclad:ascii-string-to-byte-array string))))

(defun make-digest-password (stream-id password)
  (string-downcase (digestify-string (fmt "~a~a" stream-id password))))

(defun default-stanza-callback (stanza connection &key dom-repr)
  (let ((result (parse-result connection stanza)))
    (if dom-repr
	(handle connection result)
      (handle connection (dom-to-event connection result)))))


