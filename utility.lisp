;;;; $Id: utility.lisp,v 1.4 2005/10/31 17:02:04 eenge Exp $
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

(defun hex-array-to-ascii-string (array)
  (let ((string (make-string 0)))
    (dotimes (position (length array))
      (let ((element (aref array position))
	    (*print-base* 16))
	(setq string (fmt "~a~a" string element)))) ; probably inefficient
    string))

;;; borrowed from ironclad, so Copyright (C) 2004 Nathan Froyd
(defun ascii-string-to-byte-array (string)
  (let ((vec (make-array (length string) :element-type '(unsigned-byte 8))))
    (dotimes (i (length string) vec)
      (let ((byte (char-code (char string i))))
        (assert (< byte 256))
        (setf (aref vec i) byte)))))

(defun digestify-string (string)
  (hex-array-to-ascii-string
   (ironclad:digest-sequence
    :sha1 (ascii-string-to-byte-array string))))

(defun make-digest-password (stream-id password)
  (string-downcase (digestify-string (fmt "~a~a" stream-id password))))

(defun default-stanza-callback (stanza connection &key dom-repr)
  (let ((result (parse-result connection stanza)))
    (if dom-repr
	(handle connection result)
      (handle connection (dom-to-event connection result)))))


