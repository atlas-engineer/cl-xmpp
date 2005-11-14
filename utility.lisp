;;;; $Id: utility.lisp,v 1.8 2005/11/14 15:14:07 eenge Exp $
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

(defun list-auth-method-names ()
  (mapcar #'car *auth-methods*))

(defun get-auth-method (name)
  (let ((auth-method (second (assoc name *auth-methods*))))
    (if auth-method
	(return-from get-auth-method auth-method)
      (error "Unknown mechanism name: ~s.  Please choose between: ~s."
	     name (list-auth-method-names)))))

(defun add-auth-method (name operator)
  (push (list name operator) *auth-methods*))

(defun ensure-keyword (thing)
  (cond
   ((typep thing 'string) (intern thing :keyword))
   ((typep thing 'array) (ensure-keyword (map 'string #'code-char thing)))
   (t (error "Don't know how to make keyword out of: ~a (type: ~a)" thing (type-of thing)))))