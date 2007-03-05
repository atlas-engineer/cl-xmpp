;;;; $Id: cxml.lisp,v 1.10 2005/12/31 20:15:06 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/cxml.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

;;; Perform single-byte reads to avoid blocking on the socket.
;;; Also print all data read to *DEBUG-STREAM*.
;;;
;;; FIXME: Is this still needed, now that cxml supports :BUFFERING NIL?
;;; The debugging output could be done using a special gray stream that
;;; dribbles input, instead of a special xstream.  --david

(defstruct (slow-stream (:constructor make-slow-stream (target)))
  (target nil :type stream))

(defmethod runes::figure-encoding ((stream slow-stream))
  (runes::figure-encoding (slow-stream-target stream)))

(defmethod runes::read-octets (seq (stream slow-stream) start end)
  (when (< start end)
    (let ((byte (read-byte (slow-stream-target stream) nil)))
      (when byte
	(when *debug-stream*
	  ;; Original comment:
	  ;; I'd like to see what CXML is reading from the stream
	  ;; and this code helps us in that regard by printing it
	  ;; to the *debug-stream*
	  (write-char (code-char byte) *debug-stream*))
	(setf (elt seq start) byte)
	(incf start))))
  start)

