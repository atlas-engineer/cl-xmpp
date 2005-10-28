;;;; $Id: package.lisp,v 1.1.1.1 2005/10/28 13:16:02 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/package.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :cl-xmpp
      (:use :cl)
    (:nicknames :xmpp)
    (:export
     ;; connection-related
     :connect :disconnect :socket :stream- :hostname :port :connectedp
     :receive-stanza-loop :begin-xml-stream :end-xml-stream :with-iq
     :with-iq-query
     ;; xmpp commands
     :registration-requirements :register
     :auth-requirements :auth
     :presence :message :bind
     :request-subscription :approve-subscription
     :deny/cancel-subscription :unsubscribe
     :get-roster :roster-add :roster-remove
     :get-privacy-lists :get-privacy-list
     ;; event interface
     :event
     :message :to :from :body
     :handle
     ;; variables
     :*default-port :*default-hostname*)))
