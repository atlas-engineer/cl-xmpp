;;;; $Id$
;;;; $Source$

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
     ;; variables
     :*default-port :*default-hostname*)))
