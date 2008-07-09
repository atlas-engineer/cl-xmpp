;;;; $Id: variable.lisp,v 1.2 2007/12/17 09:13:13 kcrosbie Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/variable.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

(defvar *debug-stream* *debug-io*
  "A character stream, or nil")

(defvar *default-port* 5222)
(defvar *default-hostname* "localhost")

(defvar *legacy-errors*
    '((:undefined-condition     :any    500)
    (:forbidden :auth 403)
      (:bad-auth                :auth   401)
    (:not-authorized :auth 401)
    (:payment-required :auth 402)
    (:registration-required :auth 407)
      (:subscription-required   :auth   407)
      (:redirect                :modify 302)
      (:bad-request             :modify 400)
      (:jid-malformed           :modify 400)
      (:not-acceptable          :modify 406)
      (:gone                    :modify 302)
      (:conflict                :cancel 409)
      (:feature-not-implemented :cancel 501)
      (:item-not-found          :cancel 404)
      (:not-allowed             :cancel 405)
    (:remote-server-not-found :cancel 404)
      (:service-unavailable     :cancel 503)
      (:internal-server-error   :wait   500)
      (:recipient-unavailable   :wait   404)
    (:remote-server-timeout :wait 504)
    (:resource-constraint :wait 500)
    (:unexpected-request :wait 400)))


(defvar *errors*
    '((:bad-format)
      (:bad-namespace-prefix)
      (:conflict)
      (:connection-timeout)
      (:host-gone)
      (:host-unknown)
      (:improper-addressing)
      (:internal-server-error)
      (:invalid-from)
      (:invalid-id)
      (:invalid-namespace)
      (:invalid-xml)
      (:not-authorized)
      (:policy-violation)
      (:remote-connection-failed)
      (:resource-constraint)
      (:restricted-xml)
      (:see-other-host)
      (:system-shutdown)
      (:undefined-condition)
      (:unsupported-encoding)
      (:unsupported-stanza-type)
      (:unsupported-version)
      (:xml-not-well-formed)))


(defvar *auth-methods* nil
  "Alist of method name to operator.

Operators must accept the following operands:

   connection username password resource")
