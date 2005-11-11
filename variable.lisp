;;;; $Id: variable.lisp,v 1.2 2005/10/28 21:04:12 eenge Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/variable.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :xmpp)

(defvar *debug-stream* *debug-io*
  "A character stream, or nil")

(defvar *default-port* 5222)
(defvar *default-hostname* "localhost")

(defvar *errors*
  '((:bad-request :modiy 400)
    (:conflict :cancel 409)
    (:feature-not-implemented :cancel 501)
    (:forbidden :auth 403)
    (:gone :modify 302)
    (:internal-server-error :wait 500)
    (:item-not-found :cancel 404)
    (:jid-malformed :modify 400)
    (:not-acceptable :modify 406)
    (:not-allowed :cancel 405)
    (:not-authorized :auth 401)
    (:payment-required :auth 402)
    (:recipient-unavailable :wait 404)
    (:redirect :modify 302)
    (:registration-required :auth 407)
    (:remote-server-not-found :cancel 404)
    (:remote-server-timeout :wait 504)
    (:resource-constraint :wait 500)
    (:service-unavailable :cancel 503)
    (:subscription-required :auth 407)
    (:undefined-condition :any 500)
    (:unexpected-request :wait 400)))

