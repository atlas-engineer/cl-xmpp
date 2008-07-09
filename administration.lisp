;;;; $Id: administration.lisp,v 1.1 2007/12/17 09:16:24 kcrosbie Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/administration.lisp,v $

(in-package :cl-xmpp)

;;<iq from='bard@shakespeare.lit/globe'
;;    id='end-user-session-1'
;;    to='shakespeare.lit'
;;    type='get'
;;    xml:lang='en'>
;;  <command xmlns='http://jabber.org/protocol/commands' 
;;           action='execute'
;;           node='http://jabber.org/protocol/admin#end-user-session'/>
;;</iq>

;;<iq type="set" to="shakespeare.lit" id="ae23a" >
;;  <command xmlns="http://jabber.org/protocol/commands"
;;            node="http://jabber.org/protocol/admin#end-user-session"
;;       sessionid="2007-12-04T11:56:33.920539Z" >
;;    <x xmlns="jabber:x:data" type="submit" >
;;      <field type="hidden" var="FORM_TYPE" >
;;        <value>http://jabber.org/protocol/admin</value>
;;      </field>
;;      <field type="jid-single" var="accountjid" >
;;        <value>bard@shakespeare.lit</value>
;;      </field>
;;    </x>
;;  </command>
;;</iq>

(defmethod end-user-session ((connection connection) &key to server)
  (with-xml-output (connection)
    (with-iq-command
        (connection :xmlns  "http://jabber.org/protocol/commands"
                    :node   "http://jabber.org/protocol/admin#end-user-session"
                    :to     server
                    :type   "set")
      (cxml:with-element "x"
        (cxml:attribute "xmlns" "jabber:x:data")
        (cxml:attribute "type"  "submit")
        (with-form-field "hidden" "FORM_TYPE" "http://jabber.org/protocol/admin")
        (with-form-field "jid-single" "accountjid" to)))))

;;<iq type="get"
;;      to="shakespeare.lit"
;;      id="ab48a" >
;;  <query xmlns="http://jabber.org/protocol/disco#items"
;;          node="online users" />
;;</iq>

(defmethod get-online-users ((connection connection) &key server)
  (with-xml-output (connection)
    (with-iq-query (connection :type "get" :to server
                               :xmlns "http://jabber.org/protocol/disco#items"
                               :node  "online users"))))
      