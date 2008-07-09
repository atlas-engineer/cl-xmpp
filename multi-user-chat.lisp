;;;; $Id: multi-user-chat.lisp,v 1.6 2007/12/17 09:17:22 kcrosbie Exp $
;;;; $Source: /project/cl-xmpp/cvsroot/cl-xmpp/multi-user-chat.lisp,v $

(in-package :cl-xmpp)

;;
;; Multi User Chat
;;
;;<presence
;;    from='crone1@shakespeare.lit/desktop'
;;    to='darkcave@macbeth.shakespeare.lit/firstwitch'>
;;  <x xmlns='http://jabber.org/protocol/muc'/>
;;</presence>

(defmethod create-chatroom ((connection connection) &key from room priority)
  (with-xml-output (connection)
    (cxml:with-element "presence"
      (cxml:attribute "to"   room)
      (cxml:attribute "from" from)
      (when priority
        (cxml:with-element "priority"
          (cxml:text "0")))
      (cxml:with-element "x"
        (cxml:attribute "xmlns" "http://jabber.org/protocol/muc")))))

;;<presence
;;    from='hag66@shakespeare.lit/pda'
;;    to='darkcave@macbeth.shakespeare.lit/thirdwitch'/>

(defmethod join-chatroom ((connection connection) &key from room password)
  (with-xml-output (connection)
    (cxml:with-element "presence"
      (cxml:attribute "to"   room)
      (cxml:attribute "from" from)
      (when password
        (cxml:with-element "x"
          (cxml:attribute "xmlns" "http://jabber.org/protocol/muc")
          (cxml:with-element "password"
            (cxml:text password)))))))

;;<presence
;;    from='hag66@shakespeare.lit/pda'
;;    to='darkcave@macbeth.shakespeare.lit/thirdwitch'
;;    type='unavailable'/>

(defmethod leave-chatroom ((connection connection) &key from room)
  (with-xml-output (connection)
    (cxml:with-element "presence"
      (cxml:attribute "to"   room)
      (cxml:attribute "from" from)
      (cxml:attribute "type" "unavailable"))))
  
;;<message
;;    from='crone1@shakespeare.lit/desktop'
;;    to='darkcave@macbeth.shakespeare.lit'>
;;  <x xmlns='http://jabber.org/protocol/muc#user'>
;;    <invite to='hecate@shakespeare.lit'>
;;      <reason>
;;        Hey Hecate, this is the place for all good witches!
;;      </reason>
;;    </invite>
;;  </x>
;;</message>

(defmethod invite-to-chatroom ((connection connection)
                               &key from room to reason)
  (with-xml-output (connection)
    (cxml:with-element "message"
      (cxml:attribute "to"   room)
      (cxml:attribute "from" from)
      (cxml:with-element "x"
        (cxml:attribute "xmlns" "http://jabber.org/protocol/muc#user")
        (cxml:with-element "invite"
          (cxml:attribute "to" to)
          (cxml:with-element "reason"
            (cxml:text reason)))))))


;;<iq from='fluellen@shakespeare.lit/pda'
;;    id='kick1'
;;    to='harfleur@henryv.shakespeare.lit'
;;    type='set'>
;;  <query xmlns=''>
;;    <item nick='pistol' role='none'>
;;      <reason>Avaunt, you cullion!</reason>
;;    </item>
;;  </query>
;;</iq>

(defmethod kick-from-chatroom ((connection connection)
                               &key to room reason)
  (with-xml-output (connection)
    (with-iq-query (connection :to room :type "set"
                               :xmlns "http://jabber.org/protocol/muc#admin")
      (cxml:with-element "item"
        (cxml:attribute "nick" to)
        (cxml:attribute "role" "none")
        (cxml:with-element "reason"
          (cxml:text reason))))))
                               

;;<iq from='crone1@shakespeare.lit/desktop'
;;    id='member1'
;;    to='darkcave@macbeth.shakespeare.lit'
;;    type='set'>
;;  <query xmlns='http://jabber.org/protocol/muc#admin'>
;;    <item affiliation='member'
;;          jid='hag66@shakespeare.lit'/>
;;  </query>
;;</iq>

(defmethod set-room-affiliation ((connection connection)
                                   &key room to affiliation)
  (with-xml-output (connection)
    (with-iq-query (connection :to room :type "set"
                               :xmlns "http://jabber.org/protocol/muc#admin")
      (cxml:with-element "item"
        (cxml:attribute "affiliation" affiliation)
        (cxml:attribute "jid"         to)))))


(defmethod grant-room-membership ((connection connection) &key room to)
  (set-room-affiliation connection :room room :to to :affiliation "member"))


(defmethod revoke-room-membership ((connection connection) &key room to)
  (set-room-affiliation connection :room room :to to :affiliation "none"))


;;<message
;;    from='hag66@shakespeare.lit/pda'
;;    to='darkcave@macbeth.shakespeare.lit'
;;    type='groupchat'>
;;  <body>Harpier cries: 'tis time, 'tis time.</body>
;;</message>

(defmethod broadcast-room ((connection connection) &key from room message)
  (with-xml-output (connection)
    (cxml:with-element "message"
      (cxml:attribute "from" from)
      (cxml:attribute "to"   room)
      (cxml:attribute "type" "groupchat")
      (cxml:with-element "body"
        (cxml:text message)))))

(defmacro with-form-field (type var &optional (value ""))
  `(cxml:with-element "field"
     ,(when type
        `(cxml:attribute "type" ,type))
     ,(when var
        `(cxml:attribute "var"  ,var))
     (cxml:with-element "value"
       (cxml:text ,(or value "")))))

;; For now these are just the settings that I want to use.   It would be easy
;; to change this method so that it takes a list of arguments and looks up
;; nodes/data-types in some structure.
(defmethod default-room-config ((connection connection) &key room)
  (with-xml-output (connection)
    (with-iq-query (connection :type "set" :to room
                               :xmlns "http://jabber.org/protocol/muc#owner")
      (cxml:with-element "x"
        (cxml:attribute "xmlns" "jabber:x:data")
        (cxml:attribute "type" "submit")
        (with-form-field "hidden" "FORM_TYPE" 
                         "http://jabber.org/protocol/muc#roomconfig")
        (with-form-field "text-single" "muc#roomconfig_roomname")
        (with-form-field "boolean" "muc#roomconfig_persistentroom" "0")
        (with-form-field "boolean" "muc#roomconfig_publicroom" "0")
        (with-form-field "boolean" "public_list" "0")
        (with-form-field "boolean" "muc#roomconfig_passwordprotectedroom" "0")
        (with-form-field "text-private" "muc#roomconfig_roomsecret")
        (with-form-field "list-single" "muc#roomconfig_whois" "moderators")
        (with-form-field "boolean" "muc#roomconfig_membersonly" "1")
        (with-form-field "boolean" "muc#roomconfig_moderatedroom" "0")
        (with-form-field "boolean" "members_by_default" "0")
        (with-form-field "boolean" "muc#roomconfig_changesubject" "0")
        (with-form-field "boolean" "allow_private_messages" "0")
        (with-form-field "boolean" "allow_query_users" "0")
        (with-form-field "boolean" "muc#roomconfig_allowinvites" "0")))))


;;<message
;;    from='wiccarocks@shakespeare.lit/laptop'
;;    to='darkcave@macbeth.shakespeare.lit'
;;    type='groupchat'>
;;  <subject>Fire Burn and Cauldron Bubble!</subject>
;;</message>

(defmethod set-chatroom-subject ((connection connection)
                                 &key from room subject)
  (with-xml-output (connection)
    (cxml:with-element "message"
      (cxml:attribute "from" from)
      (cxml:attribute "to"   room)
      (cxml:attribute "type" "groupchat")
      (cxml:with-element "subject"
        (cxml:text subject)))))


;;<iq from='crone1@shakespeare.lit/desktop'
;;    id='begone'
;;    to='heath@macbeth.shakespeare.lit'
;;    type='set'>
;;  <query xmlns='http://jabber.org/protocol/muc#owner'>
;;    <destroy jid='darkcave@macbeth.shakespeare.lit'>
;;      <reason>Macbeth doth come.</reason>
;;    </destroy>
;;  </query>
;;</iq>

(defmethod destroy-chatroom ((connection connection) &key room reason)
  (with-xml-output (connection)
    (with-iq-query (connection :type "set" :to room
                               :xmlns "http://jabber.org/protocol/muc#owner")
      (cxml:with-element "destroy"
        (cxml:attribute "jid" room)
        (when reason
          (cxml:with-element "reason"
            (cxml:text reason)))))))


;;<iq from='crone1@shakespeare.lit/desktop'
;;    id='voice2'
;;    to='darkcave@macbeth.shakespeare.lit'
;;    type='set'>
;;  <query xmlns='http://jabber.org/protocol/muc#admin'>
;;    <item nick='thirdwitch'
;;          role='visitor'/>
;;  </query>
;;</iq>

(defmethod revoke-voice ((connection connection) &key room nickname)
  (with-xml-output (connection)
    (with-iq-query (connection :type "set" :to room
                               :xmlns "http://jabber.org/protocol/muc#admin")
      (cxml:with-element "item"
        (cxml:attribute "nick" nickname)
        (cxml:attribute "role" "visitor")))))

