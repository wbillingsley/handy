package com.wbillingsley.handy.user

/**
 * The user's active sessions are kept, for resolving identity and in order to support
 * remote log-out.
 */
case class ActiveSession(

    /** Unique identifier, kept in the session cookie */
    key:String,

    /** IP address of the user, when this session was created */
    ip:String,

    /** When this session was created */
    since:Long = System.currentTimeMillis
)