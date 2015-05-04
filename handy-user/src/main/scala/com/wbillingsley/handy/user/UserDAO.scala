package com.wbillingsley.handy.user

import com.wbillingsley.handy.Ref
import com.wbillingsley.handy.appbase.{ActiveSession, PasswordLogin}
import org.mindrot.jbcrypt.BCrypt

/**
 * Trait to be implemented by DAOs for accessing users from the database
 * @tparam U
 * @tparam I
 */
trait UserDAO[U, I] {

  /**
   * Get the user by their current session identifier
   */
  def bySessionKey(sessionKey:String):Ref[U]

  /**
   * By a social login, or other identifier in another service
   */
  def byIdentity(identity:I):Ref[U]

  /**
   * Push a new session into the user's sessions.
   * After this call, a request to <code>bySessionKey</code> for the session key should retrieve this user.
   */
  def addSession(user:Ref[U], session:ActiveSession):Ref[U]

  /**
   * Remove a session from the user's sessions.
   * After this call, a request to <code>bySessionKey</code> for the session key should not retrieve this user.
   */
  def removeSession(user:Ref[U], sessionKey:String):Ref[U]

  /**
   * Add a social identity to the user's identities
   * After this call, a request to <code>byIdentity</code> for this identity should not retrieve this user.
   */
  def addIdentity(user:Ref[U], identity:I):Ref[U]

  /**
   * Remove a social identity from the user's sessions.
   * After this call, a request to <code>byIdentity</code> for this identity should should not retrieve this user.
   */
  def removeIdentity(user:Ref[U], identity:I):Ref[U]

  /**
   * Generate a salt and hash for this password
   */
  def hash(password: String) = BCrypt.hashpw(password, BCrypt.gensalt())

  /**
   * Check if the given password matches the given password login
   */
  def checkPassword(pwlogin:PasswordLogin, pw:String) = pwlogin.pwhash match {
    case Some(hashed) => BCrypt.checkpw(pw, hashed)
    case None => false
  }

}
