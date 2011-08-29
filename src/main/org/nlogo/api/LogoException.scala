package org.nlogo.api

/**
 * A runtime error that has occurred in NetLogo code.
 * 
 * As with any Exception, use the getMessage() method to get the error message.
 * A LogoExceptions is expected to have a nice end-user-understandable message.
 *
 * LogoException is abstract because engine code is supposed to throw
 * a concrete subclass like EngineException.
 */

abstract class LogoException(message: String, cause: Throwable) extends Exception(message, cause) {
  def this(message: String) = this(message, null)
}