// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.api

trait CompilerServices {
  def isConstant(s: String): Boolean
  @throws(classOf[CompilerException])
  def readFromString(s: String): AnyRef
  def autoConvert(source: String, subprogram: Boolean, reporter: Boolean, modelVersion: String): String
  @throws(classOf[CompilerException])
  def readNumberFromString(source: String): AnyRef
  @throws(classOf[CompilerException])
  def checkReporterSyntax(source: String)
  @throws(classOf[CompilerException])
  def checkCommandSyntax(source: String)
  def isReporter(s: String): Boolean
  def isValidIdentifier(s: String): Boolean
  def tokenizeForColorization(source: String): Seq[Token]
  def getTokenAtPosition(source: String, position: Int): Token
  def findProcedurePositions(source: String): Map[String, (String, Int, Int, Int)]
}
