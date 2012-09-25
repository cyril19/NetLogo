// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.app

import org.nlogo.agent.Observer
import org.nlogo.window.EditorAreaErrorLabel
import org.nlogo.workspace.AbstractWorkspace

import java.awt.{BorderLayout, Dimension, Graphics}
import java.awt.event.{ActionEvent, TextEvent, TextListener}
import java.awt.print.PageFormat
import javax.swing.{JButton, ImageIcon, AbstractAction, Action, ScrollPaneConstants, JScrollPane, BorderFactory, JPanel}
import org.nlogo.api.{ AgentKind, I18N }

class CodeTab(val workspace: AbstractWorkspace) extends JPanel
  with org.nlogo.window.ProceduresInterface
  with ProceduresMenuTarget
  with Events.SwitchedTabsEventHandler
  with org.nlogo.window.Events.CompiledEventHandler
  with org.nlogo.window.Events.ZoomedEventHandler
  with org.nlogo.swing.Printable {

  private val listener = new TextListener() {
    override def textValueChanged(e: TextEvent) {
      needsCompile()
      dirty()
    }
  }
  val text = new EditorFactory(workspace).newEditor(100, 100, true, listener, true)
  text.setBorder(BorderFactory.createEmptyBorder(4, 7, 4, 7))

  val errorLabel = new EditorAreaErrorLabel(text)
  val toolBar = getToolBar
  def compiler = workspace
  def program = workspace.world.program

  locally {
    setIndenter(false)
    setLayout(new BorderLayout)
    add(toolBar, BorderLayout.NORTH)
    val codePanel = new JPanel(new BorderLayout) {
      add(new JScrollPane(
        text,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED),
        BorderLayout.CENTER)
      add(errorLabel, BorderLayout.NORTH)
    }
    add(codePanel, BorderLayout.CENTER)
  }

  val compileAction: Action = new CompileAction

  private class CompileAction extends AbstractAction(I18N.gui.get("tabs.code.checkButton")) {
    putValue(Action.SMALL_ICON,
      new ImageIcon(classOf[CodeTab].getResource(
        "/images/check.gif")))
    def actionPerformed(e: ActionEvent) {
      new org.nlogo.window.Events.CompileAllEvent().raise(CodeTab.this)
    }
  }

  def getToolBar = new org.nlogo.swing.ToolBar() {
    override def addControls() {
      add(new JButton(FindDialog.FIND_ACTION))
      add(new JButton(compileAction))
      add(new org.nlogo.swing.ToolBar.Separator())
      add(new ProceduresMenu(CodeTab.this))
    }
  }

  def dirty() { new org.nlogo.window.Events.DirtyEvent().raise(this) }

  private def needsCompile() {
    _needsCompile = true
    compileAction.setEnabled(true)
  }

  // don't let the editor influence the preferred size,
  // since the editor tends to want to be huge - ST
  override def getPreferredSize: Dimension = toolBar.getPreferredSize

  def getIncludesTable: Map[String, String] = {
    val path = Option(workspace.getModelPath()).getOrElse{
      // we create an arbitrary model name for checking include paths when we don't have an actual
      // modelPath or directory
      try workspace.attachModelDir("foo.nlogo")
      catch {
        case ex: java.net.MalformedURLException =>
          // if we can't even figure out where we are, we certainly can't have includes
          return Map()
      }
    }
    workspace.compiler.findIncludes(path, getText, workspace.world.program.is3D)
  }

  override def kind = AgentKind.Observer

  protected var _needsCompile = false

  def handle(e: Events.SwitchedTabsEvent) {
    if(_needsCompile && e.oldTab == this)
      recompile()
  }

  private var originalFontSize = -1
  private var zoomFactor = 1.0
  def handle(e: org.nlogo.window.Events.ZoomedEvent) {
    if(zoomFactor != e.zoomFactor) {
      zoomFactor = e.zoomFactor
      if(originalFontSize == -1) originalFontSize = text.getFont.getSize
      text.setFont(text.getFont.deriveFont(StrictMath.ceil(originalFontSize * zoomFactor).toFloat))
      errorLabel.zoom(zoomFactor)
    }
  }

  // Error code

  def handle(e: org.nlogo.window.Events.CompiledEvent) {
    _needsCompile = false
    compileAction.setEnabled(e.error != null)
    if(e.sourceOwner == this) errorLabel.setError(e.error, headerSource.length)
  }

  def recompile() { new org.nlogo.window.Events.CompileAllEvent().raise(this) }

  override def requestFocus() { text.requestFocus() }

  def innerSource = text.getText
  def getText = text.getText  // for ProceduresMenuTarget
  def headerSource = ""
  def source = headerSource + innerSource

  def innerSource(s: String) {
    text.setText(s)
    text.setCaretPosition(0)
  }

  def select(startPos: Int, endPos: Int) { text.select(startPos, endPos) }

  def classDisplayName = "Code"

  /// printing

  // satisfy org.nlogo.swing.Printable
  @throws(classOf[java.io.IOException])
  def print(g: Graphics, pageFormat: PageFormat,pageIndex: Int, printer: org.nlogo.swing.PrinterManager): Int =
    printer.printText(g, pageFormat, pageIndex, text.getText)

  def setIndenter(isSmart: Boolean) {
    if(isSmart) text.setIndenter(new SmartIndenter(new EditorAreaWrapper(text), workspace))
    else text.setIndenter(new org.nlogo.editor.DumbIndenter(text))
  }
}
