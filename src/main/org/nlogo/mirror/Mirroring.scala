package org.nlogo.mirror

import scala.collection.JavaConverters.asScalaSetConverter
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import org.nlogo.api.AgentVariableNumbers._
import org.nlogo.api
import org.nlogo.plot
import org.nlogo.workspace.AbstractWorkspaceScala

sealed abstract class Kind
case object Patch extends Kind
case object Turtle extends Kind
case object Link extends Kind
case object World extends Kind
case object Plot extends Kind
case object PlotPen extends Kind

case class AgentKey(kind: Kind, id: Long)

case class Birth(agent: AgentKey, values: Seq[AnyRef])
case class Death(agent: AgentKey)
case class Change(variable: Int, value: AnyRef)

case class Update(deaths: Seq[Death], births: Seq[Birth], changes: Map[AgentKey, Seq[Change]])

object Mirroring {

  trait IsMirrorable[T] {
    def getVariable(obj: T, index: Int): AnyRef
    def agentKey(obj: T) = AgentKey(kind, id(obj))
    def nbVariables: Int
    def kind: Kind
    def id(obj: T): Long
  }

  class Mirrorable[+T](obj: T, m: IsMirrorable[T]) {
    def nbVariables = m.nbVariables
    def getVariable(index: Int) = m.getVariable(obj, index)
    def agentKey = m.agentKey(obj)
    def kind = m.kind
  }

  implicit def toMirrorable[T](x: T)(implicit m: IsMirrorable[T]) = new Mirrorable(x, m)

  trait AgentIsMirrorable[T <: api.Agent] extends IsMirrorable[T] {
    def id(obj: T) = obj.id
    val implicitVariables: Array[String]
    val variableOverrides = Map[Int, T => AnyRef]()
    val extraVariables = Seq[T => AnyRef]()
    lazy val nbImplicitVariables = implicitVariables.length
    override lazy val nbVariables = nbImplicitVariables + extraVariables.size
    lazy val extraVariablesIndices = nbImplicitVariables until nbVariables
    override def getVariable(agent: T, index: Int) =
      variableOverrides
        .get(index).map(_.apply(agent))
        .getOrElse(
          if (extraVariablesIndices contains index)
            extraVariables(index - nbImplicitVariables)(agent)
          else agent.getVariable(index))
  }

  implicit object TurtleIsMirrorable extends AgentIsMirrorable[api.Turtle] {
    override val implicitVariables = api.AgentVariables.getImplicitTurtleVariables
    override val variableOverrides = Map[Int, api.Turtle => AnyRef](
      VAR_BREED -> { _.getBreed.printName })
    override val extraVariables = Seq[api.Turtle => AnyRef](
      { _.lineThickness: java.lang.Double })
    val Seq(tvLineThickness) = extraVariablesIndices
    override val kind = Turtle
  }

  implicit object PatchIsMirrorable extends AgentIsMirrorable[api.Patch] {
    override val variableOverrides = Map[Int, api.Patch => AnyRef](
      VAR_PXCOR -> { _.pxcor: java.lang.Integer },
      VAR_PYCOR -> { _.pycor: java.lang.Integer })
    override val implicitVariables = api.AgentVariables.getImplicitPatchVariables
    override val kind = Patch
  }

  implicit object LinkIsMirrorable extends AgentIsMirrorable[api.Link] {
    override val variableOverrides = Map[Int, api.Link => AnyRef](
      VAR_END1 -> { _.end1.id: java.lang.Long },
      VAR_END2 -> { _.end2.id: java.lang.Long },
      VAR_LBREED -> { _.getBreed.printName })
    override val implicitVariables = api.AgentVariables.getImplicitLinkVariables
    override val kind = Link
  }

  trait OtherIsMirrorable[T] extends IsMirrorable[T] {
    val variableGetters: Seq[T => AnyRef]
    override def getVariable(obj: T, index: Int) = variableGetters(index)(obj)
    override lazy val nbVariables = variableGetters.size
  }

  implicit object WorldIsMirrorable extends OtherIsMirrorable[api.World] {
    override val kind = World
    override def id(obj: api.World) = 0L // dummy id for the one and only world
    override val variableGetters = Seq[api.World => AnyRef](
      _.patchesWithLabels: java.lang.Integer,
      _.turtleShapeList, // probably not good enough to just pass the shapelists like that...
      _.linkShapeList,
      _.patchSize: java.lang.Double,
      _.worldWidth: java.lang.Integer,
      _.worldHeight: java.lang.Integer,
      _.minPxcor: java.lang.Integer,
      _.minPycor: java.lang.Integer,
      _.maxPxcor: java.lang.Integer,
      _.maxPycor: java.lang.Integer,
      _.wrappingAllowedInX: java.lang.Boolean,
      _.wrappingAllowedInY: java.lang.Boolean,
      _.patchesAllBlack: java.lang.Boolean,
      _.program.breeds.keySet.asScala.toSeq,
      _.program.linkBreeds.keySet.asScala.toSeq,
      { w =>
        if (w.trailDrawer.isDirty) {
          val outputStream = new java.io.ByteArrayOutputStream
          val img = w.trailDrawer.getDrawing.asInstanceOf[java.awt.image.BufferedImage]
          javax.imageio.ImageIO.write(img, "png", outputStream)
          Some(outputStream.toByteArray())
        } else None
      })
    object variableIndices {
      val Seq( // init vals for indices by pattern matching over range of getters
        wvPatchesWithLabels,
        wvTurtleShapeList,
        wvlinkShapeList,
        wvPatchSize,
        wvWorldWidth,
        wvWorldHeight,
        wvMinPxcor,
        wvMinPycor,
        wvMaxPxcor,
        wvMaxPycor,
        wvWrappingAllowedInX,
        wvWrappingAllowedInY,
        wvPatchesAllBlack,
        wvTurtleBreeds,
        wvLinkBreeds,
        wvTrailDrawing
        ) = 0 until variableGetters.size
    }
  }

  class RichPlot(val p: plot.Plot, val ws: AbstractWorkspaceScala) {
    val id: Int = ws.plotManager.plots.indexOf(p)
  }
  implicit object RichPlotIsMirrorable extends OtherIsMirrorable[RichPlot] {
    override val kind = Plot
    override def id(p: RichPlot) = p.id
    override val variableGetters = Seq[RichPlot => AnyRef](
      _.p.xMin: java.lang.Double,
      _.p.xMax: java.lang.Double,
      _.p.yMin: java.lang.Double,
      _.p.yMax: java.lang.Double,
      _.p.legendIsOpen: java.lang.Boolean)
    object variableIndices {
      val Seq( // init vals for indices by pattern matching over range of getters
        pvXMin,
        pvXMax,
        pvYMin,
        pvYMax,
        pvLegendIsOpen
        ) = 0 until variableGetters.size
    }
  }

  class RichPlotPen(val pen: plot.PlotPen, val ws: AbstractWorkspaceScala) {
    val id = {
      // we combine the plot id and the pen id (which are both 
      // originally Ints) into a single Long:
      val plotId: Long = ws.plotManager.plots.indexOf(pen.plot)
      val penId: Long = pen.plot.pens.indexOf(pen)
      (plotId << 32) | penId
    }
  }
  implicit object RichPlotPenIsMirrorable extends OtherIsMirrorable[RichPlotPen] {
    override val kind = PlotPen
    override def id(richPen: RichPlotPen) = richPen.id
    override val variableGetters = Seq[RichPlotPen => AnyRef](
      _.pen.name,
      _.pen.isDown: java.lang.Boolean,
      _.pen.mode: java.lang.Integer,
      _.pen.interval: java.lang.Double,
      rp => org.nlogo.api.Color.argbToColor(rp.pen.color),
      _.pen.x: java.lang.Double,
      _.pen.points.toList)
    object variableIndices {
      val Seq( // init vals for indices by pattern matching over range of getters
        ppvName,
        ppvIsDown,
        ppvMode,
        ppvColor,
        ppvX,
        ppvPoints
        ) = 0 until variableGetters.size
    }
  }

  private def allMirrorables(workspace: AbstractWorkspaceScala) = {
    def mirrorables[A: IsMirrorable](agentSet: api.AgentSet) = {
      import collection.JavaConverters._
      agentSet.agents.asScala.map(_.asInstanceOf[A]).map(toMirrorable[A](_))
    }
    val world: api.World = workspace.world
    val turtles = mirrorables[api.Turtle](world.turtles)
    val patches = mirrorables[api.Patch](world.patches)
    val links = mirrorables[api.Link](world.links)
    val worlds = Iterable(toMirrorable(world))
    val plotList = workspace.plotManager.plots
    val plots = plotList.map(p => toMirrorable(new RichPlot(p, workspace)))
    val plotPens = for { p <- plotList; pp <- p.pens }
      yield toMirrorable(new RichPlotPen(pp, workspace))
    (worlds ++ turtles ++ patches ++ links ++ plots ++ plotPens)
  }

  type State = Map[AgentKey, Seq[AnyRef]]

  private def valueDiffs(was: Seq[AnyRef], now: Seq[AnyRef]): Seq[Change] =
    for (i <- was.indices if was(i) != now(i))
      yield Change(i, now(i))

  def diffs(oldState: State, currentWorkspace: AbstractWorkspaceScala): (State, Update) = {
    var births: Seq[Birth] = Vector()
    var deaths: Seq[Death] = Vector()
    var changes: Map[AgentKey, Seq[Change]] = Map()
    var newState: State = oldState
    var seen: Set[AgentKey] = Set()
    for (obj <- allMirrorables(currentWorkspace)) {
      val key = obj.agentKey
      seen += key
      val vars = (0 until obj.nbVariables).map(obj.getVariable)
      if (oldState.contains(key)) {
        val vd = valueDiffs(was = oldState(key), now = vars)
        if (vd.nonEmpty) {
          changes += key -> vd
          newState += key -> vars
        }
      } else {
        births :+= Birth(key, vars)
        newState += key -> vars
      }
    }
    for (key <- oldState.keys)
      if (!seen.contains(key)) {
        deaths :+= Death(key)
        newState -= key
      }
    (newState, Update(deaths, births, changes))
  }
  def merge(oldState: State, update: Update): State = {
    var newState: State = oldState
    for (Death(agent) <- update.deaths)
      newState -= agent
    for (Birth(agent, values) <- update.births)
      newState += agent -> values
    for ((agent, changes) <- update.changes)
      newState += agent -> mergeValues(oldState(agent), changes)
    newState
  }
  private def mergeValues(oldValues: Seq[AnyRef], changes: Seq[Change]): Seq[AnyRef] = {
    val newValues = oldValues.toArray
    for (Change(variable, value) <- changes)
      newValues(variable) = value
    newValues.toSeq
  }
}
