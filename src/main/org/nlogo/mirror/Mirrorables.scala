package org.nlogo.mirror

import org.nlogo.api.AgentVariableNumbers._
import org.nlogo.api
import org.nlogo.plot

import collection.JavaConverters._

object Mirrorables {

  case object Patch extends Kind
  case object Turtle extends Kind
  case object Link extends Kind
  case object Observer extends Kind
  case object World extends Kind
  case object Plot extends Kind
  case object PlotPen extends Kind
  case object InterfaceGlobals extends Kind

  // so we don't fill up memory with duplicate AgentKey objects
  val keyCache = collection.mutable.WeakHashMap[api.Agent, AgentKey]()

  abstract class MirrorableAgent[T <: api.Agent](agent: T) extends Mirrorable {
    override def getVariable(index: Int) = variables.getOrElse(index, agent.getVariable(index))
    override def agentKey = keyCache.getOrElseUpdate(agent, AgentKey(kind, agent.id))
  }

  object MirrorableTurtle {
    val tvLineThickness = api.AgentVariables.getImplicitTurtleVariables.size
  }
  class MirrorableTurtle(turtle: api.Turtle) extends MirrorableAgent(turtle) {
    import MirrorableTurtle._
    override def kind = Turtle
    override def nbVariables = api.AgentVariables.getImplicitTurtleVariables.size + 1
    override val variables = Map(
      VAR_BREED -> turtle.getBreed.printName,
      tvLineThickness -> double2Double(turtle.lineThickness))
  }

  class MirrorablePatch(patch: api.Patch) extends MirrorableAgent(patch) {
    override def kind = Patch
    override def nbVariables = api.AgentVariables.getImplicitPatchVariables.size
    override val variables = Map(
      VAR_PXCOR -> int2Integer(patch.pxcor),
      VAR_PYCOR -> int2Integer(patch.pycor))
  }

  class MirrorableLink(link: api.Link) extends MirrorableAgent(link) {
    override def kind = Link
    override def nbVariables = api.AgentVariables.getImplicitLinkVariables.size
    override val variables = Map(
      VAR_END1 -> long2Long(link.end1.id),
      VAR_END2 -> long2Long(link.end2.id),
      VAR_LBREED -> link.getBreed.printName)
  }

  object MirrorableWorld {
    val Seq(
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
      ) = 0 until 16
  }
  class MirrorableWorld(world: api.World) extends Mirrorable {
    import MirrorableWorld._
    override def kind = World
    override def agentKey = AgentKey(kind, 0) // dummy id for the one and unique world
    override val variables = Map(
      wvPatchesWithLabels -> int2Integer(world.patchesWithLabels),
      wvTurtleShapeList -> world.turtleShapeList, // probably not good enough to just pass the shapelists like that...
      wvlinkShapeList -> world.linkShapeList,
      wvPatchSize -> double2Double(world.patchSize),
      wvWorldWidth -> int2Integer(world.worldWidth),
      wvWorldHeight -> int2Integer(world.worldHeight),
      wvMinPxcor -> int2Integer(world.minPxcor),
      wvMinPycor -> int2Integer(world.minPycor),
      wvMaxPxcor -> int2Integer(world.maxPxcor),
      wvMaxPycor -> int2Integer(world.maxPycor),
      wvWrappingAllowedInX -> boolean2Boolean(world.wrappingAllowedInX),
      wvWrappingAllowedInY -> boolean2Boolean(world.wrappingAllowedInY),
      wvPatchesAllBlack -> boolean2Boolean(world.patchesAllBlack),
      wvTurtleBreeds -> world.program.breeds.keys.toSeq,
      wvLinkBreeds -> world.program.linkBreeds.keys.toSeq,
      wvTrailDrawing ->
        (if (world.trailDrawer.isDirty) {
          val outputStream = new java.io.ByteArrayOutputStream
          val img = world.trailDrawer.getDrawing.asInstanceOf[java.awt.image.BufferedImage]
          javax.imageio.ImageIO.write(img, "png", outputStream)
          Some(outputStream.toByteArray())
        } else None))
  }

  object MirrorablePlot {
    val Seq(
      pvXMin,
      pvXMax,
      pvYMin,
      pvYMax,
      pvLegendIsOpen
      ) = 0 until 5
  }

  class MirrorablePlot(val p: plot.Plot, val plots: List[plot.Plot]) extends Mirrorable {
    import MirrorablePlot._
    override def kind = Plot
    override def agentKey = AgentKey(kind, plots.indexOf(p))
    override val variables = Map(
      pvXMin -> double2Double(p.xMin),
      pvXMax -> double2Double(p.xMax),
      pvYMin -> double2Double(p.yMin),
      pvYMax -> double2Double(p.yMax),
      pvLegendIsOpen -> boolean2Boolean(p.legendIsOpen))
  }

  object MirrorablePlotPen {
    val Seq( // init vals for indices by pattern matching over range of getters
      ppvName,
      ppvIsDown,
      ppvMode,
      ppvInterval,
      ppvColor,
      ppvX,
      ppvPoints
      ) = 0 until 7
  }
  class MirrorablePlotPen(val pen: plot.PlotPen, val plots: List[plot.Plot]) extends Mirrorable {
    import MirrorablePlotPen._
    override def kind = PlotPen
    override def agentKey = {
      // we combine the plot id and the pen id (which are both
      // originally Ints) into a single Long:
      val plotId: Long = plots.indexOf(pen.plot)
      val penId: Long = pen.plot.pens.indexOf(pen)
      AgentKey(kind, (plotId << 32) | penId)
    }
    override val variables = Map(
      ppvName -> pen.name,
      ppvIsDown -> boolean2Boolean(pen.isDown),
      ppvMode -> int2Integer(pen.mode),
      ppvInterval -> double2Double(pen.interval),
      ppvColor -> org.nlogo.api.Color.argbToColor(pen.color),
      ppvX -> double2Double(pen.x),
      ppvPoints -> pen.points.toList)
  }

  class MirrorableInterfaceGlobals(world: api.World) extends Mirrorable {
    def kind = InterfaceGlobals
    def agentKey = AgentKey(kind, 0)
    val variables =
      world.program.interfaceGlobals
        .zipWithIndex
        .map { case (name, i) => i -> (name, world.observer.getVariable(i)) }
        .toMap
  }

  def allMirrorables(world: api.World, plots: List[plot.Plot]): Iterable[Mirrorable] = {
    import collection.JavaConverters._
    val turtles = world.turtles.agents.asScala.map(t => new MirrorableTurtle(t.asInstanceOf[api.Turtle]))
    val patches = world.patches.agents.asScala.map(p => new MirrorablePatch(p.asInstanceOf[api.Patch]))
    val links = world.links.agents.asScala.map(l => new MirrorableLink(l.asInstanceOf[api.Link]))
    val worldIterable = Iterable(new MirrorableWorld(world))
    val interfaceGlobals = Iterable(new MirrorableInterfaceGlobals(world))
    val plotMirrorables = for { p <- plots } yield new MirrorablePlot(p, plots)
    val plotPens = for { p <- plots; pp <- p.pens } yield new MirrorablePlotPen(pp, plots)
    (worldIterable ++ interfaceGlobals ++ turtles ++ patches ++ links ++ plotMirrorables ++ plotPens)
  }

}
