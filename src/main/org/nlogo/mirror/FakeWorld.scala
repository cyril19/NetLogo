package org.nlogo.mirror

import org.nlogo.api
import Mirroring.State
import collection.JavaConverters._
import api.AgentVariableNumbers._
import scala.collection.breakOut

abstract class FakeWorld(state: State) extends api.World {

  private val (worldVars, patchStates, turtleStates, linkStates) = {
    // each group will be a seq of (agentId, vars):
    val groups = state.toSeq.groupBy(_._1.kind).map {
      case (kind, states) => kind -> states.map {
        case (AgentKey(_, agentId), vars) => agentId -> vars
      }
    }
    (groups(World).head._2, // world should always be there 
      groups(Patch), // patches should always be there
      groups.getOrElse(Turtle, Seq()), // there might be no turtles 
      groups.getOrElse(Link, Seq())) // there might be no links
  }

  def patchColors: Array[Int] =
    patches.agentSeq
      .map(patch => api.Color.getRGBInt(patch.pcolor))
      .toArray

  class FakeAgentSet[A <: api.Agent: Manifest](val agentSeq: Seq[A])
    extends api.AgentSet {
    val `type` = classManifest[A].erasure.asInstanceOf[Class[api.Agent]]
    def count = agentSeq.size
    def world: api.World = FakeWorld.this
    def equalAgentSets(other: api.AgentSet) = unsupported
    override val agents = (agentSeq: Iterable[api.Agent]).asJava
    override def printName = unsupported
  }
  trait FakeAgent extends api.Agent {
    val vars: Seq[AnyRef]
    override def variables = unsupported
    def world = unsupported
    def classDisplayName = unsupported
    def alpha = unsupported
    def getVariable(vn: Int) = unsupported
    def setVariable(vn: Int, value: AnyRef) = unsupported
    def isPartiallyTransparent = unsupported
  }

  class FakeTurtle(agentId: Long, val vars: Seq[AnyRef])
    extends api.Turtle with FakeAgent {
    import Mirrorables.MirrorableTurtle._
    override def id = agentId
    override def xcor = vars(VAR_XCOR).asInstanceOf[Double]
    override def ycor = vars(VAR_YCOR).asInstanceOf[Double]
    override def heading = vars(VAR_HEADING).asInstanceOf[Double]
    override def hidden = vars(VAR_HIDDEN).asInstanceOf[Boolean]
    override def lineThickness = vars(tvLineThickness).asInstanceOf[Double]
    override def color = vars(VAR_COLOR)
    override def labelString = org.nlogo.api.Dump.logoObject(vars(VAR_LABEL))
    override def hasLabel = labelString.nonEmpty
    override def labelColor = vars(VAR_LABELCOLOR)
    override def getBreed = Option(program.breeds.get(vars(VAR_BREED))).getOrElse(turtles).asInstanceOf[api.AgentSet]
    override def size = vars(VAR_SIZE).asInstanceOf[Double]
    override def shape = vars(VAR_SHAPE).asInstanceOf[String]
    override def getBreedIndex = unsupported
    override def getPatchHere = unsupported
    override def jump(distance: Double) = unsupported
    override def heading(d: Double) = unsupported
  }

  override val turtles =
    new FakeAgentSet(turtleStates.map {
      case (id, vars) => new FakeTurtle(id, vars)
    }.sortBy(_.id))

  class FakePatch(agentId: Long, val vars: Seq[AnyRef])
    extends api.Patch with FakeAgent {
    override def id = agentId
    override def pxcor = vars(VAR_PXCOR).asInstanceOf[Int]
    override def pycor = vars(VAR_PYCOR).asInstanceOf[Int]
    override def pcolor = vars(VAR_PCOLOR)
    override def labelString = org.nlogo.api.Dump.logoObject(vars(VAR_PLABEL))
    override def hasLabel = labelString.nonEmpty
    override def labelColor = vars(VAR_PLABELCOLOR)
    override def getPatchAtOffsets(dx: Double, dy: Double) = unsupported
    override def size = 1
    override def shape = ""
  }

  override val patches =
    new FakeAgentSet(patchStates.map {
      case (id, vars) => new FakePatch(id, vars)
    }.sortBy(_.id))

  class FakeLink(agentId: Long, val vars: Seq[AnyRef]) extends api.Link with FakeAgent {
    override def id = agentId
    override def getBreedIndex: Int = unsupported
    override def labelColor: AnyRef = vars(VAR_LLABELCOLOR)
    override def labelString: String = org.nlogo.api.Dump.logoObject(vars(VAR_LLABEL))
    override def color: AnyRef = vars(VAR_LCOLOR)
    override def hasLabel: Boolean = labelString.nonEmpty
    override def lineThickness: Double = vars(VAR_THICKNESS).asInstanceOf[Double]
    override def hidden: Boolean = vars(VAR_LHIDDEN).asInstanceOf[Boolean]
    override def linkDestinationSize: Double = unsupported
    override def heading: Double = unsupported
    override def isDirectedLink: Boolean = false // TODO ((AgentSet) variables[VAR_BREED]).isDirected()
    override def x1: Double = end1.xcor
    override def y1: Double = end1.ycor
    override def x2: Double = end2.xcor
    override def y2: Double = end2.ycor
    override def midpointY: Double = unsupported
    override def midpointX: Double = unsupported
    override def getBreed: api.AgentSet = Option(program.linkBreeds.get(vars(VAR_LBREED))).getOrElse(links).asInstanceOf[api.AgentSet]
    // maybe I should keep a map from id to agent somewhere? Not sure it's worth it, though...
    override def end1 = turtles.agentSeq.find(_.id == vars(VAR_END1).asInstanceOf[Long]).get
    override def end2 = turtles.agentSeq.find(_.id == vars(VAR_END2).asInstanceOf[Long]).get
    override def size: Double = 1 // TODO: world.protractor.distance(end1, end2, true) 
    override def shape = vars(VAR_LSHAPE).asInstanceOf[String]
    override def toString = id + " link " + end1.id + " " + end2.id // TODO: get breed name in there
  }

  override val links = new FakeAgentSet(linkStates.map {
    case (id, vars) => new FakeLink(id, vars)
  }.sortBy(_.id)) {
    override val agents = (agentSeq.sortBy(l => (l.end1.id, l.end2.id)): Iterable[api.Agent]).asJava
  }

  import Mirrorables.MirrorableWorld._
  private def worldVar[T](i: Int) = worldVars(i).asInstanceOf[T]

  def patchesWithLabels = worldVar[Int](wvPatchesWithLabels)
  def turtleShapeList = worldVar[api.ShapeList](wvTurtleShapeList)
  def linkShapeList = worldVar[api.ShapeList](wvlinkShapeList)
  def patchSize = worldVar[Double](wvPatchSize)
  def worldWidth = worldVar[Int](wvWorldWidth)
  def worldHeight = worldVar[Int](wvWorldHeight)
  def minPxcor = worldVar[Int](wvMinPxcor)
  def minPycor = worldVar[Int](wvMinPycor)
  def maxPxcor = worldVar[Int](wvMaxPxcor)
  def maxPycor = worldVar[Int](wvMaxPycor)
  def wrappingAllowedInX = worldVar[Boolean](wvWrappingAllowedInX)
  def wrappingAllowedInY = worldVar[Boolean](wvWrappingAllowedInY)
  def patchesAllBlack = worldVar[Boolean](wvPatchesAllBlack)

  def trailDrawing = worldVar[Option[Array[Byte]]](wvTrailDrawing)
  
  def program = new api.Program {
    private def makeBreeds[A <: FakeAgent: Manifest](
      breedWorldVar: Int, genericName: String,
      agentSet: FakeAgentSet[A]): java.util.Map[String, AnyRef] = {
      val nameToAgentSet = { breedName: String =>
        val agentSeq = agentSet.agentSeq.filter(_.vars(VAR_BREED) == breedName)
        breedName -> new FakeAgentSet[A](agentSeq).asInstanceOf[AnyRef]
      }
      val breedSeq = worldVar[Seq[String]](breedWorldVar).map(nameToAgentSet)
      val breeds = new java.util.LinkedHashMap[String, AnyRef]
      for ((breedName, agentSet) <- breedSeq)
        breeds.put(breedName, agentSet)
      breeds
    }

    override val breeds = makeBreeds(wvTurtleBreeds, "TURTLES", turtles)
    override val linkBreeds = makeBreeds(wvLinkBreeds, "LINKS", links)
    override def dump: java.lang.String = unsupported
  }

  def getPatch(i: Int): api.Patch = patches.agentSeq(i)
  
  // unsupported
  def wrap(pos: Double, min: Double, max: Double): Double = unsupported
  def ticks: Double = unsupported
  def observer: api.Observer = unsupported
  def getPatchAt(x: Double, y: Double): api.Patch = unsupported
  def fastGetPatchAt(x: Int, y: Int): api.Patch = unsupported
  def followOffsetX: Double = unsupported
  def followOffsetY: Double = unsupported
  def wrapX(x: Double): Double = unsupported
  def wrapY(y: Double): Double = unsupported
  def getDrawing: AnyRef = unsupported
  def sendPixels: Boolean = unsupported
  def markDrawingClean() = unsupported
  def protractor: api.Protractor = unsupported
  def wrappedObserverX(x: Double): Double = unsupported
  def wrappedObserverY(y: Double): Double = unsupported
  def patchColorsDirty: Boolean = unsupported
  def markPatchColorsDirty() = unsupported
  def markPatchColorsClean() = unsupported
  def getVariablesArraySize(link: api.Link, breed: api.AgentSet): Int = unsupported
  def getVariablesArraySize(turtle: api.Turtle, breed: api.AgentSet): Int = unsupported
  def linksOwnNameAt(i: Int): String = unsupported
  def turtlesOwnNameAt(i: Int): String = unsupported
  def breedsOwnNameAt(breed: api.AgentSet, i: Int): String = unsupported
  def allStoredValues: Iterator[AnyRef] = unsupported
  def trailDrawer: api.TrailDrawerInterface = unsupported

  private def unsupported = throw new UnsupportedOperationException
}
