package org.nlogo.mirror

import org.nlogo.api
import Mirroring.State
import collection.JavaConverters._

abstract class FakeWorld(world: api.World, state: State) extends api.World {
  // replace with real implementations
  def patchColors: Array[Int] =
    state
      .toSeq
      .filter(_._1.kind == Patch)
      .sortBy(_._1.id)
      .map(_._2)
      .map(_(api.AgentVariableNumbers.VAR_PCOLOR))
      .map(api.Color.getRGBInt)
      .toArray
  val turtles: api.AgentSet =
    new api.AgentSet {
      val filtered = state.toSeq.filter(_._1.kind == Turtle)
      def printName = unsupported
      def count = filtered.size
      def `type` = unsupported
      def world: api.World = FakeWorld.this
      def equalAgentSets(other: api.AgentSet) = unsupported
      def agents: java.lang.Iterable[api.Agent] =
        filtered
          .sortBy(_._1.id)
          .map{case (AgentKey(kind, theid), vars) =>
            new api.Turtle {
              def variables = vars.toArray
              def xcor = vars(api.AgentVariableNumbers.VAR_XCOR).asInstanceOf[java.lang.Double].doubleValue
              def ycor = vars(api.AgentVariableNumbers.VAR_YCOR).asInstanceOf[java.lang.Double].doubleValue
              def heading = vars(api.AgentVariableNumbers.VAR_HEADING).asInstanceOf[java.lang.Double].doubleValue
              def hidden = false
              def lineThickness = 0
              def hasLabel = false
              def color = vars(api.AgentVariableNumbers.VAR_COLOR)
              def labelString = ""
              def labelColor = vars(api.AgentVariableNumbers.VAR_COLOR)
              def getBreed = turtles
              def getBreedIndex = unsupported
              def getPatchHere = unsupported
              def jump(distance: Double) = unsupported
              def heading(d: Double) = unsupported
              def getVariable(vn: Int) = unsupported
              def setVariable(vn: Int, value: AnyRef) = unsupported
              def isPartiallyTransparent = unsupported
              def alpha = unsupported
              def size = vars(api.AgentVariableNumbers.VAR_SIZE).asInstanceOf[java.lang.Double].doubleValue
              def shape = vars(api.AgentVariableNumbers.VAR_SHAPE).asInstanceOf[String]
              def id = theid
              def world = unsupported
              def classDisplayName = unsupported
              private def unsupported = throw new UnsupportedOperationException
            }: api.Agent}
          .asJava
    }
  def patches: api.AgentSet = world.patches
  def links: api.AgentSet = world.links
  def patchesWithLabels: Int = world.patchesWithLabels
  // totally ok to delegate
  def turtleShapeList: api.ShapeList = world.turtleShapeList
  def linkShapeList: api.ShapeList = world.linkShapeList
  def patchSize: Double = world.patchSize
  def worldWidth: Int = world.worldWidth
  def worldHeight: Int = world.worldHeight
  def minPxcor: Int = world.minPxcor
  def minPycor: Int = world.minPycor
  def maxPxcor: Int = world.maxPxcor
  def maxPycor: Int = world.maxPycor
  def wrappingAllowedInX: Boolean = world.wrappingAllowedInX
  def wrappingAllowedInY: Boolean = world.wrappingAllowedInY
  def program: api.Program = world.program
  // correct, but inefficient
  def patchesAllBlack: Boolean = false
  // unsupported
  def wrap(pos: Double, min: Double, max: Double): Double = unsupported
  def ticks: Double = unsupported
  def observer: api.Observer = unsupported
  def getPatch(i: Int): api.Patch = unsupported
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
  private def unsupported = throw new UnsupportedOperationException
}
