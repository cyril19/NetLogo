package org.nlogo.mirror

import scala.collection.JavaConverters.asScalaSetConverter
import scala.collection.JavaConverters.iterableAsScalaIterableConverter

import org.nlogo.api.AgentVariableNumbers._
import org.nlogo.api

sealed abstract class Kind
case object Patch extends Kind
case object Turtle extends Kind
case object Link extends Kind
case object World extends Kind

case class AgentKey(kind: Kind, id: Long)

case class Birth(agent: AgentKey, values: Seq[AnyRef])
case class Death(agent: AgentKey)
case class Change(variable: Int, value: AnyRef)

case class Update(deaths: Seq[Death], births: Seq[Birth], changes: Map[AgentKey, Seq[Change]])

object Mirroring {

  trait IsMirrorable[T] {
    def getVariable(x: T, index: Int): AnyRef
    def agentKey(x: T): AgentKey
    def nbVariables: Int
    def kind: Kind
  }
  class Mirrorable[+T](x: T, m: IsMirrorable[T]) {
    def nbVariables = m.nbVariables
    def getVariable(index: Int) = m.getVariable(x, index)
    def agentKey = m.agentKey(x)
    def kind = m.kind
  }
  implicit def toMirrorable[T](x: T)(implicit m: IsMirrorable[T]) = new Mirrorable(x, m)

  trait AgentIsMirrorable[A <: api.Agent] extends IsMirrorable[A] {
    val implicitVariables: Array[String]
    val variableOverrides = Map[Int, A => AnyRef]()
    val extraVariables = Seq[A => AnyRef]()
    lazy val nbImplicitVariables = implicitVariables.length
    override lazy val nbVariables = nbImplicitVariables + extraVariables.size
    lazy val extraVariablesIndices = nbImplicitVariables until nbVariables
    override def getVariable(agent: A, index: Int) =
      variableOverrides
        .get(index).map(_.apply(agent))
        .getOrElse(
          if (extraVariablesIndices contains index)
            extraVariables(index - nbImplicitVariables)(agent)
          else agent.getVariable(index))
    override def agentKey(agent: A) = AgentKey(kind, agent.id)
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

  implicit object WorldIsMirrorable extends IsMirrorable[api.World] {
    private val variableGetters = Seq[api.World => AnyRef](
      _.patchesWithLabels: java.lang.Integer,
      _.turtleShapeList,
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
      _.program)
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
        wvProgram
        ) = 0 until WorldIsMirrorable.variableGetters.size
    }
    override def getVariable(world: api.World, index: Int) = variableGetters(index)(world)
    override def agentKey(world: api.World) = AgentKey(World, 0)
    override val nbVariables = variableGetters.size
    override val kind = World
  }

  private def allMirrorables(world: api.World) = {
    import collection.JavaConverters._
    def mirrorables[A: IsMirrorable](agentSet: api.AgentSet) =
      agentSet.agents.asScala.map(_.asInstanceOf[A]).map(toMirrorable[A](_))
    mirrorables[api.Patch](world.patches) ++
      mirrorables[api.Turtle](world.turtles) ++
      mirrorables[api.Link](world.links) ++
      Iterable(toMirrorable(world))
  }

  type State = Map[AgentKey, Seq[AnyRef]]

  private def valueDiffs(was: Seq[AnyRef], now: Seq[AnyRef]): Seq[Change] =
    for (i <- was.indices if was(i) != now(i))
      yield Change(i, now(i))

  def diffs(oldState: State, currentWorld: api.World): (State, Update) = {
    var births: Seq[Birth] = Vector()
    var deaths: Seq[Death] = Vector()
    var changes: Map[AgentKey, Seq[Change]] = Map()
    var newState: State = oldState
    var seen: Set[AgentKey] = Set()
    for (obj <- allMirrorables(currentWorld)) {
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
