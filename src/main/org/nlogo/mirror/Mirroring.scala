package org.nlogo.mirror

import org.nlogo.api

sealed abstract class Kind
case object Patch extends Kind
case object Turtle extends Kind
case object Link extends Kind

object AgentKey {
  def fromAgent(agent: api.Agent): AgentKey =
    AgentKey(
      agent match {
        case _: api.Patch => Patch
        case _: api.Turtle => Turtle
        case _: api.Link => Link
      }, agent.id)
}
case class AgentKey(kind: Kind, id: Long)

case class Birth(agent: AgentKey, values: Seq[AnyRef])
case class Death(agent: AgentKey)
case class Change(variable: Int, value: AnyRef)

case class Update(deaths: Seq[Death], births: Seq[Birth], changes: Map[AgentKey, Seq[Change]])

object Mirroring {
  type State = Map[AgentKey, Seq[AnyRef]]
  private def allAgents(world: api.World): Iterable[api.Agent] = {
    import collection.JavaConverters._
    world.patches.agents.asScala ++
    world.turtles.agents.asScala ++
    world.links.agents.asScala
  }
  private def valueDiffs(was: Seq[AnyRef], now: Seq[AnyRef]): Seq[Change] =
    for (i <- was.indices if was(i) != now(i))
    yield Change(i, now(i))
  private val builtins: Map[Kind, Int] =
    Map(Patch -> api.AgentVariables.getImplicitPatchVariables.size,
        Turtle -> api.AgentVariables.getImplicitTurtleVariables.size,
        Link -> api.AgentVariables.getImplicitLinkVariables.size)
  def diffs(state: State, world: api.World): (State, Update) = {
    var births: Seq[Birth] = Seq()
    var deaths: Seq[Death] = Seq()
    var changes: Map[AgentKey, Seq[Change]] = Map()
    var newState: State = Map()
    var seen: Set[AgentKey] = Set()
    for(agent <- allAgents(world)) {
      val key = AgentKey.fromAgent(agent)
      seen += key
      val vars = (0 until builtins(key.kind)).map(agent.getVariable)
      if(state.contains(key)) {
        val vd = valueDiffs(was = state(key),
                            now = vars)
        if(!vd.isEmpty)
          changes += key -> vd
      }
      else
        births :+= Birth(key, vars)
      newState += key -> vars
    }
    for(key <- state.keys)
      if(!seen.contains(key)) {
        deaths :+= Death(key)
        newState -= key
      }
    (newState, Update(deaths, births, changes))
  }
  def merge(state: State, update: Update): State = {
    var newState: State = state
    for(Death(agent) <- update.deaths)
      newState -= agent
    for(Birth(agent, values) <- update.births)
      newState += agent -> values
    for((agent, changes) <- update.changes)
      newState += agent -> mergeValues(state(agent), changes)
    newState
  }
  private def mergeValues(oldValues: Seq[AnyRef], changes: Seq[Change]): Seq[AnyRef] = {
    val newValues = oldValues.toArray
    for(Change(variable, value) <- changes)
      newValues(variable) = value
    newValues.toSeq
  }
}

