package org.nlogo.headless

import org.scalatest.FunSuite
import org.scalatest.exceptions.TestFailedException
import collection.JavaConverters._
import org.nlogo.util.Pico
import org.nlogo.api
import api.AgentVariables
import org.nlogo.mirror
import mirror._
import Mirroring._

class TestMirroring extends FunSuite {

  def sizes(u: Update) =
    (u.births.size, u.deaths.size, u.changes.size)

  def withWorkspace[T](body: HeadlessWorkspace => T): T = {
    val ws = HeadlessWorkspace.newInstance
    try body(ws)
    finally ws.dispose()
  }

  def checkAllAgents(ws: HeadlessWorkspace, m: State) {
    expect(ws.world.patches.count) { m.count(_._1.kind == Patch) }
    for (patch <- ws.world.patches.agents.asScala)
      assert(m(AgentKey(Patch, patch.id)) sameElements
        patch.variables.take(AgentVariables.getImplicitPatchVariables.size))
    expect(ws.world.turtles.count) { m.count(_._1.kind == Turtle) }
    for (turtle <- ws.world.turtles.agents.asScala)
      assert(m(AgentKey(Turtle, turtle.id)) sameElements
        turtle.variables.take(AgentVariables.getImplicitTurtleVariables.size))
    expect(ws.world.links.count) { m.count(_._1.kind == Link) }
    for (link <- ws.world.links.agents.asScala)
      assert(m(AgentKey(Link, link.id)) sameElements
        link.variables.take(AgentVariables.getImplicitLinkVariables.size))
  }

  test("init") {
    withWorkspace { ws =>

      ws.initForTesting(1)
      val (m0, u0) = diffs(Map(), ws.world)
      expect((9, (9, 0, 0))) { (m0.size, sizes(u0)) }
      checkAllAgents(ws, m0)

      ws.command("crt 10")
      val (m1, u1) = diffs(m0, ws.world)
      expect((19, (10, 0, 0))) { (m1.size, sizes(u1)) }
      checkAllAgents(ws, m1)

      ws.command("ask one-of turtles [ set color red + 2 set size 3 ]")
      val (m2, u2) = diffs(m1, ws.world)
      expect((19, (0, 0, 1))) { (m2.size, sizes(u2)) }
      expect("List(Change(1,17.0), Change(10,3.0))") {
        u2.changes.head._2.toList.toString
      }
      checkAllAgents(ws, m2)

      ws.command("ask n-of 5 turtles [ die ]")
      val (m3, u3) = diffs(m2, ws.world)
      expect((14, (0, 5, 0))) { (m3.size, sizes(u3)) }
      checkAllAgents(ws, m3)

      val (m4, u4) = diffs(m3, ws.world)
      expect((14, (0, 0, 0))) { (m4.size, sizes(u4)) }
      checkAllAgents(ws, m4)

      ws.command("ask one-of patches [ set pcolor green ]")
      intercept[TestFailedException] {
        checkAllAgents(ws, m4)
      }
      ws.command("clear-patches")
      checkAllAgents(ws, m4)

    }
  }

  test("user-declared variables don't matter") {
    withWorkspace { ws =>
      val declarations =
        "patches-own [pfoo] " +
          "turtles-own [tfoo] " +
          "links-own   [lfoo]"
      ws.initForTesting(1, declarations)
      ws.command("create-turtles 3 [ create-links-with other turtles ]")
      val (m0, u0) = diffs(Map(), ws.world)
      expect((15, (15, 0, 0))) { (m0.size, sizes(u0)) }
      checkAllAgents(ws, m0)
      ws.command("ask patches [ set pfoo 1 ] " +
        "ask turtles [ set tfoo 1 ] " +
        "ask links   [ set lfoo 1 ]")
      checkAllAgents(ws, m0)
      val (m1, u1) = diffs(m0, ws.world)
      expect((15, (0, 0, 0))) { (m1.size, sizes(u1)) }
      checkAllAgents(ws, m1)
    }
  }

  test("merge") {
    withWorkspace { ws =>
      ws.initForTesting(1)
      val (m0, u0) = diffs(Map(), ws.world)
      var state: State = Mirroring.merge(Map(), u0)
      checkAllAgents(ws, m0)
      checkAllAgents(ws, state)
      ws.command("ask patches [ sprout 1 set pcolor pxcor ]")
      ws.command("ask n-of (count turtles / 2) turtles [ die ]")
      ws.command("ask turtles [ create-links-with other turtles ]")
      val (m1, u1) = diffs(m0, ws.world)
      // 9 patches + 5 turtles + 10 links = 24 agents total, 15 of which
      // are newborn.  6 patches changed color (some already had pxcor = pcolor)
      expect((24, (15, 0, 6))) { (m1.size, sizes(u1)) }
      checkAllAgents(ws, m1)
      intercept[TestFailedException] {
        checkAllAgents(ws, state)
      }
      state = Mirroring.merge(state, u1)
      checkAllAgents(ws, state)
      ws.command("ask n-of 3 turtles [ die ]")
      val (m2, u2) = diffs(m1, ws.world)
      // 9 patches + 2 turtles + 1 link remain
      expect((12, (0, 12, 0))) { (m2.size, sizes(u2)) }
      checkAllAgents(ws, m2)
      state = Mirroring.merge(state, u2)
      checkAllAgents(ws, state)
    }
  }

  def modelRenderingTest(path: String) {
    withWorkspace { ws =>
      ws.open(path)
      ws.command("random-seed 0")
      ws.command(ws.previewCommands)
      val (m0, u0) = diffs(Map(), ws.world)
      var state = Mirroring.merge(Map(), u0)
      // should I test that m0 and state are identical? maybe have a separate test for that
      val dummy = new FakeWorld(ws.world, state) {}
      val pico = new Pico
      pico.add("org.nlogo.render.Renderer")
      pico.addComponent(dummy)
      val renderer = pico.getComponent(classOf[api.RendererInterface])
      renderer.resetCache(ws.patchSize)
      val realChecksum =
        Checksummer.calculateGraphicsChecksum(ws.renderer, ws)
      val mirrorChecksum =
        Checksummer.calculateGraphicsChecksum(renderer, ws)
      expect(realChecksum) { mirrorChecksum }
    }
  }

  test("slime") {
    modelRenderingTest("models/Sample Models/Biology/Slime.nlogo")
  }

  test("wolf") {
    modelRenderingTest("models/Sample Models/Biology/Wolf Sheep Predation.nlogo")
  }
  
  // takes 40 seconds, commenting out for now
  // test("fire") {
  //   modelRenderingTest("models/Sample Models/Earth Science/Fire.nlogo")
  // }

}
