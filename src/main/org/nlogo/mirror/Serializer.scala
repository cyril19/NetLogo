// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.mirror

import org.nlogo.api
import java.io.{ ByteArrayOutputStream, DataOutputStream,
                 ByteArrayInputStream, DataInputStream }
import collection.immutable.Vector

object Serializer {

  def toBytes(update: Update): Array[Byte] = {
    val bytes = new ByteArrayOutputStream
    val data = new DataOutputStream(bytes)
    def writeValue(x: AnyRef) {
      x match {
        case s: String =>
          data.writeByte(api.Syntax.StringType)
          data.writeUTF(s)
        case _ =>
          sys.error(x.getClass.toString)
      }
    }
    def writeSeq(xs: Seq[AnyRef]) {
      data.writeInt(xs.size)
      xs.foreach(writeValue)
    }
    data.writeInt(update.deaths.size)
    for(Death(AgentKey(kind, id)) <- update.deaths) {
      data.writeByte(kind)
      data.writeLong(id)
    }
    data.writeInt(update.births.size)
    for(Birth(AgentKey(kind, id), values) <- update.births) {
      data.writeByte(kind)
      data.writeLong(id)
      writeSeq(values)
    }
    data.writeInt(update.changes.size)
    for((AgentKey(kind, id), changes) <- update.changes) {
      data.writeByte(kind)
      data.writeLong(id)
      data.writeInt(changes.size)
      for(Change(variable, value) <- changes) {
        data.writeInt(variable)
        writeValue(value)
      }
    }
    bytes.toByteArray
  }

  // TODO cache the AgentKey objects - ST 7/23/12

  def fromBytes(bytes: Array[Byte]): Update = {
    val data = new DataInputStream(
      new ByteArrayInputStream(bytes))
    def readValue(): AnyRef =
      data.readByte() match {
        case api.Syntax.StringType =>
          data.readUTF()
        case x =>
          sys.error("unknown value type: " + x)
      }
    def readValues(): Vector[AnyRef] =
      (for(_ <- 0 until data.readInt())
       yield readValue())(collection.breakOut)
    def readAgentKey(): AgentKey =
      AgentKey(kind = data.readByte().toInt,
               id = data.readLong())
    var deaths = Vector[Death]()
    for(_ <- 0 until data.readInt())
      deaths :+= Death(readAgentKey())
    var births = Vector[Birth]()
    for(_ <- 0 until data.readInt())
      births :+= Birth(readAgentKey(),
                       readValues())
    var changes = Vector[(AgentKey, Seq[Change])]()
    for(_ <- 0 until data.readInt())
      changes :+= (readAgentKey(),
                   (for(_ <- 0 until data.readInt())
                    yield Change(data.readInt(), readValue())))
    Update(deaths = deaths,
           births = births,
           changes = changes)
  }

  implicit val agentKindFromInt: Int => Kind =
    Seq(Mirrorables.Observer,
        Mirrorables.Turtle,
        Mirrorables.Patch,
        Mirrorables.Link)

  implicit def agentKindToInt(kind: Kind): Int =
    kind match {
      case Mirrorables.Observer => 0
      case Mirrorables.Turtle => 1
      case Mirrorables.Patch => 2
      case Mirrorables.Link => 3
    }

}
