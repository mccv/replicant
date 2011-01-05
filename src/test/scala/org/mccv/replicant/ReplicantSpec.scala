package org.mccv.replicant

import org.specs._
import Replicant._

class Natives {
  var a:Array[Byte] = Array(1, 2)
  var s:Array[Short] = Array(1, 2)
  var i:Array[Int] = Array(1, 2)
  var l:Array[Long] = Array(1, 2)
  var f:Array[Float] = Array(1.0f, 2.0f)
  var d:Array[Double] = Array(1.0, 2.0)
  var bo:Array[Boolean] = Array(true, false)
  var c:Array[Char] = Array('c', 'd')
  override def toString = "natives!"
}
object ReplicantSpec extends Specification {

  "Replicant" should {
    "print an object" in {
      println("s".m.pub.~("index.*").takes("int"))
    }
    "print native array types" in {
      new Natives().m
    }

    "print scala-y stuff" in {
      List(1,2,3).m
    }

    "inspect constructors" in {
      println("s".c.takes("char"))
    }
  }
}
