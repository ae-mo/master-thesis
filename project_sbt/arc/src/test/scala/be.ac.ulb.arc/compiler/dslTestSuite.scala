/*
package be.ac.ulb.arc.compiler

import scala.lms.tutorial._
import scala.collection.immutable.{HashSet => SVars}
import scala.{Int => SVar}
import scala.{Int => Position}
import scala.{Array => Program}
import scala.collection.mutable.{Set => VSRelation}
import scala.{Array => VSTuple}

class Snippet extends ((java.lang.String)=>(scala.collection.mutable.Set[Array[Int]])) {
  def apply(x0:java.lang.String): scala.collection.mutable.Set[Array[Int]] = {
    val x1 = collection.mutable.HashSet[Array[Int]]()
    val x2 = new Array[Int](4)
    val x3 = x0.length
    var x5 : Int = 0
    val x11 = while (x5 < x3) {
      val x6 = x2(0) = x5

      x5 = x5 + 1
    }
    x1
  }
}

class dslTestSuite extends TutorialFunSuite {

  val data = new {

    val vars = new SVars[SVar] + 1 + 2

    val prog2 = new Program[Instruction](11)
    prog2(0) = new Instruction(5, Array[Int](0, 0))
    prog2(2) = new Instruction(0, Array[Int]('a'.asDigit, 2))
    prog2(4) = new Instruction(5, Array[Int](1, 4))
    prog2(1) = new Instruction(3, Array[Int](1, 2, 4))
    prog2(3) = new Instruction(4, Array[Int](3, 1))
    prog2(5) = new Instruction(5, Array[Int](2, 5))
    prog2(7) = new Instruction(0, Array[Int]('a'.asDigit, 7))
    prog2(9) = new Instruction(5, Array[Int](3, 9))
    prog2(6) = new Instruction(3, Array[Int](6, 7, 9))
    prog2(8) = new Instruction(4, Array[Int](8,6))
    prog2(10) = new Instruction(2, Array[Int](10))


    val str2 = "aaaab" + '\0'
  }

  val under = "dlstest"

  test("No reps") {

    val snippet = new DSLDriver with VirtualMachineOps{
      def snippet(x: Rep[String]) = executeNFAProgram(data.prog2, data.vars, x)

    }

   print(snippet.code)

  }

  test("let us try") {

    val s= new Snippet

    val result = s.apply(data.str2)

    print("hey")
  }
}
*/
