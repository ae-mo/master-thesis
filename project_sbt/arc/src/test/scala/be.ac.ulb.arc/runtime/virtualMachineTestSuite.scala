package be.ac.ulb.arc.runtime

import org.scalatest.FunSuite

import scala.collection.immutable.{HashSet => SVars}
import scala.{Int => SVar}
import scala.{Int => Position}
import scala.{Array => Pointers}
import scala.{Array => Program}
import scala.collection.immutable.{HashSet => VSRelation}
import scala.{Array => VSTuple}
import scala.collection.mutable.{MutableList => ThreadList}

/**
  * Created by andrea on 19/05/16.
  */
class virtualMachineTestSuite extends FunSuite{

  def data = new {

    val prog1 = new Program[Instruction](10)
    prog1(0) = new SAVE(0, 0)
    prog1(1) = new CHAR('a', 1)
    prog1(3) = new CHAR('a', 3)
    prog1(5) = new SAVE(1, 5)
    prog1(2) = new SPLIT(2, prog1(3), prog1(5))
    prog1(4) = new JUMP(4, prog1(2))
    prog1(6) = new SAVE(2, 6)
    prog1(7) = new CHAR('b', 7)
    prog1(8) = new SAVE(3, 8)
    prog1(9) = new MATCH(9)

    val str1 = "aaaaab" + '\0'

    val prog2 = new Program[Instruction](11)
    prog2(0) = new SAVE(0, 0)
    prog2(2) = new CHAR('a', 2)
    prog2(4) = new SAVE(1, 4)
    prog2(1) = new SPLIT(1, prog2(2), prog2(4))
    prog2(3) = new JUMP(3, prog2(1))
    prog2(5) = new SAVE(2, 5)
    prog2(7) = new CHAR('a', 7)
    prog2(9) = new SAVE(3, 9)
    prog2(6) = new SPLIT(7, prog2(7), prog2(9))
    prog2(8) = new JUMP(8, prog2(6))
    prog2(10) = new MATCH(10)

    val str2 = "aaaab" + '\0'

  }

  test("VM should match prog1 on str1") {

    val vars = new SVars[SVar] + 1 + 2

    val tuplesOpt = VirtualMachine.execute(data.prog1, vars, data.str1)

    assert(tuplesOpt != None)

    val tuples = tuplesOpt.get

    assert(tuples.size == 1)
    assert(tuples.exists((p: VSTuple[Position]) => p(0) == 0 && p(1) == 5 && p(2) == 5 && p(3) == 6))

    OutputWriter.printOutput(data.str1, tuples)
  }

  test("VM should match prog2 on str2") {

    val vars = new SVars[SVar] + 1 + 2

    val tuplesOpt = VirtualMachine.execute(data.prog2, vars, data.str2)

    assert(tuplesOpt != None)

    val tuples = tuplesOpt.get

    OutputWriter.printOutput(data.str2, tuples)

  }
}
