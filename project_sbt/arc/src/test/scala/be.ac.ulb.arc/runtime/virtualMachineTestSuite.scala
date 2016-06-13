package be.ac.ulb.arc.runtime

import org.scalatest.FunSuite

import scala.collection.immutable.{HashSet => SVars}
import scala.{Int => SVar}
import scala.collection.immutable.{HashSet => VSRelation}
import scala.{Array => Program}
import be.ac.ulb.arc.runtime.{StringPointerCollection => VSTuple}

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

    val str2 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + '\0'

  }

  test("VM should match prog1 on str1") {

    val vars = new SVars[SVar] + 0 + 1

    val tuplesOpt = VirtualMachine.execute(data.prog1, vars, Set[(SVar, SVar)](), data.str1, 0, VirtualMachine.processSAVE)

    assert(tuplesOpt != None)

    val tuples = tuplesOpt.get

    assert(tuples.size == 1)
    assert(tuples.exists((p: VSTuple) => p(0) == 0 && p(1) == 5 && p(2) == 5 && p(3) == 6))

    OutputWriter.printOutput(data.str1, tuples)
  }

  test("VM should match prog2 on str2") {

    val vars = new SVars[SVar] + 0 + 1

    val tuplesOpt = VirtualMachine.execute(data.prog2, vars, Set[(SVar, SVar)](), data.str2, 0, VirtualMachine.processSAVE)

    assert(tuplesOpt != None)

    val tuples = tuplesOpt.get

    OutputWriter.printOutput(data.str2, tuples)

  }

  test("VM should match prog2 on str2 with var 1 equal to var 2") {

    val vars = new SVars[SVar] + 0 + 1
    var eqs = Set[(SVar, SVar)]()

    eqs = eqs + ((0, 1))

    val tuplesOpt = VirtualMachine.execute(data.prog2, vars, eqs, data.str2, 0, VirtualMachine.processSAVEwithEq)

    assert(tuplesOpt != None)

    val tuples = tuplesOpt.get

    OutputWriter.printOutput(data.str2, tuples)

  }

 /* test("VM should match prog2 on str2 with var 1 equal to var 2, with tuples backed by a shared representation") {

    val vars = new SVars[SVar] + 0 + 1
    val eqs = new Array[(SVar, SVar)](1)

    eqs(0) = ((0, 1))

    val tuplesOpt = VirtualMachine.execute(data.prog2, vars, eqs, data.str2, 1, VirtualMachine.processSAVEwithEq)

    assert(tuplesOpt != None)

    val tuples = tuplesOpt.get

    OutputWriter.printOutput(data.str2, tuples)

  }*/

  test("String pointer arrays should be equal when they have the same variables and values") {

    val s1 = new SVars[SVar] + 0 + 1
    val s2 = new SVars[SVar] + 0 + 1
    val a = new StringPointerArray(s1)
    val b = new StringPointerArray(s2)

    assert(a==b)
    assert(a.hashCode == b.hashCode)

    var ts = new VSRelation[VSTuple] + a + b

    assert(ts.size == 1)
  }
}
