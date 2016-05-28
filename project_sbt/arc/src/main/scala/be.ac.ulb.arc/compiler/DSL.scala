/*
package be.ac.ulb.arc.compiler

import scala.collection.immutable.{HashSet => SVars}
import scala.{Int => SVar}
import scala.{Int => Position}
import scala.{Array => Pointers}
import scala.{Array => Program}
import scala.collection.mutable.{ArrayBuffer, Set => VSRelation}
import scala.collection.mutable.Set
import scala.collection.mutable.{MutableList => ThreadList}
import scala.{Array => VSTuple}

import scala.lms.common._
import scala.lms.tutorial._

class Instruction(val typ:Int, val instrData:Array[Int])

trait ThreadBase extends Base {

  class Thread

  def Thread(instr:Rep[Int], instrData:Rep[Array[Int]], saved:Rep[Pointers[Position]]):Rep[Thread]

  def infix_instr(t:Rep[Thread]):Rep[Int]
  def infix_instrData(t:Rep[Thread]):Rep[Array[Int]]
  def infix_saved(t:Rep[Thread]):Rep[Array[Int]]
}

trait ThreadOpsExp extends ThreadBase with StructExp{

  def Thread(instr:Rep[Int], instrData:Rep[Array[Int]], saved:Rep[Pointers[Position]]) = struct[Thread](classTag[Thread], ("instr", instr), ("instrData", instrData), ("saved", saved))

  def infix_instr(t:Rep[Thread]):Rep[Int] = field[Int](t, "instr")
  def infix_instrData(t:Rep[Thread]):Rep[Array[Int]] = field[Array[Int]](t, "instrData")
  def infix_saved(t:Rep[Thread]):Rep[Array[Int]] = field[Array[Int]](t, "saved")

}

trait VirtualMachineOps extends Dsl with ArrayBufferOps with SetOps with ListOps with TupleOps with ThreadBase{


  /**
    * Executes a NFA program on the provided strings and outputs the spanned tuples.
    *
    * @param prog
    * @param V
    * @param input
    * @return
    */
  def executeNFAProgram(prog:Program[Instruction], V:SVars[SVar], input:Rep[String]):Rep[VSRelation[VSTuple[Position]]] = {


    val tuples:Rep[VSRelation[VSTuple[Position]]] = Set[VSTuple[Position]]()

    var cList:Rep[ArrayBuffer[Thread]] = ArrayBuffer[Thread]()
    var nList:Rep[ArrayBuffer[Thread]] = ArrayBuffer[Thread]()

    arraybuffer_append[Thread](cList, Thread(prog(0).typ, copyArray(prog(0).instrData), array_obj_new[Int](V.size * 2)))

    var sp:Int = 0

    for(sp <- 0 until input.length) {
      var i:Int = 0

      // Advance the threads
      while(i < cList.length) {

        val t:Rep[Thread] = cList(i)

        if(t.instr == 0) {

          if(input(sp) == infix_instrData(t)(0))
            arraybuffer_append[Thread](nList, Thread(prog(infix_instrData(t)(1) + 1), copyArray(prog(infix_instrData(t)(1) + 1).instrData), array_obj_new[Int](V.size * 2)))

        }

      }

      cList = nList
      nList = ArrayBuffer[Thread]()

    }

    tuples

  }

  def copyArray(in:Array[Int]):Rep[Array[Int]] = {

    val out:Rep[Array[Int]]= array_obj_new[Int](in.size)

    for(i <- 0 until in.size)
      out(i) = in(i)

    out
  }

}

trait DSLOpsExp extends DslExp with SetOpsExp with ListOpsExp

trait ScalaGenDSLOps extends DslGen with ScalaGenSetOps with ScalaGenListOps {

  val IR: DSLOpsExp
  import IR._
}

abstract class DSLDriver extends DslDriver[String,VSRelation[VSTuple[Position]]] with DSLOpsExp {
  q =>
  override val codegen = new ScalaGenDSLOps {
    val IR: q.type = q
  }
}
*/
