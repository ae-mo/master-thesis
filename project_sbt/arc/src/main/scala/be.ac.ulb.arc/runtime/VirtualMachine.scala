package be.ac.ulb.arc.runtime

import scala.collection.immutable.{HashSet => SVars}
import scala.{Int => SVar}
import scala.{Int => Position}
import scala.{Array => Pointers}
import scala.{Array => Program}
import scala.collection.immutable.{HashSet => VSRelation}
import scala.{Array => VSTuple}
import scala.collection.mutable.{MutableList => ThreadList}

/**
  * Represents a virtual machine capable of executing NFA programs.
  */
object VirtualMachine {

  /**
    * Executes a NFA program on the provided strings and outputs the spanned tuples.
    * @param prog
    * @param V
    * @param input
    * @return
    */
  def execute(prog:Program[Instruction], V:SVars[SVar], input:String):Option[VSRelation[VSTuple[Position]]] = {

    // The set of output tuples
    var tuples = new VSRelation[VSTuple[Position]]

    val len = prog.length

    // The lists of threads
    var cList = new ThreadList[Thread]()
    var nList = new ThreadList[Thread]()

    var matched = false

    // Add the first thread to the current list
    addThread(cList, prog(0), new Pointers[Position](V.size * 2))

    var sp = 0

    // Execute the program by advancing threads in lockstep
    for(sp <- 0 until input.length) {

      var i = 0

      // Advance the threads
      while(i < cList.length) {

        val t = cList(i)

        t.instr match {

          case CHAR(c, pos) => {

            if(input.charAt(sp) == c)
              addThread(nList, prog(pos + 1), t.saved)
          }
          case RANGE(r, pos) => {

            if(input.charAt(sp) >= r.min && input.charAt(sp) <= r.max)
              addThread(nList, prog(pos + 1), t.saved)
          }
          case MATCH(pos) => {

            // Add the tuple spanned by this thread to the output
            tuples = tuples + t.saved
            matched = true
          }
          case JUMP(pos, target) => {

            addThread(cList, target, t.saved)

          }
          case SPLIT(pos, next1, next2) => {

            addThread(cList, next1, t.saved)
            addThread(cList, next2, t.saved)
          }
          case SAVE(ptr, pos) => {

            t.saved(ptr) = sp
            addThread(cList, prog(pos + 1), t.saved)
          }

        }

        i += 1
      }

      cList = nList
      nList = new ThreadList[Thread]()

    }

    if(matched) Some(tuples)
    else None
  }

  /**
    * Adds a thread to the provided thread list.
    * @param l
    * @param instr
    * @param saved
    */
  def addThread(l:ThreadList[Thread], instr:Instruction, saved:Pointers[Position]):Unit = {

      l += new Thread(instr, saved.clone())

  }
}