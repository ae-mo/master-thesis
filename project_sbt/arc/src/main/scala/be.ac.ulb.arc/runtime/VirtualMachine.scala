package be.ac.ulb.arc.runtime

import scala.collection.immutable.{HashSet => SVars}
import scala.{Int => SVar}
import scala.{Int => Position}
import scala.{Array => Program}
import scala.collection.immutable.{HashSet => VSRelation}
import be.ac.ulb.arc.runtime.{StringPointerCollection => VSTuple}
import scala.collection.mutable.{MutableList => ThreadList}

/**
  * Represents a virtual machine capable of executing NFA programs.
  */
object VirtualMachine {

  /**
    * Executes a NFA program on the provided strings and outputs the spanned tuples.
    * @param prog
    * @param V
    * @param equalities
    * @param input
    * @param mode
    * @param processSAVE
    * @return
    */
  def execute(prog:Program[Instruction], V:SVars[SVar], equalities:Array[(SVar, SVar)], input:String, mode:Int,
              processSAVE:(Program[Instruction], ThreadList[Thread], Array[(SVar, SVar)], String, Position, Thread, Int, Int) => Unit)
  :Option[VSRelation[VSTuple]] = {


    // The set of output tuples
    var tuples = new VSRelation[VSTuple]

    val len = prog.length

    // The lists of threads
    var cList = new ThreadList[Thread]()
    var nList = new ThreadList[Thread]()

    var matched = false


    // Add the first thread to the current list
    addThread(cList, prog(0), new StringPointerArray(V))

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

            processSAVE(prog, cList, equalities, input, pos, t, ptr, sp)
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
  def addThread(l:ThreadList[Thread], instr:Instruction, saved:StringPointerCollection):Unit = {

      l += new Thread(instr, saved.copy)

  }

  def processSAVEwithEq (prog:Program[Instruction], l:ThreadList[Thread], eqs:Array[(SVar, SVar)], in:String, p:Position, t:Thread, ptr:Int, sp:Int):Unit = {

    // If a span is being closed
    if(ptr % 2 != 0) {

      // corresponding span variable
      val v1 = (ptr-1)/2

      // check it satisfies all string equality selections,
      // when the other variables involved have already been assigned a span
      for(eq <- eqs) {

        // get the other variable
        val v2:SVar = if(v1 == eq._1) eq._1 else if(v1 == eq._2) eq._1 else -1

        if(v2 != -1 && t.saved(v2*2 + 1) != -1) {

          // get corresponding span
          val sp2 = (t.saved(v2*2), t.saved(v2*2 + 1))

          val str1 = in.substring(t.saved(ptr-1), sp)
          val str2 = in.substring(sp2._1, sp2._2)

          // If an equality is not satisfied, the thread dies
          if(str1 != str2) return
        }

      }
    }

    t.saved(ptr) = sp
    addThread(l, prog(p + 1), t.saved)
  }

  def processSAVE(prog:Program[Instruction], l:ThreadList[Thread], eqs:Array[(SVar, SVar)], in:String, p:Position, t:Thread, ptr:Int, sp:Int): Unit = {

    t.saved(ptr) = sp
    addThread(l, prog(p + 1), t.saved)
  }
}