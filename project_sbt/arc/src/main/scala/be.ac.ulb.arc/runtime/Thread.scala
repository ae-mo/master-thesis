package be.ac.ulb.arc.runtime

import scala.{Int => Position}
import scala.collection.immutable.{HashSet => SVars}
import scala.{Int => SVar}

/**
  * Represents collection of string pointers.
  *
  * @param vars
  */
abstract class StringPointerCollection(val vars:SVars[SVar]) {

  def apply(i: Int):Position
  def update(i:Int, p:Position)
  def copy:StringPointerCollection
  def toArray:Array[Position]
}

/**
  * Represents an array of string pointers.
  *
  * @param vars
  */
class StringPointerArray(override val vars:SVars[SVar]) extends StringPointerCollection(vars) { self =>

  val sortedVars = vars.toArray.sortWith(_<_)
  val ptrs = Array.fill[Position](vars.size*2)(-1)

  def apply(i: Int):Position = {

    ptrs(getIndex(i))
  }
  def update(i:Int, p:Position) = {

    ptrs(getIndex(i)) = p
  }

  def copy:StringPointerCollection = new {

    override val ptrs = self.ptrs.clone
  } with StringPointerArray(vars)

  def toArray:Array[Position] = ptrs

  def getIndex(i:Int):Int = {

    val i2:Int = i/2
    val rem = if (i % 2 == 0) 0 else 1
    sortedVars.indexOf(i2)*2+rem
  }

  def canEqual(a: Any) = a.isInstanceOf[StringPointerArray]

  override def equals(that: Any): Boolean = {

    that match {

      case that: StringPointerArray => {

        that.canEqual(this) && this.vars == that.vars && this.ptrs.deep == that.ptrs.deep
      }
    }
  }

  override def hashCode: Int = {

    val prime = 31
    var result = 1

    for(v <- sortedVars) {

      result = prime*result + v
    }

    for(p <- ptrs) {

      result = prime*result + p
    }

    return result
  }
}


/*
class StringPointerTree(override val vars:SVars[SVar]) extends StringPointerCollection(vars) { self =>

  val ptrs = mutable.Map[Int, PointerNode]()

  def apply(i: Int):Position = {

    if(ptrs.contains(i))
      ptrs(i).p
    else
      -1
  }

  def update(i:Int, p:Position) = {

    if(!ptrs.contains(i))
      ptrs += ((i, new PointerNode(p)))
    else ptrs(i).p = p
  }

  def copy:StringPointerCollection = new {

    override val ptrs = {

       val newPtrs = mutable.Map[Int, PointerNode]()

      for((i, p) <- self.ptrs)
        newPtrs += ((i, p))

      newPtrs
    }

  } with StringPointerTree(vars)

  def toArray:Array[Position] = {

    val res = new Array[Position](vars.size*2)

    for(i <- 0 until vars.size) {

      if(ptrs.contains(i))
        res(i) = ptrs(i).p
      else
        res(i) = -1
    }

    res
  }

}

/**
  * Represents a node in the StringPointerTree class.
  * @param p
  */
class PointerNode(var p:Position)
*/


class Thread(val instr:Instruction, val saved:StringPointerCollection)