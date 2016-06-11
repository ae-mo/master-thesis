package be.ac.ulb.arc.runtime

import scala.collection.mutable
import scala.{Int => Position}

/**
  * Represents collection of string pointers.
  * @param size
  */
abstract class StringPointerCollection(val size:Int) {

  def apply(i: Int):Position
  def update(i:Int, p:Position)
  def copy:StringPointerCollection
  def toArray:Array[Position]
}

/**
  * Represents an array of string pointers.
  * @param size
  */
class StringPointerArray(override val size:Int) extends StringPointerCollection(size) { self =>

  val ptrs = Array.fill[Position](size)(-1)

  def apply(i: Int):Position = ptrs(i)
  def update(i:Int, p:Position) = ptrs(i) = p

  def copy:StringPointerCollection = new {

    override val ptrs = self.ptrs.clone
  } with StringPointerArray(size)

  def toArray:Array[Position] = ptrs
 }

/**
  * Represents a tree of string pointers.
  * @param size
  */
class StringPointerTree(override val size:Int) extends StringPointerCollection(size) { self =>

  val ptrs = mutable.Map[Int, PointerNode]()

  def apply(i: Int):Position = {

    checkBounds(i)
    if(ptrs.contains(i))
      ptrs(i).p
    else
      -1
  }

  def update(i:Int, p:Position) = {

    checkBounds(i)
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

  } with StringPointerTree(size)

  def toArray:Array[Position] = {

    val res = new Array[Position](size)

    for(i <- 0 until size) {

      if(ptrs.contains(i))
        res(i) = ptrs(i).p
      else
        res(i) = -1
    }

    res
  }

  def checkBounds(i:Int): Unit = {

    if(i < 0 || i >= size) throw new IndexOutOfBoundsException("Invalid index: " + i)
  }
}

/**
  * Represents a node in the StringPointerTree class.
  * @param p
  */
class PointerNode(var p:Position)


class Thread(val instr:Instruction, val saved:StringPointerCollection)