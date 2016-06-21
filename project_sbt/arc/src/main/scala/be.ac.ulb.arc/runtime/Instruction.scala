package be.ac.ulb.arc.runtime

import scala.{Int => Position}
import scala.{Int => Pointer}
import be.ac.ulb.arc.core.Range

// A generic instruction
abstract class Instruction(val pos:Position)
// Matches a char
case class CHAR(val c:Char, override val pos:Position) extends Instruction(pos)
// Matches a char in a range
case class RANGE(val r:Range, override val pos:Position) extends Instruction(pos)
// Splits the thread of control
case class SPLIT(override val pos:Position, var next1:Position, var next2:Position) extends Instruction(pos)
// Jumps to target instruction
case class JUMP(override val pos:Position, var target:Position) extends Instruction(pos)
// Saves the current string pointer to the specified pointer
case class SAVE(val ptr:Pointer, override val pos:Position) extends Instruction(pos)
// Matches the input
case class MATCH(override val pos:Position) extends Instruction(pos)