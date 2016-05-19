package be.ac.ulb.arc.runtime

import scala.{Int => Position}
import scala.{Array => Pointers}

class Thread(val instr:Instruction, val saved:Pointers[Position])