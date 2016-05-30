package be.ac.ulb.arc.runtimeold

import scala.collection.mutable.MutableList

package object runtime {
  
  var matched = false
  
}

/**
 * Represents a virtual machine for regular expression programs execution.
 */
object vm {

	/**
	 * Executes a regular expression program with a given input.
	 * @param prog the program to execute
	 * @param input the input for the program
	 */
	def evaluate(prog: Array[Instruction], input: Array[Char]):Array[Int] =  {

			val len = prog.length
			var cList = new MutableList[Thread]()
			var nList = MutableList[Thread]()
			var present = new Array[Boolean](len)
			var saved:Array[Int] = null
			var matched = false

			addThread(cList, present, prog(0), 0, new Array[Int](20))

			var i = 0

			for(i <- 0 until input.length) {

				val sp = input(i)
				var j = 0

				var stop = false
				while(j < cList.length) {

					var t = cList(j)

					val pc = t.pc

					pc.opCode match {

						case InstructionType.CHAR => {

							if(sp == pc.c)
								addThread(nList, present, prog(pc.num + 1), i+1, t.saved)
						}
						
						case InstructionType.DIGIT => {

						  if(sp >= '0' && sp <= '9')
								addThread(nList, present, prog(pc.num + 1), i+1, t.saved)
						}
						
						case InstructionType.WHITESPACE => {

						  if(sp == '\t' || sp == '\r' || sp == '\n' || sp == '\f' || sp == ' ')
								addThread(nList, present, prog(pc.num + 1), i+1, t.saved)
						}
						
						case InstructionType.DOT => {
						  
						  if(sp != '\0' && sp != '\n')
								addThread(nList, present, prog(pc.num + 1), i+1, t.saved)
						}

						case InstructionType.MATCH => {
						  if(i == input.length - 1) {
						    
						    saved = t.saved.clone()
							  matched = true
							  j = cList.length
						    
						  }
							
						}

						case _ => {

							addThread(nList, present, prog(pc.num), i, t.saved)

						}

					}

					j = j+1

				}

				cList = nList
				nList = MutableList[Thread]()
				present = new Array[Boolean](len)

			}
			
			runtime.matched = matched
			saved

	}


	/**
	 * Adds a thread to a given thread stack, only if a thread with the same program counter doesn't exist.
	 * @param l the stack (threads ordered by priority)
	 * @param p an array keeping track of the unique threads already present in l
	 * @param instr the next instruction
	 * @param i the current input character
	 * @param saved the current array of saved string pointers
	 */
	def addThread(l:MutableList[Thread], p:Array[Boolean],  instr:Instruction, i:Int, saved:Array[Int]) {

		instr.opCode match {

		case InstructionType.JMP => {

			addThread(l, p, instr.x, i, saved)

		}

		case InstructionType.SPLIT => {

			addThread(l, p, instr.x, i, saved)
			addThread(l, p, instr.y, i, saved)

		}

		case InstructionType.SAVE => {

			saved(instr.i) = i
			addThread(l, p, instr.x, i, saved)

		}

		case _ => {

		  if(!p(instr.num))
				l += new Thread(instr, saved.clone) ;
		}

		}


	}

}