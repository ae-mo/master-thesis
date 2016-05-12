package be.ac.ulb.amorcian.arc.compiler

import scala.collection.mutable.ArrayBuffer
import be.ac.ulb.amorcian.arc.runtime.Instruction
import be.ac.ulb.amorcian.arc.runtime.InstructionType

object main{

    val sampleVSet = "src/test/scala/be.ac.ulb.arc/compiler/sampleVSet8.txt"
    val sampleVSet2 = "src/test/scala/be.ac.ulb.arc/compiler/sampleVSet7.txt"

    def main(args: Array[String]) {

      val (nrStates, initial, transitionFunction, vars, finalStates) = VSetAFileReader.getVSetA(sampleVSet)

      val a = new VSetAutomaton(nrStates, initial, transitionFunction, vars, finalStates)

      val b = a.epsilonClosure()

			b.prune()

      println("hey")

    }

    def printProgram(program: ArrayBuffer[Instruction]) = {

      for(ins <- program) {

        print(ins.num + " ")

        ins.opCode match {

          case InstructionType.CHAR => {

							println("CHAR" + " " + ins.c.toChar)
						}

						case InstructionType.DIGIT => {

						  println("DIGIT")
						}

						case InstructionType.WHITESPACE => {

						  println("WHITESPACE")
						}

						case InstructionType.DOT => {
						  println("DOT")
						}

						case InstructionType.MATCH => {
						  println("MATCH")

						}


						case InstructionType.JMP => {
						  println("JMP" + " " + program.indexOf(ins.x))

        		}

        		case InstructionType.SPLIT => {

        		  println("SPLIT" + " " + program.indexOf(ins.x) + " " + program.indexOf(ins.y))
        		}

        		case InstructionType.SAVE => {

        			println("SAVE" + " " + ins.i)
        		}

        }
      }
    }

}
