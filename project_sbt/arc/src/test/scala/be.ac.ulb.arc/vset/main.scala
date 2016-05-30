package be.ac.ulb.arc.vset

import scala.collection.mutable.ArrayBuffer
import be.ac.ulb.arc.runtimeold.Instruction
import be.ac.ulb.arc.runtimeold.InstructionType

import dk.brics.automaton.RegExp

object main{

	val sampleVSet = "src/test/scala/be.ac.ulb.arc/compiler/sampleVSet6.txt"
	val sampleVSet2 = "src/test/scala/be.ac.ulb.arc/compiler/sampleVSet8.txt"

	def main(args: Array[String]) {

	val test = new RegExp(".").toAutomaton()

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
