package be.ac.ulb.amorcian.arc.compiler

import dk.brics.automaton.RegExp
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import be.ac.ulb.amorcian.arc.runtime.Instruction
import be.ac.ulb.amorcian.arc.runtime.InstructionType

object main{
  
    val sampleVSet = "test/be/ac/ulb/amorcian/arc/compiler/sampleVSet6.txt"
  
    def main(args: Array[String]) {
      
      val (nrStates, initial, transitionFunction, vars, finalStates) = VSetAFileReader.getVSetA(sampleVSet)
      
      val a = new VSetAutomaton(nrStates, initial, transitionFunction, vars, finalStates)
      
      val pU = a.toVSetPathUnion
      
      val hPU = pU.toHybridPathUnion
      
      val lPU = hPU.toLexicographicPathUnion
      
      val a1 = lPU.toVSetAutomaton()
      
      var program = new ArrayBuffer[Instruction]()
      val pc = 0
      val s = 0
      var visitedStates = Map[Int, Int]()
      
      a1.toNFAProgram(program, visitedStates, s, pc)
      
      for(ins <- program) {
        
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