package be.ac.ulb.amorcian.arc.compiler

import dk.brics.automaton.RegExp
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import be.ac.ulb.amorcian.arc.runtime.Instruction
import be.ac.ulb.amorcian.arc.runtime.InstructionType

object main{
  
    val sampleVSet = "test/be/ac/ulb/amorcian/arc/compiler/sampleVSet6.txt"
    val sampleVSet2 = "test/be/ac/ulb/amorcian/arc/compiler/sampleVSet7.txt"
  
    def main(args: Array[String]) {
      
      val (nrStates, initial, transitionFunction, vars, finalStates) = VSetAFileReader.getVSetA(sampleVSet)
      
      val a = new VSetAutomaton(nrStates, initial, transitionFunction, vars, finalStates)

      val (nrStates2, initial2, transitionFunction2, vars2, finalStates2) = VSetAFileReader.getVSetA(sampleVSet2)
      
      val a2 = new VSetAutomaton(nrStates2, initial2, transitionFunction2, vars2, finalStates2)

      val a3 = a.join(a2)
      
      val a4 = a3.toVSetPathUnion().toHybridPathUnion().toVSetAutomaton()
      
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