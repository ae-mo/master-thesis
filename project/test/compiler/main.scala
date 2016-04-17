package compiler

import dk.brics.automaton.RegExp

object main{
  
    val sampleVSet = "test/compiler/sampleVSet6.txt"
  
    def main(args: Array[String]) {
      
      val (nrStates, initial, transitionFunction, vars, finalStates) = VSetAFileReader.getVSetA(sampleVSet)
      
      val a = new VSetAutomaton(nrStates, initial, transitionFunction, vars, finalStates)
      
      val (pathUnion, finalStates2) = a.toVSetPathUnion
      
      val hPU = a.toHybridPathUnion(pathUnion)
      
      val lPU = a.toLexicographicPathUnion(hPU)

    }
    
}