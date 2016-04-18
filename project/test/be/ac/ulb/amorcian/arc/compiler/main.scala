package be.ac.ulb.amorcian.arc.compiler

import dk.brics.automaton.RegExp

object main{
  
    val sampleVSet = "test/be/ac/ulb/amorcian/arc/compiler/sampleVSet6.txt"
  
    def main(args: Array[String]) {
      
      val (nrStates, initial, transitionFunction, vars, finalStates) = VSetAFileReader.getVSetA(sampleVSet)
      
      val a = new VSetAutomaton(nrStates, initial, transitionFunction, vars, finalStates)
      
      val pU = a.toVSetPathUnion
      
      val hPU = pU.toHybridPathUnion
      
      val lPU = hPU.toLexicographicPathUnion
      
      val a1 = lPU.toVSetAutomaton()

    }
    
}