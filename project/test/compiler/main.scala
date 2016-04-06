package compiler

object main {
  
    val sampleVSet1 = "test/compiler/sampleVSet1.txt"
    
    def main(args: Array[String]) {
    
      val (nrStates, initial, transitionFunction,vars, finalStates) = VSetAFileReader.getVSetA(sampleVSet1)
      
      val a = new VSetAutomaton(nrStates, initial, transitionFunction, vars, finalStates)
    
      a.toVSetPathUnion()
    
    }
    
  
}