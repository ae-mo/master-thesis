package compiler

import scala.collection.mutable.Map

/**
 * Represents a reader of files containing vset-automaton encodings.
 */
object VSetAFileReader {
  
  /**
   * Returns the vset-automaton contained in the given file.
   * @param file the file name
   */
  def getVSetA(file: String): (Int, Int, Map[Int, Map[Int, String]], Array[String], Array[Int]) =  {
    
      import scala.io.Source
      
      val source = Source.fromFile(file, "UTF-8")
      val lineIterator = source.getLines
      
       
      val nrStatesStr = lineIterator.next()
      val nrStates = nrStatesStr.toInt
      
      val initialStr = lineIterator.next()
      val initial = initialStr.toInt
      
      val finalStatesStr = lineIterator.next()
      val finalStatesTok = finalStatesStr.split("\\s+") 
      val finalStates = finalStatesTok.map(_.toInt)
      
      val varsStr = lineIterator.next()
      val varsTok = varsStr.split("\\s+") 
      
      val transitionPattern = "(\\d+)\\s(.+)\\s(\\d+)".r
      
      var transitionFunction = Map[Int, Map[Int, String]]()
      
      for(l <- lineIterator) {
        
        val transitionPattern(state1, label, state2) = l
        
        if(!transitionFunction.contains(state1.toInt))
         transitionFunction = transitionFunction + (state1.toInt -> Map[Int, String]())
          
        if(transitionFunction(state1.toInt).contains(state2.toInt))
          transitionFunction(state1.toInt)(state2.toInt) += "|" + label 
        else
          transitionFunction(state1.toInt) = transitionFunction(state1.toInt) + (state2.toInt -> label)
        
      }
      
      (nrStates, initial, transitionFunction, varsTok, finalStates)
  }
  
  
}