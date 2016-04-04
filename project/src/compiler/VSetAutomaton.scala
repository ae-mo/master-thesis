package compiler

import scala.collection.mutable.Map

/**
 * Represents a vset-automaton.
*/

class VSetAutomaton(val nrStates: Int, val initial: Int, val transitionFunction:Map[Int, Map[Int, String]], val finalStates: Array[Int]) {
  
/*  def project(vars: Array[Int]): VSetAutomaton {}
  
  def join(other: VSetAutomaton): VSetAutomaton {}
  
  def union(other: VSetAutomaton): VSetAutomaton {}
  
  def toVSetPathUntion(): Map[Int, Map[Int, String]] {
    
    
    
  }
  */
  /**
   *Eliminates all the states that don't have incoming transitions with span open/close operations. 
   */
  def stateElimination(): Map[Int, Map[Int, String]] = {
    
    import scala.collection.mutable.ArrayBuffer
     
    var transitionGraph = this.transitionFunction
    var initial = this.initial
    var nrStates = this.nrStates
    var finalStates = this.finalStates
    
    // normalize the automaton
    val(a, b, c, d) = normalize(nrStates, initial, transitionGraph, finalStates)
    
    nrStates = a
    initial = b
    transitionGraph = c
    finalStates = d
    
    var statesToProcess = nrStates - initial - finalStates.size
    
    var oldTransitionGraph = transitionGraph
    var newTransitionGraph: Map[Int, Map[Int, String]] = null
    
    // eliminate states
    while(statesToProcess > 0) {
      newTransitionGraph = Map[Int, Map[Int, String]]()
      // compute the weights for each state
      val weights = computeWeights(nrStates, initial, oldTransitionGraph, finalStates)
      
      val q = weights.zipWithIndex.min._2
      
      // Compute the label of each new arc after elimination of q
      for((s1, t1) <- oldTransitionGraph) {
       for((s2, t2) <- oldTransitionGraph){
         
         if(t1.contains(q) && oldTransitionGraph(q).contains(s2)) {
          
          var alphas1s2 = ""
          var alphaqq = ""
          
          if(oldTransitionGraph(s1).contains(s2)) {
            
            alphas1s2 = "(" + oldTransitionGraph(s1)(s2) + ")"
            
          }
            
            
          if(oldTransitionGraph(s1).contains(s1))
            alphaqq = "(" + oldTransitionGraph(s1)(s1) + ")"
            
          val alphas1q = "(" + t1(q) + ")"
          val alphaqs2 = "(" + oldTransitionGraph(q)(s2) + ")"
          
          if(!newTransitionGraph.contains(s1))
           newTransitionGraph = newTransitionGraph + (s1 -> Map[Int, String]())
          
          var newTransition = ""
          if(alphas1s2 != "")
            newTransition = newTransition + alphas1s2 + "|"
            
          newTransition = newTransition + "(" + alphas1q
          
          if(alphaqq != "")
            newTransition = newTransition + alphaqq + "*"
          
          newTransition = newTransition + alphaqs2 + ")"
          
          
          
          newTransitionGraph(s1) = newTransitionGraph(s1) + (s2 -> newTransition)
        }
        else if(!t1.contains(q)) {
          
          newTransitionGraph = newTransitionGraph + (s1 -> t1)
          
        }
        
       }        
        
      }
      
      oldTransitionGraph = newTransitionGraph
      statesToProcess = statesToProcess - 1
      
    }
    
    newTransitionGraph
    
  } 
  
  /**
   * Normalizes a transition graph.
   * @return (nrStates, initial, transitionGraph, finalStates) the updated versions of the input parameters
   */
  def normalize(n: Int, i: Int, tr:Map[Int, Map[Int, String]], f: Array[Int]): 
    (Int, Int, Map[Int, Map[Int, String]], Array[Int]) = {
    
    import scala.util.control.Breaks._
    
    var transitionGraph = tr
    var initial = i
    var nrStates = n
    var finalStates = f
    
    // initial state without incoming transitions
    for((q, t) <- transitionFunction) {
      
      if(q != 0) {
        
        if(t.contains(0)) {
         
          nrStates = nrStates +1
          initial = nrStates
          transitionGraph = transitionGraph + (nrStates ->  Map[Int, String]())
          transitionGraph(nrStates) = transitionGraph(nrStates) + (0 -> "")
          
          break
          
        }
          
      }
      
    }
    
    // unique final state without outgoing transitions
    if(this.finalStates.length > 1 || transitionFunction.contains(this.finalStates(0))) {
      
      nrStates = nrStates +1  
      
      for(q <- this.finalStates) {
        
        if(!transitionGraph.contains(q))
         transitionGraph = transitionGraph + (q -> Map[Int, String]())
      
        transitionGraph(q) = transitionGraph(q) + (nrStates -> "")
      }
      
      finalStates = new Array[Int](1)
      finalStates(0) = nrStates
      
    }
    
    (nrStates, initial, transitionGraph, finalStates)
    
  }
  
  /**
   * Computes the weights of each state according to Delgado and Morais Heuristics.
   * @return an array of weights for each state
   */
  def computeWeights(n: Int, i: Int, tr:Map[Int, Map[Int, String]], f: Array[Int]): Array[Int] = {
    
    var transitionGraph = tr
    var initial = i
    var nrStates = n
    var finalStates = f
    
    var weights = new Array[Int](nrStates)
    
    for(i <- 0 until weights.length if(i != initial && !(finalStates contains i))) {
      
      var l:Int = 0
      var m:Int = 0
      var lengthIn:Int = 0
      var lengthOut:Int = 0
      var lengthSelf:Int = 0
      
      // compute lengthOut, l
      if(transitionGraph.contains(i)) {
        
        l = transitionGraph(i).size
        
        for((q,t) <- transitionGraph(i) if(q != i)) {
          
          lengthOut = lengthOut + findExpressionLength(t)
          
        }
        
        // compute LengthSelf
        if(transitionGraph(i).contains(i))
          lengthSelf = findExpressionLength(transitionGraph(i)(i))
          
        // compute LengthIn, m
        for((q, t) <- transitionGraph) {
          
          if(t.contains(i)) {
            
            m = m + 1
            lengthIn = lengthIn + findExpressionLength(t(i))
            
          }
          
        }
        
        weights(i) = (l-1)*lengthIn + (m-1)*lengthOut + (m*l - 1)*lengthSelf
          
      }
      else weights(i) = Int.MaxValue
      
    }
   
    
    // set weights of initial and final states to special value
    weights(initial) = Int.MaxValue
    
    for(f <- 0 until finalStates.length)
      weights(finalStates(f)) = Int.MaxValue
      
    weights
    
  }
  
  /**
   * Finds the number of symbols in a regular expression
   * @return the number of symbols in the expression
   */
  def findExpressionLength(expr: String): Int = {
    
    var cleaned = expr.replaceAll("\\\\", "")
    cleaned = cleaned.replaceAll("(._in)|(._out)", "a")
    
    cleaned.length()
    
  }
  
  
}

