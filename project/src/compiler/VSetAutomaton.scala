package compiler

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

/**
 * Represents a vset-automaton.
*/

class VSetAutomaton(val nrStates: Int, val initial: Int, val transitionFunction:Map[Int, Map[Int, String]], val vars: Array[String], val finalStates: Array[Int]) {
  
/*  def project(vars: Array[Int]): VSetAutomaton {}
  
  def join(other: VSetAutomaton): VSetAutomaton {}
  
  def union(other: VSetAutomaton): VSetAutomaton {}
  
  
  */
  
  /*
   * Transforms the vset-automaton into a vset path union.
   */
  def toVSetPathUnion():(ArrayBuffer[ArrayBuffer[(String, Int)]], Array[Int]) = {
    
    val (nrStates, initial, transitionGraph, finalStates2) = stateElimination()
    
    var pathsToProcess = 1
    
    var availablePathVars = new ArrayBuffer[Map[String, Boolean]]()
    var pathsUnion = new ArrayBuffer[ArrayBuffer[(String, Int)]]()
    
    var firstPathVars = Map[String, Boolean]()
    
    for(v <- vars)
      firstPathVars += (v -> true)
      
    availablePathVars += firstPathVars
    
    pathsUnion += new ArrayBuffer[(String, Int)]()
    pathsUnion(0) += (("", initial))
    
    // Grow the paths iteratively
    while(pathsToProcess > 0) {
      
      var newPaths:ArrayBuffer[ArrayBuffer[(String, Int)]] = null
      var newPathVars:ArrayBuffer[Map[String, Boolean]] = null
      
      // Advance each path by one transition, spawning new paths if there are >1 possible branches
      for(i <- 0 until pathsUnion.length) {
        
        var path = pathsUnion(i)
        var pathVars = availablePathVars(i)
        
        newPaths =  new ArrayBuffer[ArrayBuffer[(String, Int)]]()
        newPathVars = new ArrayBuffer[Map[String, Boolean]]()
        
        val (e, s) = path.last
        
        // If we haven't reached a final state in the current path
        if(!finalStates2.contains(s)) {
          
          for((t, e1) <- transitionGraph(s)) {
            
            var discard = false
            var out = false
            var v:String = null
            
            // If a branch tries to open a variable, allow it only if it's still available and remove it
            // from available vars for that branch, otherwise discard branch
            if(e1.matches(".+._in.+")) {
              
              val openedVar = ".+(.)_in.+".r
              val openedVar(x) = e1
              v = x
              
              if(pathVars(x)) {
                out = true
              }
              
              else discard = true
              
            }     
            
            // If we didn't discard the new branch, make it a path
            if(!discard) {
             
              newPaths += path.clone
              newPaths.last += ((e1, t))
              
              newPathVars += pathVars.clone
              
              if(out)
                newPathVars.last(v) = false
             
              
            }
            
          }
          
          // Add the new paths to the path union
          path += newPaths(0).last
          pathsToProcess += newPaths.tail.size
          pathsUnion.appendAll(newPaths.tail)
          
          // Add the availlable variables for the new paths
          availablePathVars(i) = newPathVars(0)
          availablePathVars.appendAll(newPathVars.tail)

          
        }
        else
          pathsToProcess -=1
        
      }
      
    }
    
    (pathsUnion, finalStates2)
    
  }
  /**
   *Eliminates all the states that don't have incoming transitions with span open/close operations. 
   */
  def stateElimination(): (Int, Int, Map[Int, Map[Int, String]], Array[Int]) = {
    
    import scala.collection.mutable.ArrayBuffer
     
    var newTransitionGraph = this.transitionFunction.clone
    var initial = this.initial
    var nrStates = this.nrStates
    var finalStates = this.finalStates
    
    // normalize the automaton
    val(a, b, c, d) = normalize(nrStates, initial, newTransitionGraph, finalStates)
    
    nrStates = a
    initial = b
    newTransitionGraph = c
    finalStates = d
    
    var oldTransitionGraph: Map[Int, Map[Int, String]] = null
    
    // compute the weights for each state
    var weights = computeWeights(nrStates, initial, newTransitionGraph, finalStates)
    
    var statesToProcess = nrStates -  weights.count(_ == Int.MaxValue)
    
    // eliminate states
    while(statesToProcess > 0) {
      
     oldTransitionGraph = newTransitionGraph.clone
      
     val q = weights.zipWithIndex.min._2
        
      // Compute the label of each new arc after elimination of q
      for((s1, t1) <- oldTransitionGraph if (t1.contains(q) && s1 != q)) {
       for((s2, t2) <- oldTransitionGraph(q) if (s2 != q)){
         
         println()

          
        var alphas1s2 = ""
        var alphaqq = ""
        
        // check if there's an arc between s1, s2
        if(t1.contains(s2)) {
          
          alphas1s2 = "(" + oldTransitionGraph(s1)(s2) + ")"
          
        }
          
        // Check if q has an arc to itself
        if(oldTransitionGraph(q).contains(q)) {
          
          alphaqq = "(" + oldTransitionGraph(q)(q) + ")"
        }
        
        
        val alphas1q = "(" + t1(q) + ")"
        val alphaqs2 = "(" + oldTransitionGraph(q)(s2) + ")"
        
        // compose the new transition between s1, s2
        var newTransition = ""
        if(alphas1s2 != "")
          newTransition = newTransition + alphas1s2 + "|"
          
        newTransition = newTransition + "(" + alphas1q
        
        if(alphaqq != "")
          newTransition = newTransition + alphaqq + "*"
        
        newTransition = newTransition + alphaqs2 + ")"
        
        // add new transition to s2
        newTransitionGraph(s1) += (s2 -> newTransition)

       }
       
       // eliminate old transitions
        newTransitionGraph(s1) -= q
        
      }
      
      newTransitionGraph -= q
      
      statesToProcess = statesToProcess - 1
      weights = computeWeights(nrStates, initial, newTransitionGraph, finalStates)
      
    }
    
    (nrStates, initial, newTransitionGraph, finalStates)
    
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
    breakable { 
      
      for((q, t) <- transitionFunction) {
      
        if(q != 0) {
          
          if(t.contains(0)) {
           
            initial = nrStates
            transitionGraph = transitionGraph + (nrStates ->  Map[Int, String]())
            transitionGraph(nrStates) = transitionGraph(nrStates) + (0 -> "epsilon")
            nrStates = nrStates +1
            
            break
            
          }
            
        }
        
      }
      
    }
    
    
    // unique final state without outgoing transitions
    if(this.finalStates.length > 1 || transitionFunction.contains(this.finalStates(0))) {
       
      
      for(q <- this.finalStates) {
        
        if(!transitionGraph.contains(q))
         transitionGraph = transitionGraph + (q -> Map[Int, String]())
      
        transitionGraph(q) = transitionGraph(q) + (nrStates -> "epsilon")
      }
      
      finalStates = new Array[Int](1)
      finalStates(0) = nrStates
      nrStates = nrStates +1 
      
    }
    
    (nrStates, initial, transitionGraph, finalStates)
    
  }
  
  /**
   * Computes the weights of each state according to Delgado and Morais Heuristics.
   * @return an array of weights for each state
   */
  def computeWeights(n: Int, i: Int, tr:Map[Int, Map[Int, String]], f: Array[Int]): Array[Int] = {
    
    import scala.util.control.Breaks._
    
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
      
      var setOp = false
      
      // check if state i has an incoming transition with a set operation
      for((s1, t1) <- transitionGraph) {
        
//        println(t1.contains(i))
//        if(t1.contains(i)) {
//          println(t1(i))
//          println(t1(i).matches("(._in)|(._out)"))
//        }
//        
        
        if(t1.contains(i) && t1(i).matches("(._in)|(._out)")) {
          
          setOp = true
          weights(i) = Int.MaxValue

        }
        
      }
      // compute lengthOut, l
      if(transitionGraph.contains(i) && !setOp) {
        
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
    cleaned = cleaned.replaceAll("epsilon", "a")
    
    cleaned.length()
    
  }
  
  
}

