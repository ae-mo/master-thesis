package be.ac.ulb.amorcian.arc.compiler

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import dk.brics.automaton.Automaton
import dk.brics.automaton.State
import dk.brics.automaton.Transition
import dk.brics.automaton.RegExp

class HybridPathUnion(var pathUnion: ArrayBuffer[ArrayBuffer[(Automaton,  Map[String, String], Int)]], var vars: Array[String]) {
  
  /**
	 * Makes a hybrid path union lexicographic.
	 */
	def toLexicographicPathUnion():HybridPathUnion = {

		// Automaton accepting only the empty string
		val epsAut = (new RegExp("()")).toAutomaton()
		
		epsAut.expandSingleton()

		var currentPathUnion = pathUnion.clone
		var nextPathUnion = new ArrayBuffer[ArrayBuffer[(Automaton,  Map[String, String], Int)]]()

		var canChange = true

		// Make the paths lexicographic
		while(canChange) {
		  
		  canChange = false

			for(path <- currentPathUnion) {
			  var noEps = path.clone
			  var epsRemoved = false
			  var newPaths = 0
			  // Remove all epsilon transitions
			  if(noEps.length > 1)  {
  				for(i <- path.length -1 to 1 by -1) {
  
  					val (a, o, t) = noEps(i)
  					// Check if the language of the automaton accepts only the empty string
  					if(a.isEmptyString) {
  
  						// eliminate the epsilon transition and add its variable ops
  						// to the previous transition
  						var part1 = if(i > 1) noEps.slice(0, i-1).clone else new ArrayBuffer[(Automaton,  Map[String, String], Int)]()
  						val part2 = if (i < noEps.length) noEps.slice(i+1, noEps.length).clone else new ArrayBuffer[(Automaton,  Map[String, String], Int)]()
  						val (a1, o1, t1) = noEps(i - 1)
  						val newOps = o ++ o1
  						
  						// Fix variables opened and closed at the same time
  						val commonVars = o.keySet.intersect(o1.keySet)
  						for(v <- commonVars) {
  						  
  						  newOps(v) = "inout"
  						}
  						
  						part1 += ((a1, newOps, t1))
  						noEps = part1 ++ part2
  						epsRemoved = true
  
  					}
							
				}
			    
			  // Remove the empty string from the languages of the transitions, keep both the new languages and the empty strings
			  // in different paths
				for(i <- 1 until noEps.length) {  
				  
				 val (a, o, t) = noEps(i)
				  
         // If it doesn't accept only the empty string
         if(a.run("")) {

           
           //create a new path with the automaton not accepting the empty string anymore
           var part1 = noEps.slice(0, i).clone
					 val part2 = if (i < path.length -1) noEps.slice(i+1, noEps.length).clone else new ArrayBuffer[(Automaton,  Map[String, String], Int)]()
           val a1 = a.minus(epsAut)
           
           // Assemble the new paths
           val newPath1 = part1.clone
           newPath1 += ((epsAut.clone, o.clone, t))
           newPath1 ++= part2.clone
           val newPath2 = part1.clone
           newPath2 += ((a1, o.clone, t))
           newPath2 ++= part2.clone
           nextPathUnion += newPath1
           nextPathUnion += newPath2
           
           newPaths += 2
           
           // need to fix the epsilon transitions 
           // plus eliminate epsilon from languages not processed because in branches just generated
           // in the next iteration
           canChange = true
           
         }
 
       }
			}
			 
			 // If no transitions accepting the empty string were discovered,
			 // simply add the original path
			 if(newPaths == 0)
			   nextPathUnion += noEps
			   
			}

		  currentPathUnion = nextPathUnion 
		  // Duplicates can occur! (Combinatorics)
		  currentPathUnion = currentPathUnion.distinct
		  
		  nextPathUnion = new ArrayBuffer[ArrayBuffer[(Automaton,  Map[String, String], Int)]]()
		}

	  new HybridPathUnion(currentPathUnion, vars)

  }
	
	
	
	/**
	 * Converts the hybrid path union into a vset-automaton.
	 */
	def toVSetAutomaton(): VSetAutomaton = {
	  
	  var sc = 0
	  var initial = 0
	  var transitionFunction = Map[Int, Map[Int, String]]()
	  var finalStates = new ArrayBuffer[Int]()
	  
	  transitionFunction += ((0, Map[Int, String]()))
	  
	  var a1:VSetAutomaton = null
	  var a1Old:VSetAutomaton = null
	  var eOld = ""
	  // Process all the edges of all the paths
	  for(p <- pathUnion) {

	    // The first edge in the path will always be an epsilon-transition.
	    val (a, ops, t) = p(0)
	    var e = flattenVarOps(ops)
	    if(e != "") {
	      sc += 1
	      transitionFunction(0)+= ((sc, e))
	    }
	    
	    
	    // Process all the other edges.
	    for(i <- 1 until p.length) {
	      
	      val (a, ops, t) = p(i)
	      
	      // If there were no variable operations in the previous edge
	      // and the previous automaton exists,
	      // connect all the final states of the previous automaton to
	      // a new state
	      if(eOld == "" && a1Old != null) {
	        
	       sc+= 1
	       for(s <- a1Old.finalStates) {
	         
	         if(!transitionFunction.contains(s))
	           transitionFunction += ((s, Map[Int, String]()))
	           
	         transitionFunction(s) += ((sc, "()"))
	       }
	       
	      }
	      
	      // Convert the automaton of the current edge and add its transitions
	      if(!a.isEmptyString()) {
	        
	        val (na, nsc) = convertEdge(a, sc)
	        transitionFunction ++= na.transitionFunction
	        sc = nsc
	        a1 = na
	        
	      }
	      
	      // Flatten the variable operations and make a new transition with them
	      e = flattenVarOps(ops)
        if(e != "") {
	       sc += 1
	       
	       if(a1 != null)
  	       for(s <- a1.finalStates) {
  	         
  	         transitionFunction(s)+= ((sc, e))
  	         
  	       }
	       else
	         transitionFunction(sc - 1) += ((sc, e))
	       
        }
	        
	      
	      
	    }
	    
	    // Determine the final states at the end of this path.
	    if(e != "")
	      finalStates += sc
	    else
	      finalStates ++= a1.finalStates
	      
	      a1Old = a1
	      eOld = e
	      
	  }
	  
	  new VSetAutomaton(sc + 1, initial, transitionFunction, vars, finalStates.toArray)
	  
	}
	
	/**
	 * Flattens a map of variable operation into a string.
	 */
	def flattenVarOps(ops: Map[String, String]):String = {
	  
	   var e = ""
	    
	    for((v, op) <- ops) {
	      
	      // Create a new transition from the variable operations
	      op match {
	        
	        case "in" => e += "," + v + "_in"
	        case "out" => e += "," + v + "_out"
	        case "inout" => e += "," + v + "_in" + "," + v + "_out"
	        case _ => ""
	      }   
	      
	    }
	   
	   e.stripPrefix(",")
	  
	}
	
	/**
	 * Converts an automaton edge into a vset-automaton, using a state counter to number its states.
	 */
	def convertEdge(e: Automaton, stateCounter: Int):(VSetAutomaton, Int) = {
	  
	  import scala.collection.JavaConversions.asScalaSet
	  import scala.collection.mutable.SortedSet
	  
	  var eStates:scala.collection.mutable.Set[State] = e.getStates
	  val eAccept:scala.collection.mutable.Set[State]  = e.getAcceptStates
	  val eInitial = e.getInitialState 
	  eStates -= eInitial
	  var sc = stateCounter
	  var stateMap = Map[State, Int]()
	  
	  var initial = 0
	  var transitionFunction = Map[Int, Map[Int, String]]()
	  var finalStates = new ArrayBuffer[Int]()
	  
	  // Treat the initial state separatedly.
	  initial = sc
	  stateMap += ((eInitial, sc))
	  if(eAccept.contains(eInitial))
	    finalStates += sc
	  
	  // Map the states to new states according to the state counter.
	  for(s <- eStates) {

	     // Map it to a new state
       sc += 1
       stateMap += ((s, sc))

	    // If it is an accepting state
	    if(eAccept.contains(s)) 
	      finalStates += sc
	      
	  }
	  
	  // Add the transitions to the transition function.
	  for((s, n) <- stateMap) {

	    val sTransitions:scala.collection.mutable.Set[Transition] = s.getTransitions
	    
	    if(sTransitions.size > 0)
	      if(!transitionFunction.contains(n))
	        transitionFunction += ((n, Map[Int, String]()))
	    
	    for(t <- sTransitions) {
	      
	      val d = t.getDest
	      val nd = stateMap(d)
	        
	      val min = t.getMin
	      val max = t.getMax

	      // Create a transition for each character in the range (maybe leave it implicit?).
	      for(c <- Set(min to max)) {
	        
	        if(!transitionFunction(n).contains(nd))
	          transitionFunction(n) += ((nd, c.charAt(0).toString()))
	        else
	          transitionFunction(n)(nd) += "|" + c.charAt(0).toString()
	      }
	    }
	  }
	  
	  // Return the created vset-automaton as well as the current state counter;
	  (new VSetAutomaton(eStates.size, initial, transitionFunction, new Array[String](0), finalStates.toArray), sc)
	}
	
}