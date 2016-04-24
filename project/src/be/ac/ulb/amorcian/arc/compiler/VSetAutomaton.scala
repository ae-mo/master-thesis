package be.ac.ulb.amorcian.arc.compiler

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import dk.brics.automaton.Automaton
import dk.brics.automaton.RegExp

import be.ac.ulb.amorcian.arc.runtime.Instruction
import be.ac.ulb.amorcian.arc.runtime.InstructionType

/**
 * Represents a vset-automaton.
 */

class VSetAutomaton(val nrStates: Int, val initial: Int, val transitionFunction:Map[Int, Map[Int, String]], val vars: Array[String], val finalStates: Array[Int]) {

	/* 

  def union(other: VSetAutomaton): VSetAutomaton {}


	 */

	/**
	 * Joins the vset-automaton with another one.
	 */
	def join(other: VSetAutomaton) = {
	    
	  import scala.util.control.Breaks._
    // Get the common variables
    val commonVars = this.vars.toSet.intersect(other.vars.toSet)
    
    var initial:(Int, Int) = null
    var trFuncsIntersection = Map[(Int, Int), Map[(Int, Int), String]]()
    var finalStates = new ArrayBuffer[(Int, Int)]()
    
		// Make both automata lexicographic
		val a1 = this.toVSetPathUnion().toHybridPathUnion().toLexicographicPathUnion().toVSetAutomaton()
		val a2 = other.toVSetPathUnion().toHybridPathUnion().toLexicographicPathUnion().toVSetAutomaton()
		
		val varPatternIn = "(.)_in".r
    val varPatternOut = "(.)_out".r
    val varPattern= ".*(._).*".r
		
    // Conditional creation of a new state pair
    val createIfNotExists = (s1: Int, s2: Int)  => if(!trFuncsIntersection.contains((s1, s2)))
                                                      trFuncsIntersection += (((s1, s2), Map[(Int, Int), String]()))
    
    // Conditional creation of a transition with empty label
    val emptyStringIfNotExists = (s1: Int, s2: Int, t1: Int, t2: Int)  => if(!trFuncsIntersection((s1, s2)).contains((t1, t2)))
                                                                              trFuncsIntersection((s1, s2)) += (((t1, t2), ""))
                                                                              
    // Deletion of a transition
    val deleteIfExists = (s1: Int, s2: Int, t1: Int, t2: Int)  => if(trFuncsIntersection.contains((s1, s2)) && trFuncsIntersection((s1, s2)).contains((t1, t2)))
                                                                              trFuncsIntersection((s1, s2)) -= ((t1, t2))                                                                           
    // Add a choice in a transition's label
    val addChoice = (s1: Int, s2: Int, t1: Int, t2: Int, choice:String)  => if(trFuncsIntersection.contains((s1, s2)) && trFuncsIntersection((s1, s2)).contains((t1, t2)))
                                                                              trFuncsIntersection((s1, s2))((t1, t2)) += "|" + choice
                                                                            else {
                                                                              createIfNotExists(s1, s2)
                                                                              trFuncsIntersection((s1, s2)) += (((t1, t2), choice))
                                                                            }
    
    // Add a variable operation in a transition's label
    val addVarOp = (s1: Int, s2: Int, t1: Int, t2: Int, v:String, varOp:String, i:Int) => if(!commonVars.contains(v)) {
                                                                                       if(trFuncsIntersection.contains((s1, s2)) && trFuncsIntersection((s1, s2)).contains((t1, t2)))
                                                                                          trFuncsIntersection((s1, s2))(if(i == 1) ((t1, s2)) else ((s1, t2))) += "," + varOp
                                                                                       else {
                                                                                         createIfNotExists(s1, s2)
                                                                                         trFuncsIntersection((s1, s2)) += ((if(i == 1) ((t1, s2)) else ((s1, t2)), varOp))
                                                                                       }
                                                                                  }
                                                                                  else {
                                                                                    
                                                                                    deleteIfExists(s1, s2, t1, t2)
                                                                                    break
                                                                                  }
		// Perform the cross product
		for((s1, ts1)<- a1.transitionFunction; (s2, ts2)<- a2.transitionFunction) {
		  
		  // The state pair is the initial state only if both components are
		  if(a1.initial == s1 && a2.initial == s2)
		    initial = ((s1, s2))
		  
		  // The state pair is a finals state only if both components are
		  if(a1.finalStates.contains(s1) && a2.finalStates.contains(s2))
		    finalStates += ((s1, s2))
		  
		  breakable {
		    
		    // Intersect the outgoing transitions of the two states
    	  for((t1, e1) <- ts1; (t2, e2) <- ts2) { 
    	    
    	    // Obtain the single tokens of the labels
          val toks1 = tokenizeEdge(e1)
          val toks2 = tokenizeEdge(e2)
          
          // Confront the tokens
          for (tok <- toks1; tok2 <-toks2) {
            
            tok match {
              
              case "." => {
    
                  if(tok2 != "\\n"  && tok2 !="()" && !tok2.matches(varPattern.toString)) {
                    
                    addChoice(s1, s2, t1, t2, tok2)
                  }
                  else if(tok2.matches(varPattern.toString)) {
                    val varPattern(x)= tok2
                    addVarOp(s1, s2, t1, t2, x, tok2, 2)
                  }
                  else if(tok2 == "()") {
                    
                    createIfNotExists(s1, s2)
                    trFuncsIntersection((s1, s2)) += (((s1, t2), tok2))
                  }
    
              }
              case "\\d" => {
    
                  if(tok2.matches("\\d") || tok2 == "\\d") {
                    
                    addChoice(s1, s2, t1, t2, tok2)
                  }
                  else if(tok2 == ".") {
                    
                    createIfNotExists(s1, s2) 
                    trFuncsIntersection((s1, s2)) += (((t1, t2), tok))
                  }
                  else if(tok2.matches(varPattern.toString)) {
                    val varPattern(x)= tok2
                    addVarOp(s1, s2, t1, t2, x, tok2, 2)
                  }
                  else if(tok2 == "()") {
                    
                    createIfNotExists(s1, s2)
                    trFuncsIntersection((s1, s2)) += (((s1, t2), tok2))
                  }
    
              }
              case "\\s" => {
    
                  if(tok2.matches("\\s") || tok2 == "\\s") {
                    
                    addChoice(s1, s2, t1, t2, tok2)
                  }
                  else if(tok2 == ".") {
                    
                    createIfNotExists(s1, s2) 
                    trFuncsIntersection((s1, s2)) += (((t1, t2), tok))
                  }
                  else if(tok2.matches(varPattern.toString)) {
                    val varPattern(x)= tok2
                    addVarOp(s1, s2, t1, t2, x, tok2, 2)
                  }
                  else if(tok2 == "()") {
                    
                    createIfNotExists(s1, s2)
                    trFuncsIntersection((s1, s2)) += (((s1, t2), tok2))
                  }
    
              }
              case "()" => {
                
                if(tok2 == "()") {
                  
                  createIfNotExists(s1, s2) 
                  trFuncsIntersection((s1, s2)) += (((t1, t2), tok))
                }
                else {
                  
                  createIfNotExists(s1, s2) 
                  trFuncsIntersection((s1, s2)) += (((t1, s2), tok))
                }
              }
              
              case varPattern(x) => {
                
                val otherContains = toks2.contains(tok)
                val isCommonVar = commonVars.contains(x)
                
                if(otherContains) {
                  
                  createIfNotExists(s1, s2)
                  emptyStringIfNotExists(s1, s2, t1, t2)
                  trFuncsIntersection((s1, s2))((t1, t2)) += "," + tok
                }
                else if(!isCommonVar) {
         
                  addVarOp(s1, s2, t1, t2, x, tok, 1)
                }
                else {
                  
                  deleteIfExists(s1, s2, t1, s2)
                  break
                }
              }
              
              // tok is a simple char, so check all the possibilities of the other token
              case _ => {
                
                tok2 match {
                  
                  case "." => {
                    
                    if(tok != "\\n") {
                      addChoice(s1, s2, t1, t2, tok)
                    }
                  }
                  case "\\d" => {
                    
                    if(tok.matches("\\d") ){
                      addChoice(s1, s2, t1, t2, tok)
                    }
                  }
                  case "\\s" => {
                    
                     if(tok.matches("\\s")){
                      addChoice(s1, s2, t1, t2, tok)
                    }
                  }
                  case "()" => {
                    
                    createIfNotExists(s1, s2) 
                    trFuncsIntersection((s1, s2)) += (((s1, t2), tok2))
                  }
                  case varPattern(x) => {
                    
                    val varPattern(x)= tok2
                    addVarOp(s1, s2, t1, t2, x, tok2, 2)
                  }
                  
                  // tok2 is a simple char too
                  case _ => {
                    
                    if(tok == tok2)
                      addChoice(s1, s2, t1, t2, tok)
                  }
                }
              }
            }
          }
    	  }
		  }
		}
		
		var oldNoUnreachableTF = trFuncsIntersection
		var noUnreachableTF = Map[(Int, Int), Map[(Int, Int), String]]()
		
		// Remove all states with no incoming transitions (except the initial) iteratively
		var canChange = true
		
		while(canChange) {
		  
		  canChange = false
		  for((s, ts) <- oldNoUnreachableTF if (s, ts) != initial) {
		  
  		  var remove = true
  		  for((s1, ts1) <- oldNoUnreachableTF) {
  		    
  		    if(ts1.contains(s))
  		      remove = false
  		  }
  		  if(!remove)
  		    noUnreachableTF += ((s, ts))
  		  else {
  		    if(finalStates.contains(s))
  		      finalStates -= s
  		    canChange = true
  		  } 
  		    
  		}
		  
		  if(canChange) {
		    
		    oldNoUnreachableTF = noUnreachableTF
        noUnreachableTF = Map[(Int, Int), Map[(Int, Int), String]]()
		  }
		}
		
		var oldNoDeadTF = noUnreachableTF
		var noDeadTF = Map[(Int, Int), Map[(Int, Int), String]]()
		
		// Remove all states with no outgoing transitions (except the finals) iteratively
		canChange = true
		
		while(canChange) {
		  
		  canChange = false
		  
		  // Remove all transitions pointing to dead states
		  for((s, ts) <- oldNoDeadTF) {
		  
  		 for((t, e) <- ts if (!finalStates.contains(t) && !oldNoDeadTF.contains(t)))
  		   oldNoDeadTF(s) -= t
  		   canChange = true
  		}
		  
		  // Copy only the states with >1 outgoing transition
		  for((s, ts) <- oldNoDeadTF) {
		    
		    if(ts.size == 0)
		      canChange = true
		    else
		      noDeadTF += ((s, ts))
		  }
		  
		  if(canChange) {
		    
		    oldNoDeadTF = noDeadTF
        noDeadTF = Map[(Int, Int), Map[(Int, Int), String]]()
		  }
		}
		
		// If the intersection is empty, return null, otherwise rename each state with a unique integer
		if(noDeadTF.size == 0 || noDeadTF(initial).size == 0 || finalStates.size == 0)
		  null
		else {
		  
		  var visitedStates= Map[(Int, Int), Int]()
		  var stateCounter = 0
		  var transitionFunction = Map[Int, Map[Int, String]]()
		  
		  // The new initial state is 0
		  val newInitial = 0 
		  stateCounter += 1
		  
		  var newFinalStates = ArrayBuffer[Int]()
		  
		  // Rename each state in the map and its targets
		  for((s , ts) <- noDeadTF) {
		    
		    var s1 = Map[Int, String]()
		    
		    if(visitedStates.contains(s))
		      transitionFunction += ((visitedStates(s), s1))
		    else {
		      
		      transitionFunction += ((stateCounter, s1))
		      visitedStates += ((s, stateCounter))
		      stateCounter +=1
		      
		    }
		    
		    for((t, e) <- ts) {
		      
		      if(visitedStates.contains(t))
  		      s1 += ((visitedStates(t), e))
  		    else {
  		      
  		      s1 += ((stateCounter, e))
  		      visitedStates += ((t, stateCounter))
  		      stateCounter +=1
  		      
  		    }
		    }
		    
		    // Update the final states
		    for(s <- finalStates) {
		      
		      if(visitedStates.contains(s))
		        newFinalStates += visitedStates(s)
		      else {
		        
		        newFinalStates += stateCounter
		        stateCounter += 1
		      }
		    }
		  }
		  
		  // Return the automaton that is the join of the two original ones
		  new VSetAutomaton(stateCounter, newInitial, transitionFunction, a1.vars ++ a2.vars, newFinalStates.toArray)
		}
	}
	
	/**
	 * Extracts the variables from a series of variable operations.
	 */
	def extractVars(ops:ArrayBuffer[String]):Set[String] = {
	  
	  var vars = new ArrayBuffer[String]()
	  
	  val openPattern = "(.)_in".r
	  val closePattern = "(.)_out".r
	  
	  for(op <- ops) {
	    
	    op match {
	      
	      case openPattern(x) => {
	        
	        vars += x
	      }
	      
	      case closePattern(x) => {
	        
	        vars+= x
	      }
	    }
	    
	  }
	  vars.toSet
	}
	
	/**
	 * Breaks an edge into tokens.
	 */
	def tokenizeEdge(e: String): ArrayBuffer[String] = {
	  
	  var tokens = new ArrayBuffer[String]()
	  
	  val choicePattern = "(.\\|)".r
	  val varPattern= ".*(._).*".r
	  
	  e match {
	    
      case varPattern(x) => {
        
        val varOps = e.split(",")
        tokens ++= varOps
      }
      
      case choicePattern(x) => {
        
        val toks = e.split("\\|")
        tokens ++= toks
      }
      case _ => {
        tokens += e
      }

	  }
	  
	  tokens
	}

	/**
	 * Projects the vset-automaton on the desired variables.  
	 */
	def project(vars: Array[String]):VSetPathUnion = {

			// get the corresponding vset path union
			val pU = this.toVSetPathUnion()

					var newPathUnion = new ArrayBuffer[ArrayBuffer[(String, Map[String, String], Int)]]()

					// eliminate variable operations involving variables on which we aren't projecting
					for(path <- pU.pathUnion) {

						var newPath = new ArrayBuffer[(String, Map[String, String], Int)]()

								for((e, ops, t) <- path if ops != null) {

									val currentVars = ops.keySet.toBuffer

											// first get the variables that we don't project on, then remove them from the new variables
											val newVars = currentVars.diff(currentVars.diff(vars))

											// select the operations we are interested in
											val newOps: Map[String, String] = newVars.map(x => x -> ops(x)) (collection.breakOut)

											newPath += ((e, newOps, t))
								}

						newPathUnion += newPath

					}

			new VSetPathUnion(newPathUnion, vars, pU.finalStates)

	}
	
	/**
	 * Translates the vset-atomaton into a NFA program.
	 */
	def toNFAProgram(program:ArrayBuffer[Instruction], visitedStates:Map[Int, Int], s:Int, pc:Int): (Int, ArrayBuffer[Instruction]) = {
	  
	  var npc = pc 
	  var jumps = new ArrayBuffer[Instruction]()
	  
	  visitedStates+= ((s, npc))
	  
	  // A final state translates into a match operation, and it 
	  // doesn't go through the previous block because it doesn't have 
	  // outgoing transitions
	  if(finalStates.contains(s)) {
	    
	    program += new Instruction(InstructionType.MATCH, -1, -1, npc, null, null)
      npc += 1
	  }
	  else {
	    
	    var j = 0
	    var oldSplit:Instruction = null
	    
	    for((t, e) <- transitionFunction(s)) {
	      
	      var instr:Instruction = null
	      var jmp:Instruction = null
	      var split:Instruction = null
	      
  	    
  	    // If there are >1 outgoing transitions for the current state, we need to put
    	  // splits
    	  if(transitionFunction(s).size > 1 && j < transitionFunction(s).size - 1) { 
    	    
    	    split = new Instruction(InstructionType.SPLIT, -1, -1, npc, null, null)
    	    program += split
  	      npc += 1
  	      
  	      // If it is not the first split, connect the previous to this
  	      if(oldSplit != null) {
  	        
  	        oldSplit.y = split
  	      }

    	  }
    	  else split = null
	      
	      val varPatternIn = "(.)_in".r
  	    val varPatternOut = "(.)_out".r
  	    val varPattern= ".*(._).*".r
  	    
  	    // Find out the type of label and add corresponding instruction to the program
  	    e match {
  	      
  	      case "\\d" => {
  	        
  	        instr = new Instruction(InstructionType.DIGIT, -1, -1, npc, null, null)
  	        program += instr
  	        npc +=1
  	      }
  	      case "." => {
  	        
  	        instr = new Instruction(InstructionType.DOT, -1, -1, npc, null, null)
  	        program += instr
  	        npc +=1
  	      }
  	      
  	      case "\\s" => {
  	        
  	        instr = new Instruction(InstructionType.WHITESPACE, -1, -1, npc, null, null)
  	        program += instr
  	        npc +=1
  	      }
  	      
  	      case varPattern(va) => {
  	        
  	         val varOps = e.split(",") 
  	         
  	         for(w <- 0 until varOps.length) {
  	           
  	           val vO = varOps(w)
  	           var in = true
  	           var v = ""
  	           if(vO.matches("._in")) {
  	             
  	             val varPatternIn(x) = vO
  	             v = x
  	           }
  	           else {
  	             
  	             val varPatternOut(x) = vO
  	             v = x
  	             in = false
  	           }
  	           
  	           var i = v.toInt
  	           
  	           // Because each variable has two places
  	           // in the array of saved pointers
  	           i *= 2
  	           
  	           // If we are closing a variable
  	           if(!in)
  	             i += 1

  	           program += new Instruction(InstructionType.SAVE, -1, i, npc, null, null)
  	           npc +=1
  	           
  	           if(w == 0)
  	             instr = program.last
  	         }
  	         
  	      }
  	      
  	      case "\\(\\)" => {
  	        
  	        // do nothing!
  	      }
  	      case _ => {
  	        
  	        instr = new Instruction(InstructionType.CHAR, e.charAt(0), -1, npc, null, null)
  	        program += instr
  	        npc +=1
  	      }
  
  	    }
  	    
  	    // if we placed a split, connect it to the first instruction
  	    if(split != null) {
  	      split.x = instr
  	      oldSplit = split
  	    }
  	    else if(oldSplit != null) {
  	      
  	      oldSplit.y = instr
  	    }
  	    
  	    
  	    if(visitedStates.contains(t)) {
  	      
  	      program += new Instruction(InstructionType.JMP, -1, -1, npc, program(visitedStates(t)), null)
  	      npc += 1
  	    }
  	    else {
  	      
  	      // Recur
    	    val (n1pc, lastJumps) = toNFAProgram(program, visitedStates, t, npc)
    	    npc = n1pc

    	    // Jump to the end of the alternatives
    	    jumps += new Instruction(InstructionType.JMP, -1, -1, npc, null, null)
    	    program += jumps.last
    	    npc+=1
    	    // Connect the jumps of the inner code to the outer code
    	    for(jmp <- lastJumps) {
    	      jmp.x = program.last
    	    }
  	    }
  	    
  	    j += 1
  	  }
	  }
  	  

	  (npc, jumps)
	}


/**
 * Transforms the vset-automaton into a vset path union.
 */
def toVSetPathUnion():VSetPathUnion = {

		val (nrStates, initial, transitionGraph, finalStates2) = stateElimination()

				var pathsToProcess = 1

				var availablePathVars = new ArrayBuffer[Map[String, Boolean]]()
				var pathsUnion = new ArrayBuffer[ArrayBuffer[(String, Map[String, String], Int)]]()

				var firstPathVars = Map[String, Boolean]()

				for(v <- vars)
					firstPathVars += (v -> true)

					availablePathVars += firstPathVars

					pathsUnion += new ArrayBuffer[(String, Map[String, String], Int)]()
					pathsUnion(0) += (("", null,initial))

					// Grow the paths iteratively
					while(pathsToProcess > 0) {

						var newPaths:ArrayBuffer[ArrayBuffer[(String,  Map[String, String], Int)]] = null
								var newPathVars:ArrayBuffer[Map[String, Boolean]] = null

								// Advance each path by one transition, spawning new paths if there are >1 possible branches
								for(i <- 0 until pathsUnion.length) {

									var path = pathsUnion(i)
											var pathVars = availablePathVars(i)

											newPaths =  new ArrayBuffer[ArrayBuffer[(String,  Map[String, String], Int)]]()
											newPathVars = new ArrayBuffer[Map[String, Boolean]]()

											val (e, varOps, s) = path.last

											// If we haven't reached a final state in the current path
											if(!finalStates2.contains(s)) {

												// Contains the variable operations to be performed at each transition


												for((t, e1) <- transitionGraph(s)) {

													var ops = Map[String, String]()

															var discard = false
															var out = false
															var v:String = null
															var e2 = e1

															// If a branch tries to open a variable, allow it only if it's still available and remove it
															// from available vars for that branch, otherwise discard branch
															if(e1.matches(".*._in.*")) {

																val openedVar = ".*(.)_in.*".r
																		val openedVar(x) = e1
																		v = x

																		if(pathVars(x)) {
																			out = true 

																					ops += ((x, "in"))

																					val varPattern = "(\\()?._in(\\))?"

																					e2 = e1.replaceAll(varPattern, "()")

																		}

																		else discard = true

															}
															else if (e1.matches(".*._out.*")) {

																val closeCommand = ".*(.)_out.*".r
																		val closeCommand(x) = e1

																		ops += ((x, "out"))

																		val varPattern = "(\\()?._out(\\))?"
																		e2 =  e1.replaceAll(varPattern, "()")

															}


													// If we didn't discard the new branch, make it a path
													if(!discard) {

														newPaths += path.clone
																newPaths.last += ((e2, ops, t))

																newPathVars += pathVars.clone

																if(out)
																	newPathVars.last(v) = false


													}

												}

												// Add the new paths to the path union
												path += newPaths(0).last
														pathsToProcess += newPaths.tail.size
														pathsUnion.appendAll(newPaths.tail)

														// Add the available variables for the new paths
														availablePathVars(i) = newPathVars(0)
														availablePathVars.appendAll(newPathVars.tail)


											}
											else
												pathsToProcess -=1

								}

					}

		// Prune paths that don't consume all the available variables
		for(i <- pathsUnion.length-1 to 0 by -1) {

			if(availablePathVars(i).values.exists(_ == true)) {

				pathsUnion.remove(i)

			}

		}

		new VSetPathUnion(pathsUnion, vars, finalStates2)

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
									transitionGraph(nrStates) = transitionGraph(nrStates) + (0 -> "()")
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

						transitionGraph(q) = transitionGraph(q) + (nrStates -> "()")
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

						if(t1.contains(i) && t1(i).matches(".*((._in)|(._out)).*")) {

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
				cleaned = cleaned.replaceAll("()", "a")

				cleaned.length()

}


}

