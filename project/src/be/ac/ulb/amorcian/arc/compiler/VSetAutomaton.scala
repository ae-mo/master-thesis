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

  def join(other: VSetAutomaton): VSetAutomaton {}

  def union(other: VSetAutomaton): VSetAutomaton {}


	 */

	/**
	 * Joins the vset automaton with another one.
	 */
	def join(other: VSetAutomaton):(ArrayBuffer[ArrayBuffer[(Automaton,  Map[String, String], Int)]], Array[Int]) = {

			val pT = this.toVSetPathUnion()
					val pO = other.toVSetPathUnion()

					// Replace each transition with an automaton
					var hPT = pT.toHybridPathUnion
					var hPO = pO.toHybridPathUnion

					var lexPathUnion = new ArrayBuffer[ArrayBuffer[(Automaton,  Map[String, String], Int)]]()
					// Automaton accepting only the empty string
					val epsAut = (new RegExp("()")).toAutomaton()

					// Make the paths lexicographic
					for(path <- hPT.pathUnion) {

						var newPaths = new ArrayBuffer[ArrayBuffer[(Automaton,  Map[String, String], Int)]]()

								for(i <- 0 until path.length) {

									val (a, o, t) = path(i)

											// Check if the language of the automaton contains the empty string
											if(a.getShortestExample(true) == "()") {

												// eliminate the epsilon transition and add its variable ops
												// to the previous transition
												var part1 = path.slice(0, i-2)
														val part2 = path.slice(i+1, path.length -1)
														val (a1, o1, t1) = path(i)
														val newOps = o ++ o1
														part1 += ((a, newOps, t))
														val newPath = part1 ++ part2
														newPaths += newPath

														// If it doesn't accept only the empty string
														if(a.getSingleton != null) {

															//create a new path with the automaton not accepting the empty string anymore
															val a1 = a.minus(epsAut)
																	part1 += path(i-1)
																	part1 += ((a1, o, t))
																	val newPath = part1 ++ part2
																	newPaths += newPath

														}

											}

								}


					}



			var join = new ArrayBuffer[ArrayBuffer[(Automaton,  Map[String, String], Int)]]()

					for (pathT <- hPT.pathUnion; pathO <- hPO.pathUnion) {

					} 

			(new ArrayBuffer[ArrayBuffer[(Automaton,  Map[String, String], Int)]](), Array[Int]())

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
    	  if(transitionFunction(s).size > 1 && j < transitionFunction.size - 1) { 
    	    
    	    split = new Instruction(InstructionType.SPLIT, -1, -1, npc, null, null)
    	    program += split
  	      npc += 1
  	      
  	      // If it is not the first split, connect the previous to this
  	      if(oldSplit != null) {
  	        
  	        oldSplit.y = split
  	      }
    	  }
	      
	      val varPatternIn = "(.)_in".r
  	    val varPatternOut = "(.)_out".r
  	    val varPattern= ".*(._).*".r
  	    
  	    // Find out the type of label and add corresponding instruction to the program
  	    e match {
  	      
  	      case "\\d" => {
  	        
  	        instr = new Instruction(InstructionType.DIGIT, -1, -1, npc, null, null)
  	        program += instr
  	      }
  	      case "." => {
  	        
  	        instr = new Instruction(InstructionType.DOT, -1, -1, npc, null, null)
  	        program += instr
  	      }
  	      
  	      case "\\s" => {
  	        
  	        instr = new Instruction(InstructionType.WHITESPACE, -1, -1, npc, null, null)
  	        program += instr
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
  	      }
  
  	    }
  	    
  	    // if we placed a split, connect it to the first instruction
  	    if(split != null) {
  	      split.x = instr
  	      oldSplit = split
  	    }
  	    
  	    npc +=1
  	    if(visitedStates.contains(t)) {
  	      
  	      program += new Instruction(InstructionType.JMP, -1, -1, npc, program(visitedStates(t)), null)
  	    }
  	    else {
  	      
  	      // Recur
    	    val (n1pc, lastJumps) = toNFAProgram(program, visitedStates, t, npc)
    	    npc = n1pc

    	    // Jump to the end of the alternatives
    	    jumps += new Instruction(InstructionType.JMP, -1, -1, npc, null, null)
    	    program += jumps.last
    	    
    	    // Connect the jumps of the inner code to the outer code
    	    for(jmp <- lastJumps) {
    	      jmp.x = program.last
    	    }
  	    }
  	    
  	    
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

