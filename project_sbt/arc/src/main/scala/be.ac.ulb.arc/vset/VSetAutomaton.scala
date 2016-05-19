package be.ac.ulb.arc.vset

import scala.collection.immutable.{HashSet => SVars}
import scala.collection.immutable.{HashSet => SVOps}
import scala.collection.immutable.{HashSet => TransitionFunction}
import scala.collection.immutable.{HashSet => StateSet}
import scala.collection.immutable.{HashSet => Predecessors}
import scala.{Int => SVar}

/**
  * Represents a variable-set automaton.
  * @param Q
  * @param q0
  * @param qf
  * @param δ
  */
class VSetAutomaton(val Q:StateSet[State], val q0:State, val qf:State, val V:SVars[SVar], val δ:TransitionFunction[Transition[State]]) {

  type State2= (State, State)

  /**
    * Performs the union of this vset-automaton with another one.
    * @param other
    * @return
    */
  def ∪(other: Option[VSetAutomaton]): Option[VSetAutomaton] = {

    // If we got nothing as parameter, abort (useful for fluent interface)
    if(other == None) return None

    val o = other.get

    // Determine common variables
    val cV = this.V.intersect(o.V)

    // If the automata are not variable compatible, abort
    if (!this.V.diff(cV).isEmpty || !o.V.diff(cV).isEmpty) return None

    // Get the transitions of the other that involve the initial state
    val oInitials = o.δ.filter((t:Transition[State]) => t.q == o.q0 || t.q1 == o.q0)
    // Get all the other transitions
    val oOthers = o.δ.filter((t:Transition[State]) => !oInitials.contains(t))

    // Initialize new transition function
    var newδ = this.δ ++ oOthers

    // Substitute the other initial state with this initial states
    // in all transition involving it
    for(t <- oInitials) {

      val isQ = t.q == o.q0
      var newT:Transition[State] = null

      t match {

        case OrdinaryTransition(q, σ, q1) => {
          if(isQ) newT = new OrdinaryTransition[State](q0, σ, q1)
          else newT = new OrdinaryTransition[State](q, σ, q0)
        }
        case RangeTransition(q, σ, q1) => {
          if(isQ) newT = new RangeTransition[State](q0, σ, q1)
          else newT = new RangeTransition[State](q, σ, q0)
        }
        case OperationsTransition(q, s, v, q1) => {

          if(isQ) newT = new OperationsTransition[State](q0, s, v, q1)
          else newT = new OperationsTransition[State](q, s, v, q0)
        }
        case _ => None
      }

      newδ = newδ + newT
    }

    // Connect the other final state to this one
    newδ = newδ + new OperationsTransition[State](o.qf, new SVOps, this.V, this.qf)

    Some(new VSetAutomaton(this.Q ++ (o.Q - o.q0), this.q0, this.qf, this.V, newδ))
  }

  /**
    * Performs the projection of this vset-automaton on the desired span variables.
    * @param vars
    * @return
    */
  def π(vars:SVars[SVar]): VSetAutomaton = {

    val nvars = this.V.diff(vars)

    // Get the transitions involving variable operations
    val tOps = this.δ.filter((t:Transition[State]) => t.isInstanceOf[OperationsTransition[State]])
    // The other transitions
    val tOther = this.δ.diff(tOps)

    // Include the other transitions as they are
    var newδ = tOther

    // Eliminate the variable operations involving variables not in vars in each operations
    // transition, add it to the new δ
    for(tO <- tOps) {

      val t = tO.asInstanceOf[OperationsTransition[State]]
      var newS = t.S

      for (op <- t.S) {

        if(nvars.contains(op.x))
          newS = newS - op
      }

      newδ = newδ + new OperationsTransition[State](t.q, newS, t.V, t.q1)
    }

    new VSetAutomaton(this.Q, this.q0, this.qf, vars, newδ)
  }

  /**
    * Returns the join of this vset-automaton with another one.
    * @param other
    * @return
    */
  def ⋈(other: Option[VSetAutomaton]): Option[VSetAutomaton] = {

    if(other == None) return None
    val o = other.get

    val q02:State2 = (this.q0, o.q0)
    val qf2:State2 = (this.qf, o.qf)

    // Perform Cross Product
    val (intδ, intQ) = this × o

    if(!intQ.contains(q02) || !intQ.contains(qf2)) return None

    // Prune away invalid states
    val (prδ, prQ) = prune[State2](intδ, intQ, q02, qf2)

    // Convert state pairs into simple states
    val (newQ, q0Opt, qfOpt, newδ) = State2toState(prQ, q02, qf2, prδ)

    if(q0Opt == None || qfOpt == None) return None

    val q0 = q0Opt.get
    val qf = qfOpt.get

    Some(new VSetAutomaton(newQ, q0, qf, this.V.union(o.V), newδ))
  }

  /**
    * Returns a vset-automaton that is the ε-closure of this one.
    * @return
    */
  def ε(): VSetAutomaton = {

    var newδ = this.δ

    // A frontier is the set of states being examined. Each state has a
    // set of predecessors, by which it is reached directly with an
    // operations transition
    var cF = Map[State, Predecessors[(State, SVOps[SVOp])]]()
    var nF = Map[State, Predecessors[(State, SVOps[SVOp])]]()

    cF = cF + ((q0, new Predecessors[(State, SVOps[SVOp])]))

    // while it is possible to create a new frontier
    while(cF.size > 0) {

      // advance the frontier, creating connections
      // with each new state's predecessors
      for((q, ps) <- cF) {

        // Get q's outgoing transitions
        val qδ = this.δ.filter((t:Transition[State]) => t.q == q)

        // each new state gets its own list of predecessors
        // all the feasible transitions are followed in order to
        // visit all the graph
        for(t <- qδ if t.q1 != q) {

          if(t.isInstanceOf[OperationsTransition[State]]) {

            val to = t.asInstanceOf[OperationsTransition[State]]

            var newP = new Predecessors[(State, SVOps[SVOp])]

            // Add any new operations carried by this transition
            // to each predecessor, connect new state to
            // the current state's predecessors
            for(p <- ps) {

              val newSVOps = p._2.union(to.S)
              newP = newP + ((p._1, newSVOps))
              newδ = newδ + new OperationsTransition[State](p._1, newSVOps, this.V, t.q1)

            }

            // Add the current state as predecessor of the
            // target of t
            val newSVOps = (new SVOps[SVOp]) ++ to.S
            newP = newP + ((q, newSVOps))

            // Add the new state to the next frontier
            nF = nF + ((to.q1, newP))
          }
          else
            // Follow all the transitions!
            nF = nF + ((t.q1, new Predecessors[(State, SVOps[SVOp])]))

        }
      }

      cF = nF
      nF = Map[State, Predecessors[(State, SVOps[SVOp])]]()
    }

    // For each state, add an epsilon transition to itself
    for(q <- this.Q) {

      newδ = newδ + new OperationsTransition[State](q, new SVOps[SVOp], this.V, q)
    }

    new VSetAutomaton(this.Q, this.q0, this.qf, this.V,  newδ)
  }

  /**
    * Returns the cross product of this transition function with another one.
    * @param other
    * @return
    */
  def ×(other: VSetAutomaton): (TransitionFunction[Transition[State2]], StateSet[State2]) = {

    // The transition function resulting from the product
    var intδ = new TransitionFunction[Transition[State2]]

    // The state set resulting from the product
    var Q2 = new StateSet[State2]

    // Intersect all possible pairs of transitions
    for(t1 <- this.δ; t2 <- other.δ) {

      val t12Opt = t1 & t2

      // If the intersection succeeded
      if(t12Opt != None) {
        val t12 = t12Opt.get
        intδ = intδ + t12
        Q2 = Q2 + t12.q
        Q2 = Q2 + t12.q1
      }

    }

    (intδ, Q2)
  }

  /**
    * Prunes away unreachable and dead states in the provided transition function and state set.
    * @param δ
    * @param Q
    * @param q0
    * @param qf
    * @tparam State
    * @return
    */
  def prune[State](δ:TransitionFunction[Transition[State]], Q:StateSet[State], q0:State, qf:State):(TransitionFunction[Transition[State]], StateSet[State]) = {

    var forwardδ = Map[State, TransitionFunction[Transition[State]]]()
    var backwardδ = Map[State, TransitionFunction[Transition[State]]]()

    var visitedForward = scala.collection.mutable.Map[State, Boolean]()
    var visitedBackward = scala.collection.mutable.Map[State, Boolean]()

    // Build forwardδ for forward traversal
    for(q <- Q) {

      val qδ = δ.filter((t:Transition[State]) => t.q == q)
      forwardδ = forwardδ + ((q, qδ))
    }

    // Build backwardδ for backward traversal
    for(q <- Q) {

      val qδ = δ.filter((t:Transition[State]) => t.q1 == q)

      // Reverse the transitions (for the traversal, we don't care about the original label)
      val qδ1: TransitionFunction[Transition[State]] = qδ.map((t: Transition[State]) => new OrdinaryTransition[State](t.q1, 'a', t.q))

      backwardδ = backwardδ + ((q, qδ1))
    }

    // The two traversals
    traverse(forwardδ, q0, visitedForward)
    traverse(backwardδ, qf, visitedBackward)

    var newQ = new StateSet[State]
    var newδ = new TransitionFunction[Transition[State]]

    // Include only transitions with states visited in both directions
    for(t <- δ) {

      if(visitedForward(t.q) && visitedForward(t.q1) && visitedBackward(t.q) && visitedBackward(t.q1))
        newδ = newδ + t
    }

    // Include only the states visited in both directions
    for(q <- Q) {

      if(visitedForward(q) && visitedForward(q))
        newQ = newQ + q
    }

    (newδ, newQ)
  }

  /**
    * Traverses the graph defined by the provided transition function, tracking the visited states.
    * @param δ
    * @param q0
    * @param visited
    * @tparam State
    * @return
    */
  def traverse[State](δ:Map[State, TransitionFunction[Transition[State]]], q0:State, visited:scala.collection.mutable.Map[State, Boolean])
    :scala.collection.mutable.Map[State, Boolean] = {

    var ts:TransitionFunction[Transition[State]] = null

    if(δ.contains(q0))
      ts = δ(q0)
    else return visited

    for(t <- ts) {

      if(!visited(t.q1)) {

        visited(t.q1) = true
        traverse[State](δ, t.q1, visited)
      }

    }

    visited

  }

  /**
    * Replaces state pairs with simple states in the provided state set and transition function.
    * @param Q2
    * @param q02
    * @param qf2
    * @param δ2
    * @return
    */
  def State2toState(Q2:StateSet[State2], q02:State2, qf2:State2, δ2:TransitionFunction[Transition[State2]])
  : (StateSet[State], Option[State], Option[State], TransitionFunction[Transition[State]]) = {

    var Q = new StateSet[State]
    var δ = new TransitionFunction[Transition[State]]
    var q0: Option[State] = None
    var qf: Option[State] = None

    var stateMap = Map[State2, State]()

    // Create a new simple state for each state pair
    for(q2 <- Q2) {

      val q = new State
      stateMap = stateMap + ((q2, q))
      Q = Q + q

      if(q2 == q02) q0 = Some(q)
      if(q2 == qf2) qf = Some(q)
    }

    // Create a new transition with the simple states associated to the original state pairs
    // as the new source and destination
    for(t <- δ2) {

      val newQ = stateMap(t.q)
      val newQ1 = stateMap(t.q1)

      t match {

        case OrdinaryTransition(q, σ, q1) => {
          δ = δ + new OrdinaryTransition[State](newQ, σ, newQ1)
        }
        case RangeTransition(q, σ, q1) => {
          δ = δ + new RangeTransition[State](newQ, σ, newQ1)
        }
        case OperationsTransition(q, s, v, q1) => {
          δ = δ + new OperationsTransition[State](newQ, s, v, newQ1)
        }
      }
    }

    (Q, q0, qf, δ)
  }

}
