package be.ac.ulb.arc.core

import be.ac.ulb.arc.runtime.{StringPointerCollection => VSTuple}
import be.ac.ulb.arc.runtime._

import scala.collection.mutable.{HashSet => VSRelation}
import scala.collection.immutable.{HashSet => SVars}
import scala.collection.mutable.{Map, ArrayBuffer => Program}
import scala.{Int => SVar}
import scala.collection.mutable.{Map => CoreSpannersCollection}
import scala.collection.immutable.{HashSet => SVOps}
import scala.collection.immutable.{HashSet => TransitionFunction}
import scala.collection.immutable.{HashSet => StateSet}
import scala.collection.mutable.{HashSet => RunSet}

/**
  * Represents a core spanner.
  */
class CoreSpanner(val automaton:VSetAutomaton, val equalities:Option[Set[(SVar, SVar)]]) {

  // Convert the vset-automaton into an NFA program
  val prog = new Program[Instruction]()
  var map = scala.collection.immutable.Map[State, Array[Transition[State]]]()

  // Pair each state with its outgoing transitions for performance
  for(q <- automaton.Q)
    map = map + ((q, automaton.δ.filter((t:Transition[State]) => t.q == q && t.q1 == q).toArray ++
      automaton.δ.filter((t:Transition[State]) => t.q == q && t.q1 != q)))

  /**
    * Produces a NFA program representation of the spanner.
    */
  def toNFAProgram(): Unit = {
    automaton.toNFAProgram(prog, Map[State, Int](), automaton.q0, 0)
  }

  /**
    * Evaluates the spanner on the given document.
    *
    * @param doc
    * @return
    */
  def evaluate(doc: String):Option[VSRelation[VSTuple]]= {

    /*val eqs = if(equalities != None) equalities.get else Set[(SVar, SVar)]()

    // Use the virtual machine to span tuples from the document
    val tuplesOpt = VirtualMachine.execute(prog.toArray, automaton.V, eqs, doc, 0, VirtualMachine.processSAVE)

    if(tuplesOpt != None) {

      val tuples = tuplesOpt.get
      return Some(tuples)
    }
    None*/

    run(doc)
  }

  def run(doc:String):Option[VSRelation[VSTuple]] = {

    val runs = new RunSet[Run]
    val tuples = new VSRelation[VSTuple]

    runs += new Run(automaton.q0, automaton.qf, doc, 0, new StringPointerArray(automaton.V), map)

    while(runs.size > 0)
      for(r <- runs)
        r.advance(runs, tuples)

    if(tuples.size == 0)
      return None
    else {

      return Some(tuples)
    }

  }

  /**
    * Performs the projection of the spanner on the desired span variables.
    *
    * @param vars
    * @return
    */
  def π(vars:SVars[SVar]):Option[CoreSpanner] = {

    val res = this.automaton.π(vars)

    return Some(new CoreSpanner(res, this.equalities))
  }

  /**
    * Performs the union of the spanner with the given spanner.
    *
    * @param other
    * @return
    */
  def ∪(other:CoreSpanner):Option[CoreSpanner] = {

    val resOpt = this.automaton.∪(other.automaton)

    if (resOpt == None) return None

    val res = resOpt.get

    val eqs = getEqualities(this.equalities, other.equalities)

    Some(new CoreSpanner(res, eqs))
  }

  /**
    * Performs the join of the spanner with the given spanner.
    *
    * @param other
    * @return
    */
  def ⋈(other:CoreSpanner):Option[CoreSpanner] = {

    val resOpt = this.automaton.⋈(other.automaton)

    if (resOpt == None) return None

    val res = resOpt.get

    val eqs = getEqualities(this.equalities, other.equalities)

    Some(new CoreSpanner(res, eqs))
  }

  /**
    * Performs the string equality of the spanner on the given pair of span variables.
    *
    * @param var1
    * @param var2
    * @return
    */
  def ς(var1:SVar, var2:SVar):Option[CoreSpanner] = {

    var eqs:Option[Set[(SVar, SVar)]] = None

    // Add existing equalities
    var eqSet = Set[(SVar, SVar)]()
    if(this.equalities != None)
      eqSet = eqSet ++ this.equalities.get

    // Add new equalities
    eqSet = eqSet + ((var1, var2))
    eqs = Some(eqSet)

    Some(new CoreSpanner(this.automaton, eqs))
  }

  /**
    * Returns the spanner that spans the s-tuples in which the span assigned to
    * var1 is distant from the span assigned to var2 min to max characters.
    *
    * @param other
    * @param var1
    * @param var2
    * @param min
    * @param max
    * @return
    */
  def isWithin(other:CoreSpanner, var1:SVar, var2:SVar, min:Int, max:Int, var3:SVar):Option[CoreSpanner] = {

    val followsOpt = this.follows(other, var1, var2, min, max, var3)
    val precedesOpt = other.follows(this, var2, var1, min, max, var3)

    if(followsOpt != None && precedesOpt != None)
      return followsOpt.get ∪ precedesOpt.get
    else if(followsOpt != None)
      return followsOpt
    else if(precedesOpt != None)
      return precedesOpt
    else
      return None
  }

  /**
    * Returns the spanner that spans the s-tuples in which the span assigned to
    * var1 follows the span assigned to var2 within min to max characters.
    *
    * @param other
    * @param var1 Has to be a span variable from this spanner, not in the other.
    * @param var2 Has to be a span variable from the other spanner, not in this one.
    * @param min
    * @param max
    * @return
    */
  def follows(other:CoreSpanner, var1:SVar, var2:SVar, min:Int, max:Int, var3:SVar):Option[CoreSpanner] = {

    if(!this.automaton.V.contains(var1) || this.automaton.V.contains(var2) ||
      other.automaton.V.contains(var1) || !other.automaton.V.contains(var2)) return None

    // Create the spanner that joins this spanner with the other based on the relative
    // distance of their s-tuples
    var Q = new StateSet[State]
    var δ = new TransitionFunction[Transition[State]]
    var V = new SVars[SVar] ++ this.automaton.V ++ other.automaton.V + var3
    val q0 = new State
    val qf = new State
    Q = Q + q0 + qf

    //Add states and transitions
    // Unanchored matching
    δ = δ + new RangeTransition[State](q0, new Range(Char.MinValue, Char.MaxValue), V, q0)
    δ = δ + new RangeTransition[State](qf, new Range(Char.MinValue, Char.MaxValue), V, qf)
    // Open the first variable and enclosing variable
    var s:State = new State
    Q = Q + s
    var S:SVOps[SVOp] = new SVOps + new SVOp(var1, ⊢) + new SVOp(var3, ⊢)
    δ = δ + new OperationsTransition[State](q0, S, V, s)
    // Span the first variable
    δ = δ + new RangeTransition[State](s, new Range(Char.MinValue, Char.MaxValue), V, s)
    // Close the first variable
    var s1:State = new State
    Q = Q + s1
    S = new SVOps + new SVOp(var1, ⊣)
    δ = δ + new OperationsTransition[State](s, S, V, s1)
    // Match the minimum amount of characters required
    var i = 1
    s = s1
    while(i <= min) {

      s1 = new State
      Q = Q + s1
      δ = δ + new RangeTransition[State](s, new Range(Char.MinValue, Char.MaxValue), V, s1)
      s = s1
      i += 1
    }
    // Match the characters between min and max
    i = min + 1
    S = new SVOps[SVOp] + new SVOp(var2, ⊢)
    var s2 = new State
    Q = Q + s2
    δ = δ + new OperationsTransition[State](s, S, V, s2)
    while (i <= max) {

      // Match one character
      s1 = new State
      Q = Q + s1
      δ = δ + new RangeTransition[State](s, new Range(Char.MinValue, Char.MaxValue), V, s1)
      // Or go to the second variable
      δ = δ + new OperationsTransition[State](s, S, V, s2)
      s = s1
      i += 1
    }
    δ = δ + new OperationsTransition[State](s, S, V, s2)
    // Span the second variable
    δ = δ + new RangeTransition[State](s2, new Range(Char.MinValue, Char.MaxValue), V, s2)
    // Close the second variable and the enclosing one and match
    S = new SVOps + new SVOp(var2, ⊣) + new SVOp(var3, ⊣)
    δ = δ + new OperationsTransition[State](s2, S, V, qf)

    // Create the vset-automaton
    val joinA = new VSetAutomaton(Q, q0, qf, V, δ)
    // Create the spanner
    val joinS = new CoreSpanner(joinA, None)

    // do the join between this spanner and the context spanner
    val j1Opt = this ⋈ joinS
    if(j1Opt == None) return None
    val j1 = j1Opt.get

    // Join the result of the first join with the other spanner
    val j2Opt = j1 ⋈ other
    if(j2Opt == None) return None
    // Project on the enclosing variable
    else j2Opt.get.π(new SVars + var3)
  }

  /**
    * Adds a span variable which captures the next or the previous min to max characters to the given span veriable.
    * @param v
    * @param jV
    * @param min
    * @param max
    * @param forward
    * @return
    */
  def addIsFollowedBy(v:SVar, jV:SVar, min:Int, max:Int, forward:Boolean = true): CoreSpanner ={

    var q0 = automaton.q0
    var qf = automaton.qf

    val V = new SVars[SVar] + v + jV

    if(forward) {

      q0 = qf
      qf = new State

      var (newQ, newδ) = distanceConstraint(q0, qf, V, jV, min, max)

      newQ = newQ + q0 + qf

      newδ = newδ + new RangeTransition[State](qf, new Range(Char.MinValue, Char.MaxValue), V, qf)

      val loops = automaton.δ.filter((t:Transition[State]) => t.q == q0 && t.q1 == q0)

      new CoreSpanner(new VSetAutomaton(newQ ++ automaton.Q, automaton.q0, qf,  automaton.V + jV, automaton.δ -- loops ++ newδ), equalities)

    }
    else {

      qf = q0
      q0 = new State

      var (newQ, newδ) = distanceConstraint(q0, qf, V, jV, min, max)

      newQ = newQ + q0 + qf

      newδ = newδ + new RangeTransition[State](q0, new Range(Char.MinValue, Char.MaxValue), V, q0)

      val loops = automaton.δ.filter((t:Transition[State]) => t.q == qf && t.q1 == qf)

      new CoreSpanner(new VSetAutomaton(newQ ++ automaton.Q, q0, automaton.qf, automaton.V + jV, automaton.δ -- loops ++ newδ), equalities)
    }
  }

  /**
    * Creates the necessary states and transitions to add an 'isFollowedBy' context span variable.
    * @param q0
    * @param qf
    * @param V
    * @param v
    * @param min
    * @param max
    * @return
    */
  def distanceConstraint(q0:State, qf:State, V:SVars[SVar], v:SVar, min:Int, max:Int): (StateSet[State], TransitionFunction[Transition[State]]) = {

    var Q = new StateSet[State]
    var δ = new TransitionFunction[Transition[State]]

    // Open the distance constraint variable
    var s = new State
    Q = Q + s
    var S:SVOps[SVOp] = new SVOps + new SVOp(v, ⊢)
    δ = δ + new OperationsTransition[State](q0, S, V, s)

    // Match the minimum amount of characters required
    var i = 1
    var s1 = s
    while(i <= min) {

      s1 = new State
      Q = Q + s1
      δ = δ + new RangeTransition[State](s, new Range(Char.MinValue, Char.MaxValue), V, s1)
      s = s1
      i += 1
    }
    // Match the characters between min and max
    i = min + 1
    S = new SVOps[SVOp]  + new SVOp(v, ⊣)
    var s2 = qf
    δ = δ + new OperationsTransition[State](s, S, V, s2)
    while (i <= max) {

      // Match one character
      s1 = new State
      Q = Q + s1
      δ = δ + new RangeTransition[State](s, new Range(Char.MinValue, Char.MaxValue), V, s1)
      // Or go to final state
      δ = δ + new OperationsTransition[State](s, S, V, s2)
      s = s1
      i += 1
    }

    δ = δ + new OperationsTransition[State](s, S, V, s2)

    (Q, δ)
  }

  /**
    * Merges Two sets of string equality selections.
    *
    * @param eqs1
    * @param eqs2
    * @return
    */
  def getEqualities(eqs1:Option[Set[(SVar, SVar)]], eqs2:Option[Set[(SVar, SVar)]]):Option[Set[(SVar, SVar)]] = {

    if(eqs1 == None && eqs2 == None) None
    else if(eqs1 != None) this.equalities
    else if(eqs2 != None) eqs2
    else Some(eqs1.get ++ eqs2.get)
  }

  /**
    * Returns an executable string representation of the spanner.
    *
    * @return
    */
  override def toString(): String = {

    var stateMap = Map[State, Int]()
    var counter = 2
    stateMap += ((automaton.q0, 0))
    stateMap += ((automaton.qf, 1))

    var s = ""
    s += automaton.Q.size.toString + '\n'
    s += "0" + '\n'
    s += "1" + '\n'
    for(v <- automaton.V)
      s += v.toString + " "
    s += '\n'

    if(equalities != None) {

      val eqs = equalities.get

      for(e <- eqs) {

        s += "(" + e._1 + "," + e._2 + ")\n"
      }
    }
    s += "-" + '\n'
    for(t <- automaton.δ) {

      var sQ = ""
      var dQ = ""

      if(stateMap.contains(t.q))
        sQ += stateMap(t.q)
      else {
        stateMap += ((t.q, counter))
        sQ += counter
        counter += 1
      }

      if(stateMap.contains(t.q1))
        dQ += stateMap(t.q1)
      else {
        stateMap += ((t.q1, counter))
        dQ += counter
        counter += 1
      }

      val label:String = t match {

        case OrdinaryTransition(q, σ, v, q1) => {
          σ.toString
        }
        case RangeTransition(q, σ, v, q1) => {
          "(" + σ.min.toChar + ", " + σ.max.toChar + ")"
        }
        case OperationsTransition(q, s, v, q1) => {
          var l = "{"

          var sA = s.toArray
          var o:SVOp = null

          if (sA.length > 0) {
            o = sA(0)
            l += o.x + (if (o.t == ⊢) "⊢" else "⊣")
          }

          if(sA.length > 1)
            for(i <- 1 until sA.length) {
              o = sA(i)
              l +=  ", " + o.x + (if(o.t == ⊢) "⊢" else "⊣")
            }

          l += "}"

          l
        }
      }

      s += sQ + " " + label + " " + dQ + '\n'
    }

    s
  }
}

/**
  * Represents a generator of a core spanner.
  */
object CoreSpannerGenerator {

  /**
    * Generates a core spanner from an AQL core fragment specification.
    *
    * @param spec
    * @return
    */
  def generate(spec:AQLCoreFragmentSpecification):CoreSpanner = {

    var spanners = CoreSpannersCollection[String, CoreSpanner]()
    var result:CoreSpanner = null

    spanners = spanners ++ spec.spanners
    // Perform the operations on the input spanners
    for(op <- spec.operations) {

      val resultOpt = op.perform(spanners)

      if(resultOpt == None) throw new Exception("Could not generate the spanner: " + op.res + ".")

      result = resultOpt.get

      spanners += ((op.res, result))
    }

    // Generate the NFA program
    result.toNFAProgram
    result
  }
}
