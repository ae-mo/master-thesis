package be.ac.ulb.arc.core

import be.ac.ulb.arc.runtime.{StringPointerCollection => VSTuple}
import be.ac.ulb.arc.runtime._

import scala.collection.immutable.{HashSet => VSRelation}
import scala.collection.immutable.{HashSet => SVars}
import scala.collection.mutable.{Map, ArrayBuffer => Program}
import scala.{Int => SVar}
import scala.collection.mutable.{Map => CoreSpannersCollection}
import scala.collection.immutable.{HashSet => SVOps}
import scala.collection.immutable.{HashSet => TransitionFunction}
import scala.collection.immutable.{HashSet => StateSet}

/**
  * Represents a core spanner.
  */
class CoreSpanner(val automaton:VSetAutomaton, val equalities:Option[Set[(SVar, SVar)]]) {

  // Convert the vset automaton into an NFA program
  val prog = new Program[Instruction]()
  automaton.toNFAProgram(prog, Map[State, Int](), automaton.q0, 0)
  /**
    * Evaluates the spanner on the given document.
    * @param doc
    * @return
    */
  def evaluate(doc: String):Option[VSRelation[VSTuple]]= {

    val eqs = if(equalities != None) equalities.get else Set[(SVar, SVar)]()

    // Use the virtual machine to span tuples from the document
    val tuplesOpt = VirtualMachine.execute(prog.toArray, automaton.V, eqs, doc, 0, VirtualMachine.processSAVE)

    if(tuplesOpt != None) {

      val tuples = tuplesOpt.get
      return Some(tuples)
    }
    None
  }

  /**
    * Performs the projection of the spanner on the desired span variables.
    * @param vars
    * @return
    */
  def π(vars:SVars[SVar]):Option[CoreSpanner] = {

    val res = this.automaton.π(vars)

    return Some(new CoreSpanner(res, this.equalities))
  }

  /**
    * Performs the union of the spanner with the given spanner.
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
    * @param other
    * @param var1
    * @param var2
    * @param min
    * @param max
    * @return
    */
  def isWithin(other:CoreSpanner, var1:SVar, var2:SVar, min:Int, max:Int):Option[CoreSpanner] = {

    val followsOpt = this.follows(other, var1, var2, min, max)
    val precedesOpt = other.follows(this, var2, var1, min, max)

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
    * @param other
    * @param var1 Has to be a span variable from this spanner, not in the other.
    * @param var2 Has to be a span variable from the other spanner, not in this one.
    * @param min
    * @param max
    * @return
    */
  def follows(other:CoreSpanner, var1:SVar, var2:SVar, min:Int, max:Int):Option[CoreSpanner] = {

    if(!this.automaton.V.contains(var1) || this.automaton.V.contains(var2) ||
      other.automaton.V.contains(var1) || !other.automaton.V.contains(var2)) return None

    // do the join between the input spanners
    val j1Opt = this ⋈ other
    if(j1Opt == None) return None
    val j1 = j1Opt.get

    // Create the spanner that joins this spanner with the other based on the relative
    // distance of their s-tuples
    var Q = new StateSet[State]
    var δ = new TransitionFunction[Transition[State]]
    var V = new SVars[SVar] ++ this.automaton.V ++ other.automaton.V
    val q0 = new State
    val qf = new State
    Q = Q + q0 + qf

    //Add states and transitions
    // Unanchored matching
    δ = δ + new RangeTransition[State](q0, new Range(Char.MinValue, Char.MaxValue), V, q0)
    δ = δ + new RangeTransition[State](qf, new Range(Char.MinValue, Char.MaxValue), V, qf)
    // Open the first variable
    var s:State = new State
    Q = Q + s
    var S:SVOps[SVOp] = new SVOps + new SVOp(var1, ⊢)
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
    // Close the second variable and match
    S = new SVOps + new SVOp(var2, ⊣)
    δ = δ + new OperationsTransition[State](s2, S, V, qf)

    // Create the vset-automaton
    val joinA = new VSetAutomaton(Q, q0, qf, V, δ)
    // Create the spanner
    val joinS = new CoreSpanner(joinA, None)

    // Join the result of the first join with the created spanner
    val j2Opt = j1 ⋈ joinS
    j2Opt
  }

  /**
    * Merges Two sets of string equality selections.
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
}

/**
  * Represents a generator of a core spanner.
  */
object CoreSpannerGenerator {

  /**
    * Generates a core spanner from an AQL core fragment specification.
    * @param spec
    * @return
    */
  def generate(spec:AQLCoreFragmentSpecification):CoreSpanner = {

    val spanners = CoreSpannersCollection[String, CoreSpanner]()
    var result:CoreSpanner = null

    // Perform the operations on the input spanners
    for(op <- spec.operations) {

      val resultOpt = op.perform(spanners)

      if(resultOpt == None) throw new Exception("Could not generate the spanner.")

      result = resultOpt.get

      spanners += ((op.res, result))
    }

    result
  }
}
