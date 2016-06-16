package be.ac.ulb.arc.vset

import be.ac.ulb.arc.runtime.{StringPointerCollection => VSTuple}
import be.ac.ulb.arc.runtime._

import scala.collection.immutable.{HashSet => VSRelation}
import scala.collection.immutable.{HashSet => SVars}
import scala.collection.mutable.{Map, ArrayBuffer => Program}
import scala.{Int => SVar}
import scala.collection.mutable.{Map => CoreSpannersCollection}

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
