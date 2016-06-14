package be.ac.ulb.arc.vset

import be.ac.ulb.arc.runtime.{StringPointerCollection => VSTuple}
import be.ac.ulb.arc.runtime._
import scala.collection.immutable.{HashSet => VSRelation}
import scala.collection.mutable.{ArrayBuffer => Program, Map}
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
