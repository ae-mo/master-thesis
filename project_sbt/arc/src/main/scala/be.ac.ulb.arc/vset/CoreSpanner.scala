package be.ac.ulb.arc.vset

import scala.collection.mutable.{Map => CoreSpannersCollection}
import scala.{Int => SVar}
import scala.{Array => Operations}

/**
  * Represents a core spanner.
  */
class CoreSpanner(val automaton:VSetAutomaton, val equalities:Option[Set[(SVar, SVar)]])

/**
  * Represents a generator of a core spanner.
  */
object CoreSpannerGenerator {

  /**
    * Generates a core spanner from an AQL core fragment specification.
    * @param spec
    * @return
    */
  def generate(spec:AQLCoreFragmentSpecification):Option[CoreSpanner] = {

    var result:CoreSpanner = null

    // Perform the operations on the input spanners
    for(op <- spec.operations) {


    }
    None
  }
}
