package be.ac.ulb.arc.runtime

import scala.{Int => Position}
import scala.collection.immutable.{HashSet => VSRelation}
import scala.{Array => VSTuple}
import scala.collection.mutable.ArrayBuffer

/**
  * A writer for outputs of NFA programs executed on input strings.
  */
object OutputWriter {

  /**
    * Prints the given tuples spanned from the given program to standard output.
    * @param input
    * @param output
    */
  def printOutput(input:String, output:VSRelation[VSTuple[Position]]):Unit = {

    for(t <- output) {

      var spans = new ArrayBuffer[Position]() ++ t

      while(!spans.isEmpty) {

        val (span, newSpans) = spans.splitAt(2)
        spans = newSpans

        print("(" + span(0) + "," + span(1) + ") ")
      }

      spans = new ArrayBuffer[Position]() ++ t

      while(!spans.isEmpty) {

        val (span, newSpans) = spans.splitAt(2)
        spans = newSpans

        if(span(0) < span(1))
          print(" " + input.substring(span(0), span(1)) + " ")
        else print(" ε ")
      }
      println()
    }
  }
}
