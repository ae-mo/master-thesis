package be.ac.ulb.arc.runtime

import scala.{Int => Position}
import be.ac.ulb.arc.runtime.{StringPointerCollection => VSTuple}
import scala.collection.mutable.ArrayBuffer

import java.io.BufferedWriter

/**
  * A writer for outputs of NFA programs executed on input strings.
  */
object OutputWriter {

  /**
    * Prints the given tuples spanned from the given program to standard output.
    * @param input
    * @param output
    */
  def printOutput(input:String, output:Iterable[VSTuple], lengthConstraint:Int = -1):Unit = {

    for(t <- output) {

      var spans = new ArrayBuffer[Position]() ++ t.toArray

      while(!spans.isEmpty) {

        val (span, newSpans) = spans.splitAt(2)
        spans = newSpans

        print("(" + span(0) + "," + span(1) + ") ")

      }

      spans = new ArrayBuffer[Position]() ++ t.toArray

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

  /**
    * Writes the given tuples spanned from the given program to the given output file.
    * @param input
    * @param output
    * @param outputFileWriter
    */
  def writeOutput(input:String, output:Iterable[VSTuple], outputFileWriter:BufferedWriter, lengthConstraint:Int = -1):Unit = {

    for(t <- output) {

      var spans = new ArrayBuffer[Position]() ++ t.toArray

      var printed = 0

      while(!spans.isEmpty) {

        val (span, newSpans) = spans.splitAt(2)
        spans = newSpans

        outputFileWriter.write("(" + span(0) + "," + span(1) + ") ")

      }

      spans = new ArrayBuffer[Position]() ++ t.toArray

      while(!spans.isEmpty) {

        val (span, newSpans) = spans.splitAt(2)
        spans = newSpans

        if(span(0) < span(1))
          outputFileWriter.write(" " + input.substring(span(0), span(1)) + " ")
        else outputFileWriter.write(" ε ")

      }
      outputFileWriter.write('\n')

    }
  }

}
