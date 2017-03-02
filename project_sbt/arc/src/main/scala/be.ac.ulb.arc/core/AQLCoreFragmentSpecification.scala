package be.ac.ulb.arc.core

import scala.collection.mutable.{HashSet => VSRelation}
import be.ac.ulb.arc.runtime.{StringPointerCollection => VSTuple}
import be.ac.ulb.arc.runtime.ClassicalInterpreter

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Map => CoreSpannersCollection}
import scala.{Array => Operations}
import scala.util.control.Breaks._

/**
  * Represents a specification of a core spanner.
  * @param spanners
  * @param operations
  */
class AQLCoreFragmentSpecification(val spanners:CoreSpannersCollection[String, CoreSpanner], val operations:Operations[Operation]) {

  // Obtain the NFA program representations of the base spanners
  for((k, s) <- spanners)
    s.toNFAProgram

  /**
    * Executes this AQL specification on a given input document.
    * @param doc
    * @param lazyEv
    * @return
    */
  def evaluate(doc:String, lazyEv:Boolean = false):Option[VSRelation[VSTuple]] = {

    ClassicalInterpreter.execute(this, doc, lazyEv)
  }
}

/**
  * Represents a reader for a specification of a core spanners.
  */
object AQLCoreFragmentSpecificationReader {

  /**
    * Returns the core spanner specification contained in the specified file.
    * @param file
    * @return
    */
  def getSpecification(file: String, specialize:Boolean = false): Option[AQLCoreFragmentSpecification] = {

    import scala.io.Source

    val source = Source.fromFile(file, "UTF-8")
    val lineIterator = source.getLines

    // The collection of core spanners
    val spanners = CoreSpannersCollection[String, CoreSpanner]()
    // The series of operations to perform
    val opsBuffer = new ArrayBuffer[Operation]()

    val automatonPattern = "\\s*(\\S+)\\s*=\\s*(\\S+)\\s*".r

    var line = lineIterator.next()

    // Acquire the spanners
    while(line != "-") {

      val automatonPattern(name, fileName) = line

      val spannerOpt = CoreSpannerFileReader.getCoreSpanner(fileName)

      if(spannerOpt == None) throw new Exception("Spanner " + name + "not found or invalid.")

      spanners += ((name, spannerOpt.get))

      line = lineIterator.next()
    }

    // Set the specialization flags to true
    if(specialize)
      OperationExtractors.specializeJoin = true

    // Acquire the operations
    for(line <- lineIterator) {

      breakable {

        for(i <- 0 until OperationExtractors.size) {

          val ext = OperationExtractors(i)

          val opOpt = ext.extract(line)

          if(opOpt != None) {

            opsBuffer += opOpt.get
            break
          }

        }

      }

    }

    source.close

    val ops = opsBuffer.toArray

    Some(new AQLCoreFragmentSpecification(spanners, ops))
  }

}