package be.ac.ulb.arc.vset

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Map => CoreSpannersCollection}
import scala.{Array => Operations}
import scala.util.control.Breaks._

/**
  * Represents a specification of a core spanner.
  * @param spanners
  * @param operations
  */
class AQLCoreFragmentSpecification(val spanners:CoreSpannersCollection[String, CoreSpanner], val operations:Operations[Operation])

/**
  * Represents a reader for a specification of a core spanners.
  */
object AQLCoreFragmentSpecificationReader {

  /**
    * Returns the core spanner specification contained in the specified file.
    * @param file
    * @return
    */
  def getSpecification(file: String): Option[AQLCoreFragmentSpecification] = {

    import scala.io.Source

    val source = Source.fromFile(file, "UTF-8")
    val lineIterator = source.getLines

    // The collection of core spanners
    val spanners = CoreSpannersCollection[String, CoreSpanner]()
    // The series of operations to perform
    val opsBuffer = new ArrayBuffer[Operation]()

    val automatonPattern = "\\s*(.+)\\s*=\\s*(.+)\\s*".r

    var line = lineIterator.next()

    // Acquire the spanners
    while(line != "-") {

      val automatonPattern(name, fileName) = line

      val spannerOpt = CoreSpannerFileReader.getCoreSpanner(fileName)

      if(spannerOpt == None) throw new Exception("Spanner " + name + "not found or invalid.")

      spanners += ((name, spannerOpt.get))

    }

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

    val ops = opsBuffer.toArray

    Some(new AQLCoreFragmentSpecification(spanners, ops))
  }
}