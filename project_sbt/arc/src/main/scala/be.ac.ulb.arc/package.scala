package be.ac.ulb.arc

import core.CoreSpannerFileReader

import java.io._
import scala.io._

/**
  * Created by andrea on 21/06/16.
  */
package object arc {

  val defaultOutputTuplesFileName = "tuples"
  val defaultOutputSpannerFileName = "spanner.csp"

  def main(args: Array[String]):Unit = {

    val coreSpannerFilePattern = ".+\\.csp"
    val AQLSpecificationFilePattern = ".+\\.aqls"
    val illegalCharPattern = ".*[^-_.A-Za-z0-9].*"

    // If the input file is a core spanner file
    if(args.last.matches(coreSpannerFilePattern)) {

      // Get the input spanner
      if(!(new File(args.last)).exists) throw new IllegalArgumentException("The file" + args.last + " does not exist.")
      val spannerOpt = CoreSpannerFileReader.getCoreSpanner(args.last)
      if(spannerOpt == None) throw new IllegalArgumentException("The input spanner is not valid.")
      val spanner = spannerOpt.get

      // look for the input documents folder
      val docsI = args.indexOf("-d")
      if(docsI == "-1") throw new MissingArgumentException("Missing argument: -d")
      val docsFolder = args(docsI + 1)
      val docsFolderFile = new File(docsFolder)
      if(!docsFolderFile.exists) throw new IllegalArgumentException("The folder" + docsFolder + " does not exist.")
      if(!docsFolderFile.isDirectory) throw new IllegalArgumentException(docsFolder + " is not a directory.")

      var outputFileName = defaultOutputTuplesFileName

      // Look for possible output filename
      val outI = args.indexOf("-o")
      if(outI != -1) {

        val fileName = args(outI + 1)
        if(fileName.matches(illegalCharPattern)) throw new IllegalArgumentException("The name" + fileName + " is not a valid filename.")
        outputFileName = fileName
      }

      // Open output file
      val outputFile = new File(outputFileName)
      // Check if it already exists
      if(outputFile.exists) throw new Exception("File " + outputFileName + " already exists.")
      val fw = new FileWriter(outputFile)

      // Get input files
      val inputFileNames = docsFolderFile.listFiles.filter(_.isFile)

      // Evaluate the spanner on the input files
      for(fn <- inputFileNames) {

        val source = Source.fromFile(fn)
        val d = try source.getLines.mkString finally source.close
        if(d.isInstanceOf[String]) {

          val tsOpt = spanner.evaluate(d)
        }
      }

    }
    else if(args.last.matches(AQLSpecificationFilePattern)) {}
    else throw new IllegalArgumentException("Invalid input file: " + args.last)
  }
}

class MissingArgumentException(s:String) extends Exception(s)