package be.ac.ulb.arc

import core.CoreSpannerFileReader
import runtime.OutputWriter
import core.AQLCoreFragmentSpecificationReader
import core.CoreSpannerGenerator

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
    val illegalCharPattern = ".*[^a-zA-Z0-9_./-].*"

    try {

      if(args.size == 0) throw new MissingArgumentException("Not enough arguments.")
      // If the input file is a core spanner file
      if(args.last.matches(coreSpannerFilePattern)) {

        // Get the input spanner
        if(!(new File(args.last)).exists) throw new IllegalArgumentException("The file" + args.last + " does not exist.")
        val spannerOpt = CoreSpannerFileReader.getCoreSpanner(args.last)
        if(spannerOpt == None) throw new IllegalArgumentException("The input spanner is not valid.")
        val spanner = spannerOpt.get

        // look for the input documents folder
        val docsI = args.indexOf("-d")
        if(docsI == -1) throw new MissingArgumentException("Missing argument: -d")
        val docsFolder = args(docsI + 1)
        val docsFolderFile = new File(docsFolder)
        if(!docsFolderFile.exists) throw new IllegalArgumentException("The folder" + docsFolder + " does not exist.")
        if(!docsFolderFile.isDirectory) throw new IllegalArgumentException(docsFolder + " is not a directory.")

        var outputFileName = defaultOutputTuplesFileName

        // Look for possible output filename
        val outI = args.indexOf("-o")
        if(outI != -1) {

          val fileName = args(outI + 1)
          if(fileName.matches(illegalCharPattern)) throw new IllegalArgumentException("The name " + fileName + " is not a valid filename.")
          outputFileName = fileName
        }

        // Open output file
        val outputFile = new File(outputFileName)
        // Check if it already exists
        if(outputFile.exists) throw new Exception("File " + outputFileName + " already exists.")
        val fw = new FileWriter(outputFile, true)

        // Get input files
        val inputFiles = docsFolderFile.listFiles.filter(_.isFile)

        // Evaluate the spanner on the input files
        for(f <- inputFiles) {

          val source = Source.fromFile(f, "ISO-8859-1")
          val d = try source.getLines.mkString + '\0' finally source.close

          if(d.isInstanceOf[String]) {

            // Evaluate the spanner on the document and write
            // resulting tuples to output
            val tsOpt = spanner.evaluate(d)
            if(tsOpt != None) {

              fw.write("-----------------------------" + f.getName + "-----------------------------\n")
              OutputWriter.writeOutput(d, tsOpt.get, fw)
              fw.flush
            }
          }
        }
        fw.close
      }
      else if(args.last.matches(AQLSpecificationFilePattern)) {

        // Get the specification
        if (!(new File(args.last)).exists) throw new IllegalArgumentException("The file" + args.last + " does not exist.")
        val specOpt = AQLCoreFragmentSpecificationReader.getSpecification(args.last)
        if (specOpt == None) throw new IllegalArgumentException("The input specification is not valid.")
        val spec = specOpt.get

        val compileI = args.indexOf("-c")

        // Execute the AQL specification
        if (compileI == -1) {

          // look for the input documents folder
          val docsI = args.indexOf("-d")
          if (docsI == -1) throw new MissingArgumentException("Missing argument: -d")
          val docsFolder = args(docsI + 1)
          val docsFolderFile = new File(docsFolder)
          if (!docsFolderFile.exists) throw new IllegalArgumentException("The folder" + docsFolder + " does not exist.")
          if (!docsFolderFile.isDirectory) throw new IllegalArgumentException(docsFolder + " is not a directory.")

          var outputFileName = defaultOutputTuplesFileName

          // Look for possible output filename
          val outI = args.indexOf("-o")
          if (outI != -1) {

            val fileName = args(outI + 1)
            if (fileName.matches(illegalCharPattern)) throw new IllegalArgumentException("The name " + fileName + " is not a valid filename.")
            outputFileName = fileName
          }

          // Open output file
          val outputFile = new File(outputFileName)
          // Check if it already exists
          if (outputFile.exists) throw new Exception("File " + outputFileName + " already exists.")
          val fw = new FileWriter(outputFile, true)

          // Get input files
          val inputFiles = docsFolderFile.listFiles.filter(_.isFile)

          // Evaluate the specification on the input files
          for (f <- inputFiles) {

            val source = Source.fromFile(f, "ISO-8859-1")
            val d = try source.getLines.mkString  + '\0' finally source.close

            if (d.isInstanceOf[String]) {

              // Evaluate the spanner on the document and write
              // resulting tuples to output
              val tsOpt = spec.evaluate(d)
              if (tsOpt != None) {
                fw.write("-----------------------------" + f.getName + "-----------------------------\n")
                OutputWriter.writeOutput(d, tsOpt.get, fw)
                fw.flush
              }
            }
          }
          fw.close
        }
        else {

          var outputFileName = defaultOutputSpannerFileName

          // Look for possible output filename
          val outI = args.indexOf("-o")
          if (outI != -1) {

            val fileName = args(outI + 1)
            if (fileName.matches(illegalCharPattern)) throw new IllegalArgumentException("The name " + fileName + " is not a valid filename.")
            outputFileName = fileName + ".csp"
          }

          // Open output file
          val outputFile = new File(outputFileName)
          // Check if it already exists
          if (outputFile.exists) throw new Exception("File " + outputFileName + " already exists.")

          val spanner = CoreSpannerGenerator.generate(spec)

          import java.nio.file.{Paths, Files}
          import java.nio.charset.StandardCharsets

          Files.write(Paths.get(outputFileName), spanner.toString.getBytes(StandardCharsets.UTF_8))

        }
      }
      else throw new IllegalArgumentException("Invalid input file: " + args.last)
    }
    catch {

      case e:Exception => {

        println("ERROR: " + e.getMessage)
        e.printStackTrace
      }
    }


  }
}

class MissingArgumentException(s:String) extends Exception(s)