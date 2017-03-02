package be.ac.ulb.arc

import be.ac.ulb.arc.core.{AQLCoreFragmentSpecificationReader, CoreSpannerFileReader, CoreSpannerGenerator}
import be.ac.ulb.arc.runtime.OutputWriter
import org.scalatest.FunSuite

import scala.io.Source
/**
  * Created by andrea on 22/06/16.
  */
class mainTestSuite extends FunSuite{

  val data = new {

    val args = new Array[String](5)
    args(0) = "-d"
    args(1) = "src/test/scala/be.ac.ulb.arc/benchmark/excerpt"
    args(2) = "-o"
    args(3) = "/home/output/output.txt"
    args(4) = "src/test/scala/be.ac.ulb.arc/benchmark/title.csp"

    val args1 = new Array[String](5)
    args1(0) = "-d"
    args1(1) = "src/test/scala/be.ac.ulb.arc/benchmark/excerpt"
    args1(2) = "-o"
    args1(3) = "/home/output/output.txt"
    args1(4) = "src/test/scala/be.ac.ulb.arc/benchmark/movieFollowsTitle.aqls"

    val args2 = new Array[String](4)
    args2(0) = "-c"
    args2(1) = "-o"
    args2(2) = "/tmp/movieFollowedByTitle"
    args2(3) = "src/test/scala/be.ac.ulb.arc/benchmark/movieFollowsTitle.aqls"

    val args3 = new Array[String](6)
    args3(0) = "-d"
    args3(1) = "src/test/scala/be.ac.ulb.arc/benchmark/excerpt"
    args3(2) = "-s"
    args3(3) = "-o"
    args3(4) = "/home/output/output.txt"
    args3(5) = "src/test/scala/be.ac.ulb.arc/benchmark/test2.aqls"

    val args5 = new Array[String](12)
    args5(0) = "-d"
    args5(1) = "src/test/scala/be.ac.ulb.arc/benchmark/excerpt"
    args5(2) = "-s"
    args5(3) = "-o"
    args5(4) = "/tmp/output.txt"
    args5(5) = "-C"
    args5(6) = "10"
    args5(7) = "-b"
    args5(8) = "(10, 170, 10)"
    args5(9) = "-l"
    args5(10) = "70"
    args5(11) = "src/test/scala/be.ac.ulb.arc/benchmark/test5.aqls"

    val args6 = new Array[String](4)
    args6(0) = "-c"
    args6(1) = "-o"
    args6(2) = "/tmp/spanner"
    args6(3) = "src/test/scala/be.ac.ulb.arc/benchmark/test5.aqls"

    val args7 = new Array[String](9)
    args7(0) = "-d"
    args7(1) = "src/test/scala/be.ac.ulb.arc/benchmark/excerpt"
    args7(2) = "-o"
    args7(3) = "/tmp/output1.txt"
    args7(4) = "-C"
    args7(5) = "10"
    args7(6) = "-b"
    args7(7) = "(10, 170, 1)"
    args7(8) = "src/test/scala/be.ac.ulb.arc/benchmark/test5.aqls"

    val args8 = new Array[String](9)
    args8(0) = "-d"
    args8(1) = "src/test/scala/be.ac.ulb.arc/benchmark/excerpt"
    args8(2) = "-o"
    args8(3) = "/tmp/output.txt"
    args8(4) = "-C"
    args8(5) = "10"
    args8(6) = "-b"
    args8(7) = "(10, 170, 1)"
    args8(8) = "src/test/scala/be.ac.ulb.arc/benchmark/spanner.csp"

    val args9 = new Array[String](4)
    args9(0) = "-c"
    args9(1) = "-o"
    args9(2) = "/tmp/spanner2"
    args9(3) = "src/test/scala/be.ac.ulb.arc/benchmark/test2.aqls"

    val args10 = new Array[String](9)
    args10(0) = "-d"
    args10(1) = "src/test/scala/be.ac.ulb.arc/benchmark/excerpt"
    args10(2) = "-o"
    args10(3) = "/tmp/output1.txt"
    args10(4) = "-C"
    args10(5) = "7"
    args10(6) = "-b"
    args10(7) = "(7, 170, 1)"
    args10(8) = "src/test/scala/be.ac.ulb.arc/benchmark/spanner2.csp"

    val args11 = new Array[String](9)
    args11(0) = "-d"
    args11(1) = "src/test/scala/be.ac.ulb.arc/benchmark/excerpt"
    args11(2) = "-o"
    args11(3) = "/tmp/output2.txt"
    args11(4) = "-C"
    args11(5) = "7"
    args11(6) = "-b"
    args11(7) = "(7, 170, 1)"
    args11(8) = "src/test/scala/be.ac.ulb.arc/benchmark/test2.aqls"

    val args12 = new Array[String](4)
    args12(0) = "-c"
    args12(1) = "-o"
    args12(2) = "/tmp/sentimentOrGenreFollowedByMovie"
    args12(3) = "src/test/scala/be.ac.ulb.arc/benchmark/sentimentOrGenreFollowedByMovie.aqls"

  }

  test("arc should correctly run a spanner on the input documents") {

    Main.main(data.args)
  }

  test("arc should correctly run an AQL specification on the input documents") {

    Main.main(data.args1)
  }

  test("arc should correctly compile an AQL specification into a spanner") {

    Main.main(data.args2)
    val spanner = CoreSpannerFileReader.getCoreSpanner(data.args2(2) + ".csp").get
    println(spanner.toString())
  }

  test("the (V, s)-relation spanned by an AQL specification should equal the one spanned by the corresponding spanner") {

    val spec = AQLCoreFragmentSpecificationReader.getSpecification(data.args2(3)).get
    val spanner = CoreSpannerGenerator.generate(spec)

    val source = Source.fromFile(data.args1(1) + "/317450.female.25.indUnk.Taurus.xml", "ISO-8859-1")
    val d = try source.getLines.mkString + '\0' finally source.close

    val s1Opt = spec.evaluate(d)
    val s1 = s1Opt.get

    val s2Opt = spanner.evaluate(d)
    assert(s2Opt != None)
    val s2 = s2Opt.get

    assert(s1 == s2)
  }

  test("the (V, s)-relation spanned by an AQL specification (with specialized join on 'follows') should equal the one spanned by the corresponding spanner") {

    val spec = AQLCoreFragmentSpecificationReader.getSpecification(data.args2(3), true).get
    val spanner = CoreSpannerGenerator.generate(spec)

    val source = Source.fromFile(data.args1(1) + "/317450.female.25.indUnk.Taurus.xml", "ISO-8859-1")
    val d = try source.getLines.mkString + '\0' finally source.close

    val s1Opt = spec.evaluate(d)
    val s1 = s1Opt.get

    val s2Opt = spanner.evaluate(d)
    assert(s2Opt != None)
    val s2 = s2Opt.get

    assert(s1 == s2)
  }

  test("consolidation + block + length constraint") {

    Main.main(data.args7)

  }

  test("AQL specification test5 should correctly get compiled into a core spanner") {

    Main.main(data.args6)

  }

  test("consolidation + block + length constraint with spanner") {

    Main.main(data.args8)

  }

  test("AQL specification test2 should correctly get compiled into a core spanner") {

    Main.main(data.args9)
  }

  test("consolidation + block + length constraint with spanner2") {

    Main.main(data.args10)

  }

  test("consolidation + block + length constraint with test2") {

    Main.main(data.args11)

  }

  test("sentimentOrGenreFollowedByMovie should be correctly compiled into a core spanner") {

    Main.main(data.args12)
  }
}
