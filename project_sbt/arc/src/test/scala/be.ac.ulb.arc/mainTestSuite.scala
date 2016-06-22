package be.ac.ulb.arc

import be.ac.ulb.arc.core.{AQLCoreFragmentSpecificationReader, CoreSpannerFileReader, CoreSpannerGenerator}
import org.scalatest.FunSuite

import scala.io.Source
/**
  * Created by andrea on 22/06/16.
  */
class mainTestSuite extends FunSuite{

  val data = new {

    val args = new Array[String](5)
    args(0) = "-d"
    args(1) = "/home/andrea/Documenti/ulb/thesis/amorcian/project_sbt/arc/src/test/scala/be.ac.ulb.arc/benchmark/excerpt"
    args(2) = "-o"
    args(3) = "/home/andrea/Documenti/ulb/thesis/output/output.txt"
    args(4) = "src/test/scala/be.ac.ulb.arc/benchmark/title.csp"

    val args1 = new Array[String](5)
    args1(0) = "-d"
    args1(1) = "/home/andrea/Documenti/ulb/thesis/amorcian/project_sbt/arc/src/test/scala/be.ac.ulb.arc/benchmark/excerpt"
    args1(2) = "-o"
    args1(3) = "/home/andrea/Documenti/ulb/thesis/output/output.txt"
    args1(4) = "src/test/scala/be.ac.ulb.arc/benchmark/movieFollowsTitle.aqls"

    val args2 = new Array[String](4)
    args2(0) = "-c"
    args2(1) = "-o"
    args2(2) = "/home/andrea/Documenti/ulb/thesis/output/spanner"
    args2(3) = "src/test/scala/be.ac.ulb.arc/benchmark/movieFollowsTitle.aqls"
  }

  test("arc should correctly run a spanner on the input documents") {

    arc.main(data.args)
  }

  test("arc should correctly run an AQL specification on the input documents") {

    arc.main(data.args1)
  }

  test("arc should correctly compile an AQL specification into a spanner") {

    arc.main(data.args2)
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
}
