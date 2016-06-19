package be.ac.ulb.arc.runtime

import be.ac.ulb.arc.vset.CoreSpannerFileReader
import scala.collection.immutable.{HashSet => SVars}
import org.scalatest.FunSuite
/**
  * Created by andrea on 19/06/16.
  */
class classicalImplementationTestSuite extends FunSuite{



  val data = new {

    val spannerFile1 = "src/test/scala/be.ac.ulb.arc/vset/spanner.txt"
    val spannerFile2 = "src/test/scala/be.ac.ulb.arc/vset/spanner2.txt"
    val spannerFile3 = "src/test/scala/be.ac.ulb.arc/vset/spanner3.txt"
    val spannerFile4 = "src/test/scala/be.ac.ulb.arc/vset/spanner4.txt"
    val spannerFile5 = "src/test/scala/be.ac.ulb.arc/vset/spanner5.txt"
    val spannerFile6 = "src/test/scala/be.ac.ulb.arc/vset/spanner6.txt"
    val spannerFile7 = "src/test/scala/be.ac.ulb.arc/vset/spanner7.txt"
    val spannerFile8 = "src/test/scala/be.ac.ulb.arc/vset/spanner8.txt"
    val spannerFile9 = "src/test/scala/be.ac.ulb.arc/vset/spanner9.txt"
    val spannerFile10 = "src/test/scala/be.ac.ulb.arc/vset/spanner10.txt"
    val spannerFile11 = "src/test/scala/be.ac.ulb.arc/vset/spanner11.txt"
    val spannerFile12 = "src/test/scala/be.ac.ulb.arc/vset/spanner12.txt"
    val spannerFile13 = "src/test/scala/be.ac.ulb.arc/vset/spanner13.txt"

    val spanner = CoreSpannerFileReader.getCoreSpanner(spannerFile1).get
    val spanner3 = CoreSpannerFileReader.getCoreSpanner(spannerFile3).get
    val spanner4 = CoreSpannerFileReader.getCoreSpanner(spannerFile4).get
    val spanner5 = CoreSpannerFileReader.getCoreSpanner(spannerFile5).get
    val spanner6 = CoreSpannerFileReader.getCoreSpanner(spannerFile6).get
    val spanner7 = CoreSpannerFileReader.getCoreSpanner(spannerFile7).get
    val spanner8 = CoreSpannerFileReader.getCoreSpanner(spannerFile8).get
    val spanner9 = CoreSpannerFileReader.getCoreSpanner(spannerFile9).get
    val spanner10 = CoreSpannerFileReader.getCoreSpanner(spannerFile10).get
    val spanner11 = CoreSpannerFileReader.getCoreSpanner(spannerFile11).get
    val spanner12 = CoreSpannerFileReader.getCoreSpanner(spannerFile12).get
    val spanner13 = CoreSpannerFileReader.getCoreSpanner(spannerFile13).get
  }

  test("the projection operation should result in the projection of the input (V, S)-relation on the desired span variables") {

    val s1 = "this is a test ab test ab ab ababab" + '\0'

    val t110Opt = data.spanner10.evaluate(s1)

    val t110 = t110Opt.get

    val t110pOpt = ClassicalImplementation.π(t110, data.spanner10.automaton.V, new SVars + 1)

    assert(t110pOpt != None)

    val t110p = t110pOpt.get

    val spanner11Opt = data.spanner10.π(new SVars + 1)
    val spanner11 = spanner11Opt.get
    val t111Opt = spanner11.evaluate(s1)
    val t111 = t111Opt.get

    assert(t110p == t111)
  }

  test("the natural join operation should result in the natural join of the input (V, S)-relations") {

    val s = "3 this 0 is a 3 test, a 4 useful 2 test 4" + '\0'

    val t11Opt = data.spanner11.evaluate(s)
    assert(t11Opt != None)
    val t11 = t11Opt.get
    OutputWriter.printOutput(s, t11)

    val t12Opt = data.spanner12.evaluate(s)
    assert(t12Opt != None)
    val t12 = t12Opt.get
    OutputWriter.printOutput(s, t12)

    val t11j12Opt = ClassicalImplementation.⋈(t11, data.spanner11.automaton.V,
      t12, data.spanner12.automaton.V)
    assert(t11j12Opt != None)
    val t11j12 = t11j12Opt.get
    OutputWriter.printOutput(s, t11j12)

    val spanner13Opt = data.spanner11 ⋈ data.spanner12
    val spanner13 = spanner13Opt.get

    val t13Opt = spanner13.evaluate(s)
    assert(t13Opt != None)

    val t13 = t13Opt.get

    assert(t11j12 == t13)

  }

  test("the union operation should result in the union of the input (V, S)-relations") {

    val s = "aa" + '\0'

    val t = data.spanner.evaluate(s).get
    val t4 = data.spanner4.evaluate(s).get

    val tut4Opt = ClassicalImplementation.∪(t, data.spanner.automaton.V, t4, data.spanner4.automaton.V)
    assert(tut4Opt != None)

    assert(tut4Opt.get == (t ++ t4))
  }

  test("the string equality selection operation should only retain the tuples whose specified variables are equal") {

    val s = "this a b b is a test a b a b" + '\0'

    val t = data.spanner13.evaluate(s).get

    val tEqOpt = ClassicalImplementation.ς(s, t, new SVars + 0 + 1, 0, 1)
    assert(tEqOpt != None)

    val tEq = tEqOpt.get

    OutputWriter.printOutput(s, tEq)

    for(t <- tEq) assert(s.substring(t(0), t(1)) == s.substring(t(2), t(3)))

    val tNEq = t.diff(tEq)

    for(t <- tNEq) assert(s.substring(t(0), t(1)) != s.substring(t(2), t(3)))
  }
}
