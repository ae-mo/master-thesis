package be.ac.ulb.arc.core

import org.scalatest.FunSuite
import scala.collection.immutable.{HashSet => SVars}

/**
  * Created by andrea on 14/06/16.
  */
class coreSpannerReaderTestSuite extends FunSuite{

  val data = new {

    val spannerFile1 = "src/test/scala/be.ac.ulb.arc/vset/spanner.txt"
    val spannerFile2 = "src/test/scala/be.ac.ulb.arc/vset/spanner2.txt"
  }

  test("The core spanner reader should correctly read core spanners from text files") {

    val spannerOpt = CoreSpannerFileReader.getCoreSpanner(data.spannerFile1)

    assert(spannerOpt != None)

    val spanner = spannerOpt.get

    assert(spanner.automaton.Q.size == 5)
    assert(spanner.automaton.δ.size == 5)
    assert(spanner.equalities != None)

    val eqs = spanner.equalities.get

    assert(eqs.contains((1, 2)))

    val spanner2Opt = CoreSpannerFileReader.getCoreSpanner(data.spannerFile2)

    assert(spanner2Opt != None)

    val spanner2 = spanner2Opt.get

    assert(spanner2.automaton.V == (new SVars + 1 + 2))
    assert(spanner2.automaton.Q.size == 11)
    assert(spanner2.automaton.δ.size == 13)
    assert(spanner2.equalities == None)

  }

}
