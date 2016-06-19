package be.ac.ulb.arc.vset

import be.ac.ulb.arc.runtime._
import scala.collection.immutable.{HashSet => SVars}
import org.scalatest.FunSuite

/**
  * Created by andrea on 14/06/16.
  */
class coreSpannerTestSuite extends FunSuite{

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


    val spanner = CoreSpannerFileReader.getCoreSpanner(spannerFile1).get
    val spanner3 = CoreSpannerFileReader.getCoreSpanner(spannerFile3).get
    val spanner4 = CoreSpannerFileReader.getCoreSpanner(spannerFile4).get
    val spanner5 = CoreSpannerFileReader.getCoreSpanner(spannerFile5).get
    val spanner6 = CoreSpannerFileReader.getCoreSpanner(spannerFile6).get
    val spanner7 = CoreSpannerFileReader.getCoreSpanner(spannerFile7).get
    val spanner8 = CoreSpannerFileReader.getCoreSpanner(spannerFile8).get
    val spanner9 = CoreSpannerFileReader.getCoreSpanner(spannerFile9).get
    val spanner10 = CoreSpannerFileReader.getCoreSpanner(spannerFile10).get
  }

  test("A core spanner should be correctly converted into an NFA program") {

    val s1 = "aa" + '\0'
    val t1Opt = data.spanner.evaluate(s1)
    assert(t1Opt != None)
    val t1 = t1Opt.get
    assert(t1.size == 2)
    assert(t1.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 1, 1, 1).deep).size == 1)
    assert(t1.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 2, 2, 2).deep).size == 1)

    val t3Opt = data.spanner3.evaluate(s1)
    assert(t3Opt != None)
    val t3 = t3Opt.get
    assert(t3.size == 1)
    assert(t3.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 1, 1, 1).deep).size == 1)

    val s2 = "b" + '\0'

    val t12opt = data.spanner.evaluate(s2)
    val t32opt = data.spanner3.evaluate(s2)

    assert(t12opt == None)
    assert(t32opt != None)

    val t32 = t32opt.get
    assert(t32.size == 1)
    assert(t32.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 1, 1, 1).deep).size == 1)

    val s3 = "cabccabcc" + '\0'

    val t34Opt = data.spanner4.evaluate(s3)

    assert(t34Opt != None)

    val t34 = t34Opt.get

    assert(t34.size == 3)

    assert(t34.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 1, 1, 1).deep).size == 1)
    assert(t34.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 5, 5, 5).deep).size == 1)
    assert(t34.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 9, 9, 9).deep).size == 1)

  }

  test("the union operation between two spanners should result in the union of their (V, S)-relations") {

    val spanner5Opt = data.spanner ∪ data.spanner4

    assert(spanner5Opt != None)

    val spanner5 = spanner5Opt.get

    val s1 = "aa" + '\0'

    val t15Opt = spanner5.evaluate(s1)

    assert(t15Opt != None)

    val t15 = t15Opt.get
    assert(t15.size == 2)
    assert(t15.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 1, 1, 1).deep).size == 1)
    assert(t15.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 2, 2, 2).deep).size == 1)

    val s2 = "cabccabcc" + '\0'

    val t25Opt = spanner5.evaluate(s2)

    assert(t25Opt != None)

    val t25 = t25Opt.get

    assert(t25.size == 3)
    assert(t25.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 1, 1, 1).deep).size == 1)
    assert(t25.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 5, 5, 5).deep).size == 1)
    assert(t25.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 9, 9, 9).deep).size == 1)
  }

  test("the natural join operation between two spanners should result in the natural join of their (V, S)-relations") {

    val spanner7Opt = data.spanner5 ⋈ data.spanner6

    assert(spanner7Opt != None)

    val spanner7 = spanner7Opt.get

    val s1 = "bbbbddd" + '\0'

    val t17Opt = spanner7.evaluate(s1)

    assert(t17Opt != None)

    val t17 = t17Opt.get

    assert(t17.size == 5)
    assert(t17.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 1, 1, 1, 1, 1).deep).size == 1)
    assert(t17.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 2, 2, 2, 2, 2).deep).size == 1)
    assert(t17.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 3, 3, 3, 3, 3).deep).size == 1)
    assert(t17.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 4, 4, 4, 4, 4).deep).size == 1)
    assert(t17.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 0, 0, 0, 0, 0).deep).size == 1)

  }

  test("the natural join operation between two spanners should result in the natural join of their (V, S)-relations (2)") {

    val s1 = "test aaa ababab aaaaa" + '\0'

    val t17Opt = data.spanner7.evaluate(s1)

    assert(t17Opt != None)

    val t17 = t17Opt.get

    OutputWriter.printOutput(s1, t17)

    val spanner9Opt = data.spanner7 ⋈ data.spanner8

    assert(spanner9Opt != None)

    val spanner9 = spanner9Opt.get

    val t19Opt = spanner9.evaluate(s1)

    assert(t19Opt != None)

    val t19 = t19Opt.get

    OutputWriter.printOutput(s1, t19)

  }

  test("Spanning overlapping tuples") {

    val s1 = "this test will show the truth" + '\0'

    val t19Opt = data.spanner9.evaluate(s1)

    val t19 = t19Opt.get

    OutputWriter.printOutput(s1, t19)
  }

  test("the projection operation on a spanner should in the projection of its (V, S)-relation on the desired span variables") {

    val s1 = "this is a test ab test ab ab ababab" + '\0'

    val t110Opt = data.spanner10.evaluate(s1)

    assert(t110Opt != None)

    val t110 = t110Opt.get

    OutputWriter.printOutput(s1, t110)

    val spanner11Opt = data.spanner10.π(new SVars + 1)

    assert(spanner11Opt != None)

    val spanner11 = spanner11Opt.get

    val t111Opt = spanner11.evaluate(s1)

    assert(t111Opt != None)

    val t111 = t111Opt.get

    OutputWriter.printOutput(s1, t111)

    assert(t110.size == t111.size)

    for(t <- t110) {

      assert(t111.exists((t1:StringPointerCollection) => t1(2) == t(2) && t1(3) == t(3)))
    }
  }
}
