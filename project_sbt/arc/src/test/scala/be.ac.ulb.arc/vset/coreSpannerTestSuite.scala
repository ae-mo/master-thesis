package be.ac.ulb.arc.vset

import be.ac.ulb.arc.runtime._
import scala.collection.mutable.{ArrayBuffer, Map}
import org.scalatest.FunSuite

/**
  * Created by andrea on 14/06/16.
  */
class coreSpannerTestSuite extends FunSuite{

  val data = new {

    val spannerFile1 = "src/test/scala/be.ac.ulb.arc/vset/spanner.txt"
    val spannerFile2 = "src/test/scala/be.ac.ulb.arc/vset/spanner2.txt"
    val spannerFile3 = "src/test/scala/be.ac.ulb.arc/vset/spanner3.txt"
  }

  test("A core spanner should be correctly converted into an NFA program") {

    val spanner = CoreSpannerFileReader.getCoreSpanner(data.spannerFile1).get

    val spanner3 = CoreSpannerFileReader.getCoreSpanner(data.spannerFile3).get

    val s1 = "aa" + '\0'
    val t1Opt = spanner.evaluate(s1)
    assert(t1Opt != None)
    val t1 = t1Opt.get
    assert(t1.size == 2)
    assert(t1.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 1, 1, 1).deep).size == 1)
    assert(t1.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 2, 2, 2).deep).size == 1)

    val t3Opt = spanner3.evaluate(s1)
    assert(t3Opt != None)
    val t3 = t3Opt.get
    assert(t3.size == 1)
    assert(t3.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 1, 1, 1).deep).size == 1)

    val s2 = "b" + '\0'

    val t12opt = spanner.evaluate(s2)
    val t32opt = spanner3.evaluate(s2)

    assert(t12opt == None)
    assert(t32opt != None)

    val t32 = t32opt.get
    assert(t32.size == 1)
    assert(t32.filter((p:StringPointerCollection) => p.toArray.deep == Array[Int](0, 1, 1, 1).deep).size == 1)

  }


}
