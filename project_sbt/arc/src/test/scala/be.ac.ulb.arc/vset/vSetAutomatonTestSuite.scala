package be.ac.ulb.arc.vset

import org.scalatest.FunSuite

/**
  * Created by andrea on 18/05/16.
  */
class vSetAutomatonTestSuite extends FunSuite{

  val data = new {

    val vsetFile1 = "src/test/scala/be.ac.ulb.arc/vset/sampleVSet6.txt"
    val vsetFile2 = "src/test/scala/be.ac.ulb.arc/vset/sampleVSet7.txt"
  }

  test("Epsilon closure should add the correct transitions to vset-automata") {

    val vset1 = VSetAutomatonFileReader.getVSetAutomaton(data.vsetFile2).get

    val vset2 = vset1.ε()

    val newTr = vset2.δ.diff(vset1.δ)

    assert(newTr.size == 6)

  }

}
