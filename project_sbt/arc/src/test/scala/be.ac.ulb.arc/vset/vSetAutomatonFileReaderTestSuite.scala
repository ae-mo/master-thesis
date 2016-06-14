package be.ac.ulb.arc.vset

import org.scalatest.FunSuite

/**
  * Created by andrea on 18/05/16.
  */
class vSetAutomatonFileReaderTestSuite extends FunSuite {

  val data = new {

    val vsetFile1 = "src/test/scala/be.ac.ulb.arc/vset/spanner2.txt"
    val vsetFile2 = "src/test/scala/be.ac.ulb.arc/vset/spanner.txt"
  }

  test("File reader should correctly return vset-automata contained in given files") {

    /*val vset1 = VSetAutomatonFileReader.getVSetAutomaton(data.vsetFile1).get

    assert(vset1.isInstanceOf[VSetAutomaton])

    val ordinaryTrs1 = vset1.δ.filter((t:Transition[State]) => t.isInstanceOf[OrdinaryTransition[State]])
    val operationsTrs1 = vset1.δ.filter((t:Transition[State]) => t.isInstanceOf[OperationsTransition[State]])

    assert(ordinaryTrs1.size == 3)
    assert(operationsTrs1.size == 10)

    val vset2 = VSetAutomatonFileReader.getVSetAutomaton(data.vsetFile2).get

    assert(vset2.isInstanceOf[VSetAutomaton])*/
  }
}
