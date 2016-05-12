package be.ac.ulb.amorcian.arc.compiler

import org.scalatest.FunSuite

class compilerTestSuite extends FunSuite {

  def files = new {

    val sampleVSet1 = "src/test/scala/be.ac.ulb.arc/compiler/sampleVSet6.txt"
    val sampleVSet2 = "src/test/scala/be.ac.ulb.arc/compiler/sampleVSet7.txt"

  }


  test("Just trying") {

    val (nrStates, initial, transitionFunction, vars, finalStates) = VSetAFileReader.getVSetA(files.sampleVSet1)

    val a = new VSetAutomaton(nrStates, initial, transitionFunction, vars, finalStates)

    val (nrStates2, initial2, transitionFunction2, vars2, finalStates2) = VSetAFileReader.getVSetA(files.sampleVSet2)

    val a2 = new VSetAutomaton(nrStates2, initial2, transitionFunction2, vars2, finalStates2)

    val a3 = a.union(a2)

    println("hey")

    assert (1 == 1)

  }

}
