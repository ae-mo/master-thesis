package be.ac.ulb.arc.vset

import org.scalatest.FunSuite

import scala.collection.immutable.{HashSet => SVars}
import scala.collection.immutable.{HashSet => SVOps}


class basicsTestSuite extends FunSuite {
  type State2 = (State, State)
  def data = new {

    val q1 = new State
    val q2 = new State
    val q3 = new State
    val q4 = new State
    val q5 = new State
    val q6 = new State

    val vs = new SVars + 1 + 2 + 3
    val vs2 = new SVars + 2 + 3
    val vs3 = new SVars + 4 + 5

    val vop1 = new SVOp(1, ⊢)
    val vop2 = new SVOp(2, ⊢)
    val vop3 = new SVOp(3, ⊢)
    val vop4 = new SVOp(1, ⊢)
    val vop5 = new SVOp(2, ⊣)
    val vop6 = new SVOp(2, ⊣)
    val vop7 = new SVOp(4, ⊣)
    val vop8 = new SVOp(5, ⊢)


    val vos = new SVOps[SVOp] + vop1 + vop2 + vop3
    val vos2 = new SVOps[SVOp] + vop2 + vop3
    val vos3 = new SVOps[SVOp] + vop7 + vop8
    val vos4 = new SVOps[SVOp]
    val vos5 = new SVOps[SVOp]

    val t1 = new OrdinaryTransition[State](q1, 'g', vs, q2)
    val t2 = new RangeTransition[State](q3, new Range('e', 'i'), vs, q4)
    val t3 = new OperationsTransition[State](q5, vos, vs, q6)
    val t4 = new OperationsTransition[State](q3, vos2, vs2, q4)
    val t5 = new RangeTransition[State](q3, new Range('g', 'o'), vs, q4)
    val t6 = new RangeTransition[State](q3, new Range('b', 'h'), vs, q4)
    val t7 = new RangeTransition[State](q3, new Range('s', 'u'), vs, q4)
    val t8 = new RangeTransition[State](q3, new Range('a', 'b'), vs, q4)
    val t9 = new RangeTransition[State](q3, new Range('a', 'g'), vs, q4)
    val t10 = new OperationsTransition[State](q3, vos2, vs, q4)
    val t11 = new OperationsTransition[State](q3, vos3, vs3, q4)
    val t12 = new OperationsTransition[State](q3, vos4, vs2, q4)
    val t13 = new OperationsTransition[State](q3, vos5, vs3, q4)
    val t14 = new OrdinaryTransition[State](q2, 'g', vs, q6)

  }

  test("States of type State2 should be equal if their correspondent components are the same") {

    // Copying the fixture here because it doesn't seem to stay fixed...
    val q1 = new State
    val q2 = new State
    val q3 = new State
    val q4 = new State

    val q12 = (q1, q2)
    val q12_ = (q1, q2)
    val q13 = (q1, q3)
    val q34 = (q3, q4)

    assert(q12 == q12_)
    assert(q12 != q13)
    assert(q13 != q34)

  }

  test("Span variable operations should be recognized equal when they have the same variable and they are of the same kind") {

    assert(data.vop1 == data.vop4)
    assert(data.vop5 == data.vop6)
    assert(data.vop1 != data.vop5)
    assert(data.vop2 != data.vop6)
  }
  
  test("Ordinary transitions should correctly intersect with other transitions") {

    val t12 = data.t1.&(data.t2).get

    assert(t12 != None)

    val t13 = data.t1.&(data.t3)

    assert(t13 == None)

    val t114 = data.t1.&(data.t14).get


  }

  test("Range transitions should correctly intersect with other transitions") {

    val t56 = data.t5.&(data.t6).get
    val t65 = data.t6.&(data.t5).get
    val t27 = data.t2.&(data.t7)
    val t72 = data.t7.&(data.t2)
    val t78 = data.t7.&(data.t8)
    val t59 = data.t5.&(data.t9).get
    val t95 = data.t9.&(data.t5).get


    val t23 = data.t2.&(data.t3)

  }

  test("Operations transitions should correctly intersect with other transitions") {

    val t34 = data.t3.&(data.t4).get
    val t310 = data.t3.&(data.t10)
    val t311 = data.t3.&(data.t11).get
    val t1213 = data.t12.&(data.t13).get

  }

  test("Transitions should be considered equal only when they are of the same type, have same source and destination state, same label and same variable set") {

    // Copying the fixture here because it doesn't seem to stay fixed...
    val q1 = new State
    val q2 = new State
    val q3 = new State
    val q4 = new State
    val q5 = new State
    val q6 = new State

    val vs = new SVars + 1 + 2 + 3
    val vs2 = new SVars + 2 + 3
    val vs3 = new SVars + 4 + 5
    val vs4 = new SVars + 4 + 5

    val vop1 = new SVOp(1, ⊢)
    val vop2 = new SVOp(2, ⊢)
    val vop3 = new SVOp(3, ⊢)
    val vop4 = new SVOp(1, ⊢)
    val vop5 = new SVOp(2, ⊣)
    val vop6 = new SVOp(2, ⊣)
    val vop7 = new SVOp(4, ⊣)
    val vop8 = new SVOp(5, ⊢)
    val vop9 = new SVOp(4, ⊣)
    val vop10 = new SVOp(5, ⊢)


    val vos = new SVOps[SVOp] + vop1 + vop2 + vop3
    val vos2 = new SVOps[SVOp] + vop2 + vop3
    val vos3 = new SVOps[SVOp] + vop7 + vop8
    val vos4 = new SVOps[SVOp]
    val vos5 = new SVOps[SVOp]
    val vos6 = new SVOps[SVOp] + vop9 + vop10

    val t1 = new OrdinaryTransition[State](q1, 'g', vs, q2)
    val t2 = new RangeTransition[State](q3, new Range('e', 'i'), vs, q4)
    val t3 = new OperationsTransition[State](q5, vos, vs, q6)
    val t4 = new OperationsTransition[State](q3, vos2, vs2, q4)
    val t5 = new RangeTransition[State](q3, new Range('g', 'o'), vs, q4)
    val t6 = new RangeTransition[State](q3, new Range('b', 'h'), vs, q4)
    val t7 = new RangeTransition[State](q3, new Range('s', 'u'), vs, q4)
    val t8 = new RangeTransition[State](q3, new Range('a', 'b'), vs, q4)
    val t9 = new RangeTransition[State](q3, new Range('a', 'g'), vs, q4)
    val t10 = new OperationsTransition[State](q3, vos2, vs, q4)
    val t11 = new OperationsTransition[State](q3, vos3, vs3, q4)
    val t12 = new OperationsTransition[State](q3, vos4, vs2, q4)
    val t13 = new OperationsTransition[State](q3, vos5, vs3, q4)
    val t14 = new OrdinaryTransition[State](q2, 'g', vs, q6)
    val t15 = new RangeTransition[State](q3, new Range('b', 'h'), vs, q4)
    val t16 = new OperationsTransition[State](q3, vos6, vs4, q4)
    val t17 = new OperationsTransition[State](q3, vos5, vs3, q6)
    val t18 = new OperationsTransition[State](q1, vos5, vs3, q6)

    assert(t1 == t1)
    assert(t1 != t2)
    assert(t1 != t14)
    assert(t2 == t2)
    assert(t6 == t15)
    assert(t7 != t9)
    assert(t10 != t11)
    assert(t11 == t16)
    assert(t13 != t17)
    assert(t17 != t18)

  }
   
}