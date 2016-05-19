package scala.lms.tutorial

import scala.lms.common._


class lmsTestSuite extends TutorialFunSuite {
  val under = "dslapi"

  test("1") {
    val snippet = new DslDriver[Int,Int] {
      def snippet(x: Rep[Int]) = {

        def compute(b: Boolean): Rep[Int] = {
          // the if is executed in the first stage
          if (b) 1 else x
        }
        compute(true)+compute(1==1)

      }
    }
    assert(snippet.eval(0) === 2)
    check("1", snippet.code)
  }

  test("2") {
    val snippet = new  DslDriver[Int,Int] {
      def snippet(x: Rep[Int]) = {

        def compute(b: Rep[Boolean]): Rep[Int] = {
          // the if is deferred to the second stage
          if (b) 1 else x
        }
        compute(x==1)

      }
    }
    assert(snippet.eval(2) === 2)
    check("2", snippet.code)
  }

}
