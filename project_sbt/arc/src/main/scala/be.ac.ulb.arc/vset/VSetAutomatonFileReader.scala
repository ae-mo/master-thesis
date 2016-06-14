package be.ac.ulb.arc.vset

import scala.collection.immutable.{HashSet => SVars}
import scala.collection.immutable.{HashSet => SVOps}
import scala.collection.immutable.{HashSet => TransitionFunction}
import scala.collection.immutable.{HashSet => StateSet}
import scala.collection.mutable.ArrayBuffer
import scala.{Int => SVar}

/**
  * A reader of files containing vset-automata specifications.
  */
object CoreSpannerFileReader {

  /**
    * Returns the vset-automaton contained in the given file.
    *
    * @param file the file name
    */
  def getCoreSpanner(file: String): (Option[CoreSpanner]) = {

    import scala.io.Source

    val source = Source.fromFile(file, "UTF-8")
    val lineIterator = source.getLines

    var QMap = Map[Int, State]()

    var Q = new StateSet[State]
    var V = new SVars[SVar]

    val QSizeStr = lineIterator.next()
    val QSize = QSizeStr.toInt

    // Initial state
    val q0Str = lineIterator.next()
    val q0Int = q0Str.toInt
    val q0 = new State
    Q = Q + q0
    QMap = QMap + ((q0Int, q0))

    // Final state
    val qfStr = lineIterator.next()
    val qfInt = qfStr.toInt
    val qf = new State
    Q = Q + qf
    QMap = QMap + ((qfInt, qf))

    // Span Variables
    val VStr = lineIterator.next()
    val VTok = VStr.split("\\s+").map(_.toInt)
    V = V ++ VTok

    // String equalities
    val eqPattern = "\\s*\\(\\s*(.)\\s*,\\s*(.)\\s*\\)\\s*".r
    var eqsBuf = new ArrayBuffer[(SVar, SVar)]()
    var eqStr = lineIterator.next()

    while(!eqStr.matches("-")) {

      val eqPattern(x, y) = eqStr
      eqsBuf += ((x.toInt, y.toInt))
      eqStr = lineIterator.next()
    }
    var eqsSet:Option[Set[(SVar, SVar)]] = None

    if(eqsBuf.size > 0)
      eqsSet = Some(eqsBuf.toSet)

    // Transition function and other states
    val transitionPattern = "(\\d+)\\s(.+)\\s(\\d+)".r
    var δ = new TransitionFunction[Transition[State]]

    for (l <- lineIterator) {

      val transitionPattern(qStr, label, q1Str) = l

      val qInt = qStr.toInt
      val q1Int = q1Str.toInt

      var q: State = null
      var q1: State = null

      // Process q
      if (!QMap.contains(qInt)) {
        q = new State
        Q = Q + q
        QMap = QMap + ((qInt, q))
      }
      else q = QMap(qInt)

      // Process q1
      if (!QMap.contains(q1Int)) {
        q1 = new State
        Q = Q + q1
        QMap = QMap + ((q1Int, q1))
      }
      else q1 = QMap(q1Int)

      // Process label and create correspondent transition
      if(label.length == 1) {
        δ = δ + new OrdinaryTransition[State](q, label.charAt(0), V, q1)
      }
      else if(label.matches("\\{.*\\}")) {

        val vOpsTok = if(label.matches("\\{.+\\}")) label.replaceAll("[\\{\\}]", "").split(',') else Array[String]()
        var S = new SVOps[SVOp]

        for(vOp <- vOpsTok) {

          val opSymbol = vOp.charAt(vOp.length - 1)
          val sVar = vOp.dropRight(1).toInt
          val kind = if(opSymbol == '⊢') ⊢ else ⊣
          S = S + new SVOp(sVar, kind)
        }

        δ = δ + new OperationsTransition[State](q, S, V, q1)
      }
    }

    (Some(new CoreSpanner(new VSetAutomaton(Q, q0, qf, V, δ), eqsSet)))
  }
}
