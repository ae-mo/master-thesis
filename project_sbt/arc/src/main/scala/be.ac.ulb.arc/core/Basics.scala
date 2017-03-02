package be.ac.ulb.arc.core

import scala.collection.immutable.{HashSet => SVars}
import scala.collection.immutable.{HashSet => SVOps}
import scala.{Int => SVar}
import be.ac.ulb.arc.runtime.StringPointerCollection
import be.ac.ulb.arc.runtime.{StringPointerCollection => VSTuple}
import scala.collection.mutable.{HashSet => VSRelation}

import scala.collection.mutable.{HashSet => RunSet}
import scala.collection.mutable.ArrayBuffer

trait ⊢/⊣

class State

case object ⊢ extends ⊢/⊣
case object ⊣ extends ⊢/⊣

trait types {

  type SVar = Int
}

class SVOp(val x:SVar, val t: ⊢/⊣) {

  def canEqual(a: Any) = a.isInstanceOf[SVOp]

  override def equals(that: Any): Boolean = {

    that match {

      case that: SVOp =>  {

        that.canEqual(this) && that.x == this.x && that.t == this.t && this.hashCode == that.hashCode
      }
      case _ => false
    }
  }

  override def hashCode:Int = {

    val prime = 31
    var result = 1

    result = prime*result + this.x
    result = prime*result + (if (this.t == null) 0 else this.t.hashCode)
    return result
  }

}

class Range(val min:Char, val max:Char) {

  def canEqual(a: Any) = a.isInstanceOf[Range]

  override def equals(that: Any): Boolean = {

    that match {

      case that: Range =>  {

        that.canEqual(this) && that.min == this.min && that.max == this.max && this.hashCode == that.hashCode
      }
      case _ => false
    }
  }

  override def hashCode:Int = {

    val prime = 31
    var result = 1

    result = prime*result + min
    result = prime*result + max
    return result
  }
}

/**
  * Represents a generic transition.
  */
abstract class Transition[State](val q:State, val V:SVars[SVar], val q1:State) extends types {

  type State2 = (State, State)

  def &(other:Transition[State]):Option[Transition[State2]]
}

/**
  * Represents an ordinary transition, with a character label.
  *
  * @param q
  * @param σ
  * @param q1
  * @tparam State
  */
case class OrdinaryTransition[State](override val q: State, σ: Char, override val V:SVars[SVar], override val q1: State) extends Transition[State](q, V, q1) {

  override def &(other:Transition[State]):Option[Transition[State2]] = {

    other match {

      case OrdinaryTransition(q:State, σ:Char, v:SVars[SVar], q1:State) => {

        if(this.σ == σ) {
          Some(OrdinaryTransition[State2]((this.q, q), σ, v.union(this.V), (this.q1, q1)))
        }
        else None

      }

      case RangeTransition(q:State, σ:Range, v:SVars[SVar], q1:State) => {

        if(this.σ >= σ.min && this.σ <= σ.max)
          Some(OrdinaryTransition[State2]((this.q, q), this.σ, this.V.union(v), (this.q1, q1)))
        else None

      }

      case OperationsTransition(q, s, v, q1) => {

        for(o <- s) {

          if(this.V.contains(o.x))
            return None
        }

        Some(OperationsTransition[State2]((this.q, q), s, this.V.union(v), (this.q, q1)))

      }
    }

  }

  def canEqual(a: Any) = a.isInstanceOf[OrdinaryTransition[State]]

  override def equals(that: Any): Boolean = {

    that match {

      case that: OrdinaryTransition[State] =>  {

        that.canEqual(this) && that.q == this.q && that.q1 == this.q1 && that.σ == this.σ && this.V == that.V && this.hashCode == that.hashCode
      }
      case _ => false
    }
  }

  override def hashCode:Int = {

    val prime = 31
    var result = 1

    result = prime*result + (if (this.q == null) 0 else this.q.hashCode)
    result = prime*result + (if (this.σ == null) 0 else this.σ.hashCode)
    result = prime*result + (if (this.q1 == null) 0 else this.q1.hashCode)
    result = prime*result + (if (this.V == null) 0 else this.V.hashCode)
    return result
  }
}

/**
  *
  * @param q
  * @param σ
  * @param q1
  * @tparam State
  */
case class RangeTransition[State](override val q: State, σ: Range, override val V:SVars[SVar], override val q1: State) extends Transition[State](q, V, q1) {

  override def &(other:Transition[State]):Option[Transition[State2]] = {

    other match {

      case OrdinaryTransition(q:State, σ:Char, v:SVars[SVar], q1:State) => {

        if(this.σ.min <= σ && this.σ.max >= σ) {
           Some(OrdinaryTransition[State2]((this.q, q), σ, v.union(this.V), (this.q1, q1)))
        }
        else None

      }

      case RangeTransition(q:State, σ:Range, v:SVars[SVar], q1:State) => {

        if(this.σ.min <= σ.max && σ.min <= this.σ.max) {

          val r:Range = new Range(this.σ.min.max(σ.min), this.σ.max.min(σ.max))

          if(r.min != r.max) Some(RangeTransition[State2]((this.q, q), r, this.V.union(v), (this.q1, q1)))
          else Some(OrdinaryTransition[State2]((this.q, q), r.min, this.V.union(v), (this.q1, q1)))
        }
        else None

      }

      case OperationsTransition(q, s, v, q1) => {

        for(o <- s) {

          if(this.V.contains(o.x))
            return None
        }

        Some(OperationsTransition[State2]((this.q, q), s, this.V.union(v), (this.q, q1)))
      }
    }
  }

  def canEqual(a: Any) = a.isInstanceOf[RangeTransition[State]]

  override def equals(that: Any): Boolean = {

    that match {

      case that: RangeTransition[State] =>  {

        that.canEqual(this) && that.q == this.q && that.q1 == this.q1 && that.σ == this.σ && this.V == that.V && this.hashCode == that.hashCode
      }
      case _ => false
    }
  }

  override def hashCode:Int = {

    val prime = 31
    var result = 1

    result = prime*result + (if (this.q == null) 0 else this.q.hashCode)
    result = prime*result + (if (this.σ == null) 0 else this.σ.hashCode)
    result = prime*result + (if (this.q1 == null) 0 else this.q1.hashCode)
    result = prime*result + (if (this.V == null) 0 else this.V.hashCode)
    return result
  }
}

/**
  *
  * @param q
  * @param S
  * @param V
  * @param q1
  * @tparam State
  */
case class OperationsTransition[State](override val q: State, S: SVOps[SVOp], override val V:SVars[SVar], override val q1: State) extends Transition[State](q, V, q1) {

  override def &(other: Transition[State]): Option[Transition[State2]] = {

    other match {

      case OrdinaryTransition(q: State, σ: Char, v: SVars[SVar], q1: State) => {

        for (o <- this.S) {

          if (v.contains(o.x))
            return None
        }

        Some(OperationsTransition[State2]((this.q, q), this.S, this.V.union(v), (this.q1, q)))

      }

      case RangeTransition(q: State, σ: Range, v: SVars[SVar], q1: State) => {

        for (o <- this.S) {

          if (v.contains(o.x))
            return None
        }

        Some(OperationsTransition[State2]((this.q, q), this.S, this.V.union(v), (this.q1, q)))

      }

      case OperationsTransition(q: State, s: SVOps[SVOp], v: SVars[SVar], q1: State) => {

        // common variables
        val V1 = V.intersect(v)

        // variable operations in both sets
        val S1 = S.intersect(s)

        // variable operations only in S
        val S_S1 = S.diff(S1)

        // Check if the variable operations only in S involve common variables.
        // In that case, abort.
        for (v <- S_S1) {

          if (V1.contains(v.x))
            return None

        }

        // the operations don't involve common variables, so add them.
        val S2 = S1 ++ S_S1

        // variable operations only in s
        val s_S1 = s.diff(S1)

        // Check if the variable operations only in s involve common variables.
        // In that case, abort.
        for (v <- s_S1) {

          if (V1.contains(v.x))
            return None

        }

        // the operations don't involve common variables, so add them.
        val S3 = S2 ++ s_S1

        Some(OperationsTransition[State2]((this.q, q), S3, V.union(v), (this.q1, q1)))

      }
    }
  }

  def canEqual(a: Any) = a.isInstanceOf[OperationsTransition[State]]

  override def equals(that: Any): Boolean = {

    that match {

      case that: OperationsTransition[State] => {

        that.canEqual(this) && that.q == this.q && that.q1 == this.q1 && that.S == this.S && that.V == this.V && this.hashCode == that.hashCode
      }
      case _ => false
    }
  }

  override def hashCode: Int = {

    val prime = 31
    var result = 1

    result = prime * result + (if (this.q == null) 0 else this.q.hashCode)
    result = prime * result + (if (this.S == null) 0 else this.S.hashCode)
    result = prime * result + (if (this.V == null) 0 else this.V.hashCode)
    result = prime * result + (if (this.q1 == null) 0 else this.q1.hashCode)
    return result
  }

}

/**
  * Represents a configuration of a vset-automaton.
  *
  * @param q
  * @param i
  * @param saved
  */
class Configuration(var q: State, var i: Int, var saved: StringPointerCollection) {

  def canEqual(a: Any) = a.isInstanceOf[Configuration]

  override def equals(that: Any): Boolean = {

    that match {

      case that: Configuration=> {

        that.canEqual(this) && that.q == this.q && that.q == this.q && that.i == this.i && that.saved == this.saved
      }
      case _ => false
    }
  }

  override def hashCode: Int = {

    val prime = 31
    var result = 1

    result = prime * result + (if (this.q == null) 0 else this.q.hashCode)
    result = prime * result + i
    result = prime * result + (if (this.saved == null) 0 else this.saved.hashCode)
    return result
  }
}

/**
  * Represents a run of a vset-automaton.
  *
  * @param q0
  * @param qf
  * @param input
  * @param i
  * @param saved
  * @param δ
  */
class Run(val q0: State, val qf: State, val input: String, val i: Int, val saved: StringPointerCollection, val δ: Map[State, Array[Transition[State]]]) {

  val configuration = new Configuration(q0, i, saved)

  /**
    * Advances this run.
    *
    * @param runs
    * @param tuples
    */
  def advance(runs: RunSet[Run], tuples:VSRelation[VSTuple]):Unit = {

    // If we are in the final state, return the spanned tuple
    // Eliminate this run from the list of active runs
    if (configuration.q == qf) {
      runs -= this
      tuples += configuration.saved
      return
    }

    val (tr, rδ) = δ(configuration.q).splitAt(1)
    val i = configuration.i
    val saved = configuration.saved


    var evOpt = applyTransition(tr(0), configuration.i, configuration.saved)
    var ev:(State, Int, StringPointerCollection) = null

    if(evOpt == None) {

      runs -= this
    }
    else {
      ev = evOpt.get
      configuration.q = ev._1
      configuration.i = ev._2
      configuration.saved = ev._3
    }


    for (t <- rδ) {

      evOpt = applyTransition(t, i, saved)
      if(evOpt != None) {

        ev = evOpt.get
        runs += new Run(ev._1, qf, input, ev._2, ev._3, δ)
      }

    }
  }

  /**
    * Applies the given transition with the given string pointer and saved pointers collection.
    *
    * @param t
    * @param i
    * @param saved
    * @return
    */
  def applyTransition(t:Transition[State], i:Int, saved:StringPointerCollection): Option[(State, Int, StringPointerCollection)] = {

    t match {

      case OrdinaryTransition(q:State, σ:Char, v:SVars[SVar], q1:State) => {

        if(i < input.size && input(i) == σ)
          return Some((q1, i+1, saved))

        None
      }
      case RangeTransition(q: State, σ: Range, v: SVars[SVar], q1: State) => {

        if(i < input.size && input(i) >= σ.min && input(i) <= σ.max)
          return Some((q1, i+1, saved))

        None
      }
      case OperationsTransition(q: State, s: SVOps[SVOp], v: SVars[SVar], q1: State) => {

        if(s.size > 0) {

          val savedC = saved.copy

          for(o <- s) {

            if(o.t == ⊢)
              savedC(o.x * 2) = i
            else
              savedC(o.x * 2 + 1) = i
          }

          return Some((q1, i, savedC))
        }

        return Some((q1, i, saved))
      }
    }
  }

  /*ef canEqual(a: Any) = a.isInstanceOf[Run]

  override def equals(that: Any): Boolean = {

    that match {

      case that: Run => {

        that.canEqual(this) && that.configuration == this.configuration
      }
      case _ => false
    }
  }

  override def hashCode: Int = {

    val prime = 31
    var result = 1

    result = prime * result + (if (this.configuration == null) 0 else this.configuration.hashCode)
    return result
  }*/
}

