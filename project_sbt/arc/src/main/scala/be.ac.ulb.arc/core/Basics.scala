package be.ac.ulb.arc.core

import scala.collection.immutable.{HashSet => SVars}
import scala.collection.immutable.{HashSet => SVOps}
import scala.{Int => SVar}

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

        if(this.σ >= σ.min || this.σ <= σ.max)
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

  override def &(other:Transition[State]):Option[Transition[State2]]  = {

    other match {

      case OrdinaryTransition(q:State, σ:Char, v:SVars[SVar], q1:State) => {

        for(o <- this.S) {

          if(v.contains(o.x))
            return None
        }

        Some(OperationsTransition[State2]((this.q, q), this.S, this.V.union(v), (this.q1, q)))

      }

      case RangeTransition(q:State, σ:Range, v:SVars[SVar], q1:State) => {

        for(o <- this.S) {

          if(v.contains(o.x))
            return None
        }

        Some(OperationsTransition[State2]((this.q, q), this.S, this.V.union(v), (this.q1, q)))

      }

      case OperationsTransition(q:State, s:SVOps[SVOp], v:SVars[SVar], q1:State) => {

        // common variables
        val V1 = V.intersect(v)

        // variable operations in both sets
        val S1 = S.intersect(s)

        // variable operations only in S
        val S_S1 = S.diff(S1)

        // Check if the variable operations only in S involve common variables.
        // In that case, abort.
        for(v <- S_S1) {

          if(V1.contains(v.x))
            return None

        }

        // the operations don't involve common variables, so add them.
        val S2 = S1 ++ S_S1

        // variable operations only in s
        val s_S1 = s.diff(S1)

        // Check if the variable operations only in s involve common variables.
        // In that case, abort.
        for(v <- s_S1) {

          if(V1.contains(v.x))
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

      case that: OperationsTransition[State] =>  {

        that.canEqual(this) && that.q == this.q && that.q1 == this.q1 && that.S == this.S && that.V == this.V && this.hashCode == that.hashCode
      }
      case _ => false
    }
  }

  override def hashCode:Int = {

    val prime = 31
    var result = 1

    result = prime*result + (if (this.q == null) 0 else this.q.hashCode)
    result = prime*result + (if (this.S == null) 0 else this.S.hashCode)
    result = prime*result + (if (this.V == null) 0 else this.V.hashCode)
    result = prime*result + (if (this.q1 == null) 0 else this.q1.hashCode)
    return result
  }

}
