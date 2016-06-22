package be.ac.ulb.arc.core

import scala.collection.mutable.{Map => CoreSpannersCollection}
import scala.collection.mutable.{Map => VSRelationsCollection}
import scala.collection.immutable.{HashSet => VSRelation}
import be.ac.ulb.arc.runtime.{StringPointerCollection => VSTuple}
import be.ac.ulb.arc.runtime.ClassicalImplementation
import scala.collection.immutable.{HashSet => SVars}
import scala.{Int => SVar}
import scala.collection.mutable.ArrayBuffer

/**
  * Represents a generic operation on a (V, s)-relation(s).
  */
abstract class Operation(val res:String) {

  /**
    * Performs an operation on input spanners(s), resulting in a new spanner.
    *
    * @param spanners
    * @return
    */
  def perform(spanners:CoreSpannersCollection[String, CoreSpanner]):Option[CoreSpanner]

  /**
    * Execute an operation on input relation(s), derived from input spanner(s). It results
    * in a new relation.
    *
    * @param spanners
    * @param relations
    * @param doc
    * @param lazyEv
    * @return
    */
  def execute(spanners:CoreSpannersCollection[String, CoreSpanner], relations:VSRelationsCollection[String, VSRelation[VSTuple]], doc:String, lazyEv:Boolean = false): Option[VSRelation[VSTuple]]

  /**
    * Gets a relation from a collection of relations. Supports lazy evaluation.
    *
    * @param a
    * @param spanners
    * @param relations
    * @param doc
    * @param lazyEv
    * @return
    */
  def getRelation(a:String, spanners:CoreSpannersCollection[String, CoreSpanner], relations:VSRelationsCollection[String, VSRelation[VSTuple]], doc:String, lazyEv:Boolean):Option[VSRelation[VSTuple]] = {

    var r:VSRelation[VSTuple] = null

    // If the input relation isn't there and lazy evaluation is enabled,
    // evaluate it
    if(!relations.contains(a)) {

      if(!lazyEv)
        return None

      // if the spanner corresponding to a is not in the base spanners,
      // the corresponding relation is empty
      if(!spanners.contains(a)) return None

      val rOpt = spanners(a).evaluate(doc)

      if(rOpt == None) return None

      r = rOpt.get

    }
    else r = relations(a)

    Some(r)
  }
}

/**
  * Represents the projection operation.
  *
  * @param a
  * @param vars
  */
case class π(val a:String, val a2:String, val vars:SVars[SVar]) extends Operation(a2) {

  override def perform(spanners:CoreSpannersCollection[String, CoreSpanner]):Option[CoreSpanner] = {

    val _a = spanners(a)

    if( _a.equalities != None) {

      val eqs =  _a.equalities.get

      // Check if we are trying to eliminate variables on which we want to perform
      // string equality selection
      for(eq <- eqs) {

        if(!vars.contains(eq._1) || !vars.contains(eq._1))
          throw new Exception("Trying to eliminate variables needed for selection in " + a)
      }

    }

    val autOpt = _a.π(vars)
    if (autOpt == None)
      return None

    val _a2 = autOpt.get

    // Add the new spanner to the spanner collection
    spanners += ((a2, _a2))

    Some(_a2)
  }

  override def execute(spanners:CoreSpannersCollection[String, CoreSpanner], relations:VSRelationsCollection[String, VSRelation[VSTuple]], doc:String, lazyEv:Boolean = false): Option[VSRelation[VSTuple]] = {

    val rOpt = getRelation(a, spanners, relations, doc, lazyEv)

    if(rOpt == None) return None

    ClassicalImplementation.π(rOpt.get, spanners(a).automaton.V, vars)
  }

}

/**
  * Represents the natural join operation.
  *
  * @param a1
  * @param a2
  */
case class ⋈(val a1:String, val a2:String, val a3:String) extends Operation(a3) {

  override def perform(spanners:CoreSpannersCollection[String, CoreSpanner]):Option[CoreSpanner] = {

    val _a1 = spanners(a1)
    val _a2 = spanners(a2)

    // Join the underlying automata
    val aut3Opt = _a1 ⋈ _a2

    if(aut3Opt != None) {

      var a3Eqs:Option[Set[(SVar, SVar)]] = None

      // Get the union of the equalities
      if(_a1.equalities != None || _a2.equalities != None) {

        var eqSet = Set[(SVar, SVar)]()
        if(_a1.equalities != None)
          eqSet = eqSet ++ _a1.equalities.get
        if(_a2.equalities != None)
          eqSet = eqSet ++ _a2.equalities.get

        a3Eqs = Some(eqSet)

        val _a3 = aut3Opt.get

        // Add the result to the spanner collection
        spanners += ((a3, _a3))

        return Some(_a3)
      }

    }
    None
  }

  override def execute(spanners:CoreSpannersCollection[String, CoreSpanner], relations:VSRelationsCollection[String, VSRelation[VSTuple]], doc:String, lazyEv:Boolean = false): Option[VSRelation[VSTuple]] = {

    val r1Opt = getRelation(a1, spanners, relations, doc, lazyEv)
    // If any of the two input relations is empty, the result is empty
    if(r1Opt == None) return None
    val r2Opt = getRelation(a2, spanners, relations, doc, lazyEv)
    if(r2Opt == None) return None

    ClassicalImplementation.⋈(r1Opt.get, spanners(a1).automaton.V, r2Opt.get, spanners(a2).automaton.V)
  }
}

/**
  * Represents the union operation.
  *
  * @param a1
  * @param a2
  */
case class ∪(val a1:String, val a2:String, val a3:String) extends Operation(a3) {

  override def perform(spanners:CoreSpannersCollection[String, CoreSpanner]):Option[CoreSpanner] = {

    val _a1 = spanners(a1)
    val _a2 = spanners(a2)

    // Join the underlying automata
    val aut3Opt = _a1 ∪ _a2

    if(aut3Opt != None) {

      var a3Eqs:Option[Set[(SVar, SVar)]] = None

      // Get the union of the equalities
      if(_a1.equalities != None || _a2.equalities != None) {

        var eqSet = Set[(SVar, SVar)]()
        if(_a1.equalities != None)
          eqSet = eqSet ++ _a1.equalities.get
        if(_a2.equalities != None)
          eqSet = eqSet ++ _a2.equalities.get

        a3Eqs = Some(eqSet)

        val _a3 = aut3Opt.get

        // Add the result to the spanner collection
        spanners += ((a3, _a3))

        return Some(_a3)
      }

    }
    None
  }

  override def execute(spanners:CoreSpannersCollection[String, CoreSpanner], relations:VSRelationsCollection[String, VSRelation[VSTuple]], doc:String, lazyEv:Boolean = false): Option[VSRelation[VSTuple]] = {

    val r1Opt = getRelation(a1, spanners, relations, doc, lazyEv)
    var r1:VSRelation[VSTuple] = null
    if(r1Opt == None) r1 = VSRelation[VSTuple]() else r1Opt.get
    val r2Opt = getRelation(a2, spanners, relations, doc, lazyEv)
    var r2:VSRelation[VSTuple] = null
    if(r2Opt == None) r2 = VSRelation[VSTuple]() else r2Opt.get

    ClassicalImplementation.∪(r1, spanners(a1).automaton.V, r2, spanners(a2).automaton.V)
  }
}

/**
  * Represents the string equality selection operation.
  *
  * @param a
  * @param v1
  * @param v2
  */
case class ς(val a:String, val a2:String, val v1:SVar, val v2:SVar) extends Operation(a2) {

  override def perform(spanners:CoreSpannersCollection[String, CoreSpanner]):Option[CoreSpanner] = {

    val _a = spanners(a)

    var a2Eqs:Option[Set[(SVar, SVar)]] = None

    // Add existing equalities
    var eqSet = Set[(SVar, SVar)]()
    if(_a.equalities != None)
      eqSet = eqSet ++ _a.equalities.get

    // Add new equalities
    eqSet = eqSet + ((v1, v2))
    a2Eqs = Some(eqSet)

    val _a2 = new CoreSpanner(_a.automaton, a2Eqs)

    // Add the result to the spanner collection
    spanners +=((a2, _a2))

    Some(_a2)
  }

  override def execute(spanners:CoreSpannersCollection[String, CoreSpanner], relations:VSRelationsCollection[String, VSRelation[VSTuple]], doc:String, lazyEv:Boolean = false): Option[VSRelation[VSTuple]] = {

    val rOpt = getRelation(a, spanners, relations, doc, lazyEv)

    if(rOpt == None) return None

    ClassicalImplementation.ς(doc, rOpt.get, spanners(a).automaton.V, v1, v2)
  }

}

/**
  * Represents the join based on the 'follows' predicate.
  *
  * @param a1
  * @param a2
  * @param a3
  * @param var1
  * @param var2
  * @param min
  * @param max
  */
case class Follows(val a1:String, val a2:String, val a3:String, val var1:SVar, val var2:SVar, val min:Int, val max:Int) extends Operation(a3) {

  override def perform(spanners:CoreSpannersCollection[String, CoreSpanner]):Option[CoreSpanner] = {

    val _a1 = spanners(a1)
    val _a2 = spanners(a2)

    val _a3Opt = _a1.follows(_a2, var1, var2, min, max)

    _a3Opt
  }

  override def execute(spanners:CoreSpannersCollection[String, CoreSpanner], relations:VSRelationsCollection[String, VSRelation[VSTuple]], doc:String, lazyEv:Boolean = false): Option[VSRelation[VSTuple]] = {

    val r1Opt = getRelation(a1, spanners, relations, doc, lazyEv)
    if(r1Opt == None) return None
    val r2Opt = getRelation(a2, spanners, relations, doc, lazyEv)
    if(r1Opt == None) return None

    val pars = new Array[Int](2)
    pars(0) = min
    pars(1) = max

    // Do the join with the 'follows' predicate
    ClassicalImplementation.genericJoin(r1Opt.get, spanners(a1).automaton.V, var1, r2Opt.get, spanners(a2).automaton.V, var2, ClassicalImplementation.follows, pars)
  }
}

/**
  * Represents the join based on the 'isWithin' predicate.
  *
  * @param a1
  * @param a2
  * @param a3
  * @param var1
  * @param var2
  * @param min
  * @param max
  */
case class IsWithin(val a1:String, val a2:String, val a3:String, val var1:SVar, val var2:SVar, val min:Int, val max:Int) extends Operation(a3) {

  override def perform(spanners:CoreSpannersCollection[String, CoreSpanner]):Option[CoreSpanner] = {

    val _a1 = spanners(a1)
    val _a2 = spanners(a2)

    val _a3Opt = _a1.isWithin(_a2, var1, var2, min, max)

    _a3Opt
  }

  override def execute(spanners:CoreSpannersCollection[String, CoreSpanner], relations:VSRelationsCollection[String, VSRelation[VSTuple]], doc:String, lazyEv:Boolean = false): Option[VSRelation[VSTuple]] = {

    val r1Opt = getRelation(a1, spanners, relations, doc, lazyEv)
    if(r1Opt == None) return None
    val r2Opt = getRelation(a2, spanners, relations, doc, lazyEv)
    if(r1Opt == None) return None

    val pars = new Array[Int](2)
    pars(0) = min
    pars(1) = max

    // Do the join with the 'isWithin' predicate
    ClassicalImplementation.genericJoin(r1Opt.get, spanners(a1).automaton.V, var1, r2Opt.get, spanners(a2).automaton.V, var2, ClassicalImplementation.isWithin, pars)
  }
}

/**
  * Represents a generic extractor of an operation pattern in a string.
  */
abstract class OperationExtractor {

  /**
    * Extracts an operation from the given string.
    *
    * @param s
    * @return
    */
  def extract(s:String):Option[Operation]
}

/**
  * Represents an extractor of a join operation.
  */
object joinExtractor extends OperationExtractor {

  val joinPatternS = "\\s*(\\S+)\\s*=\\s*(\\S+)\\s*⋈\\s*(\\S+)\\s*"
  val joinPattern = joinPatternS.r

  override def extract(s:String):Option[Operation] = {

    if(s.matches(joinPatternS)) {

      val joinPattern(a3, a1, a2) = s

      return Some(new ⋈(a1, a2, a3))
    }
    None
  }
}

/**
  * Represents an extractor of a union operation.
  */
object unionExtractor extends OperationExtractor{

  val unionPatternS = "\\s*(\\S+)\\s*=\\s*(\\S+)\\s*∪\\s*(\\S+)\\s*"
  val unionPattern = unionPatternS.r

  override def extract(s:String):Option[Operation] = {

    if(s.matches(unionPatternS)) {

      val unionPattern(a3, a1, a2) = s

      return Some(new ∪(a1, a2, a3))
    }
    None
  }
}

/**
  * Represents an extractor of a projection operation.
  */
object projectionExtractor extends OperationExtractor{

  val projectionPatternS = "\\s*(\\S+)\\s*=\\s*π\\s*\\((\\S+)\\)\\s*(\\S+)\\s*"
  val projectionPattern = projectionPatternS.r
  var varsPattern = "\\s*\\d(\\s*,\\s*\\d\\s*)*"

  override def extract(s:String):Option[Operation] = {

    if(s.matches(projectionPatternS)) {

      val projectionPattern(a2, vs, a) = s

      if(vs.matches(varsPattern)) {

        val vars = vs.split(",").map(_.toInt)

        val set = new SVars[SVar] ++ vars

        return Some(new π(a, a2, set))
      }

    }
    None
  }
}

/**
  * Represents an extractor of a selection operation.
  */
object selectionExtractor extends OperationExtractor {

  val selectionPatternS = "\\s*(\\S+)\\s*=\\s*ς\\s*\\(\\s*(\\d)\\s*,\\s*(\\d)\\s*\\)\\s*(\\S+)\\s*"
  val selectionPattern = selectionPatternS.r

  override def extract(s:String):Option[Operation] = {

    if(s.matches(selectionPatternS)) {

      val selectionPattern(a2, v1, v2, a) = s

      return Some(new ς(a, a2, v1.toInt, v2.toInt))
    }
    None
  }
}

/**
  * Represents an extractor of a join operation based on the 'follows' predicate.
  */
object followsExtractor extends OperationExtractor {

  val followsPatternS = "\\s*(\\S+)\\s*=\\s*\\(\\s*(\\S+)\\s*,\\s*(\\d+)\\s*\\)\\s*follows\\s*\\(\\s*(\\S+)\\s*,\\s*(\\d+)\\s*\\)\\s*\\(\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\)\\s*"
  val followsPattern = followsPatternS.r

  override def extract(s:String):Option[Operation] = {

    if(s.matches(followsPatternS)) {

      val followsPattern(a3, a1, v1, a2, v2, min, max) = s

      return Some(new Follows(a1, a2, a3, v1.toInt, v2.toInt, min.toInt, max.toInt))
    }
    None
  }
}

/**
  * Represents an extractor of a join operation based on the 'isWithin' predicate.
  */
object isWithinExtractor extends OperationExtractor {

  val isWithinPatternS = "\\s*(\\S+)\\s*=\\s*\\(\\s*(\\S+)\\s*,\\s*(\\d+)\\s*\\)\\s*isWithin\\s*\\(\\s*(\\S+)\\s*,\\s*(\\d+)\\s*\\)\\s*\\(\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\)\\s*"
  val isWithinPattern = isWithinPatternS.r

  override def extract(s:String):Option[Operation] = {

    if(s.matches(isWithinPatternS)) {

      val isWithinPattern(a3, a1, v1, a2, v2, min, max) = s

      return Some(new Follows(a1, a2, a3, v1.toInt, v2.toInt, min.toInt, max.toInt))
    }
    None
  }
}

/**
  * Collects the operation extractors available.
  */
object OperationExtractors {

  val π = projectionExtractor
  val ⋈ = joinExtractor
  val ∪ = unionExtractor
  val ς = selectionExtractor
  val follows = followsExtractor
  val isWithin = isWithinExtractor

  val buf = new ArrayBuffer[OperationExtractor]()

  buf += π
  buf += ⋈
  buf += ∪
  buf += ς
  buf += follows
  buf += isWithin

  def apply(i:Int):OperationExtractor = buf(i)
  def size = buf.size
}