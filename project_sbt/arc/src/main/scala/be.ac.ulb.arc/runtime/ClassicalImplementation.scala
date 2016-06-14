package be.ac.ulb.arc.runtime

import scala.collection.immutable.{HashSet => SVars}
import scala.{Int => SVar}
import scala.collection.immutable.{HashSet => VSRelation}
import be.ac.ulb.arc.runtime.{StringPointerCollection => VSTuple}
import scala.collection.mutable.{Map => CoreSpannersCollection}
import scala.collection.mutable.{Map => VSRelationsCollection}
import be.ac.ulb.arc.vset.AQLCoreFragmentSpecification
import be.ac.ulb.arc.vset.CoreSpanner

/**
  * Represents an interpreter that views each operation in an AQL fragment as an operation on relations.
  */
object ClassicalInterpreter {

  /**
    * Executes an AQL fragment on a document.
    * @param fragment
    * @param doc
    * @param lazyEv
    * @return
    */
  def execute(fragment:AQLCoreFragmentSpecification, doc:String, lazyEv:Boolean = false):Option[VSRelation[VSTuple]] = {

    val relations = VSRelationsCollection[String, VSRelation[VSTuple]]()

    var result:Option[VSRelation[VSTuple]] = None

    // If we don't want lazy evaluation
    if(!lazyEv) {

      // evaluate all the spanners before the operations
      for((k, sp) <- fragment.spanners) {

        val rOpt = sp.evaluate(doc)

        if(rOpt != None) relations += ((k, rOpt.get))
      }

      // execute the operations
      for(op <- fragment.operations) {

        result = op.execute(fragment.spanners, relations, doc)
      }
    }
    else {

      for(op <- fragment.operations) {

        result = op.execute(fragment.spanners, relations, doc, true)
      }
    }

    result
  }
}

/**
  * Represents a classical implementation of the relational operators of interest, and of
  * string equality selection.
  */
object ClassicalImplementation {

  /**
    * Performs the projection on the desired span variables of the given (V, s)-relation.
    * @param table
    * @param vars
    * @param projVars
    * @return
    */
  def π(table:VSRelation[VSTuple], vars:SVars[SVar], projVars:SVars[SVar]):Option[VSRelation[VSTuple]] = {

    val sortedProjVars = projVars.toArray.sortWith(_<_)
    var table2 = VSRelation[VSTuple]()

    for(t <- table) {

      val t1 = new StringPointerArray(projVars)

      for(v <- sortedProjVars) {

        t1(v*2) = t(v*2)
        t1(v*2 + 1) = t(v*2 + 1)
      }

      table2 = table2 + t1

    }

    Some(table2)
  }

  /**
    * Performs the natural join of two given (V, s)-relations.
    * @param table1
    * @param vars1
    * @param table2
    * @param vars2
    * @return
    */
  def ⋈(table1:VSRelation[VSTuple], vars1:SVars[SVar], table2:VSRelation[VSTuple], vars2:SVars[SVar]):Option[VSRelation[VSTuple]] = {

    val varsInt = vars1.intersect(vars2)
    val sortedVarsInt = varsInt.toArray.sortWith(_<_)
    val vars3 = vars1.union(vars2)
    val sortedVarsUn = vars3.toArray.sortWith(_<_)
    var table3 = VSRelation[VSTuple]()

    // Do the hash join
    if(varsInt.size > 0) {
      val hashedT = if(table1.size < table2.size) table1 else table2
      val varsH = if(table1.size < table2.size) vars1 else vars2
      val scannedT = if(table1.size >= table2.size) table1 else table2
      val varsS = if(table1.size >= table2.size) vars1 else vars2

      val h = hashedT.groupBy((t:VSTuple) => hash(t, sortedVarsInt))
      table3 = table3 ++ scannedT.flatMap((t:VSTuple) => h(hash(t, sortedVarsInt)).map(mergeTuples(t, varsS, _, varsH, vars3)))

    }
    // Do the cartesian product
    else {

      for(t1 <- table1; t2 <- table2) {

        table3 = table3 + mergeTuples(t1, vars1, t2, vars2, vars3)
      }
    }

    if(table3.size > 0) Some(table3)
    else None

  }

  /**
    * Merges two tuples that agree on common variables.
    * @param t1
    * @param v1
    * @param t2
    * @param v2
    * @param vars
    * @return
    */
  def mergeTuples(t1:VSTuple, v1:SVars[SVar], t2:VSTuple, v2:SVars[SVar], vars:SVars[SVar]):VSTuple = {

    val t3 = new StringPointerArray(vars)

    for(v <- vars) {

      if(v1.contains(v)) {

        t3(v*2) = t1(v*2)
        t3(v*2 + 1) = t1(v*2 + 1)
      }
      else {

        t3(v*2) = t2(v*2)
        t3(v*2 + 1) = t1(v*2 + 1)
      }
    }

    t3
  }
  /**
    * A simple hashing function to support hash-join.
    * @param t
    * @param vars
    * @return
    */
  def hash(t:VSTuple, vars:Array[SVar]):String = {

    var h = "("

    for(v <- vars) {

      h += t(v*2).toString + ","
      h += t(v*2 + 1).toString + ","
    }

    h+= ")"
    h

  }

  /**
    * Performs the union of two given (V, s)-relations.
    * @param table1
    * @param vars1
    * @param table2
    * @param vars2
    * @return
    */
  def ∪(table1:VSRelation[VSTuple], vars1:SVars[SVar], table2:VSRelation[VSTuple], vars2:SVars[SVar]):Option[VSRelation[VSTuple]] = {

    if(vars1 != vars2)
      return None

    val table3 = table1 ++ table2

    Some(table3)
  }

  /**
    * Performs a string equality selection on the given (V, s)-relation.
    * @param input
    * @param table
    * @param vars
    * @param var1
    * @param var2
    * @return
    */
  def ς(input:String, table:VSRelation[VSTuple], vars:SVars[SVar], var1:SVar, var2:SVar):Option[VSRelation[VSTuple]] = {

    if(!vars.contains(var1) || !vars.contains(var2))
      return None

    var table2 = VSRelation[VSTuple]()

    for(t <- table) {

      val str1 = input.substring(t(var1*2), t(var1*2+1))
      val str2 = input.substring(t(var2*2), t(var2*2+1))

      if(str1 == str2)
        table2 = table2 + t

    }

    if(table2.size != 0) Some(table2)
    else None
  }
}