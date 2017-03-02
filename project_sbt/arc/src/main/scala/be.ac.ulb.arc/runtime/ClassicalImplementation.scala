package be.ac.ulb.arc.runtime

import scala.collection.immutable.{HashSet => SVars}
import scala.{Int => SVar}
import scala.collection.mutable.{HashSet => VSRelation}
import be.ac.ulb.arc.runtime.{StringPointerCollection => VSTuple}
import scala.collection.mutable.{Map => VSRelationsCollection}
import be.ac.ulb.arc.core.AQLCoreFragmentSpecification
/**
  * Represents an interpreter that views each operation in an AQL fragment as an operation on relations.
  */
object ClassicalInterpreter {

  /**
    * Executes an AQL fragment on a document.
    *
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

        if(rOpt != None)
          relations += ((k, rOpt.get))
      }

      // execute the operations
      for(op <- fragment.operations) {

        result = op.execute(fragment.spanners, relations, doc)
        if(result != None)
          relations += ((op.res, result.get))
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

  def Ωo(table:VSRelation[VSTuple], v:SVar):VSRelation[VSTuple] = {

    if(table.size <= 1) return table

    val par = new Array[Int](1)
    par(0) = 0
    var result = new VSRelation[VSTuple]
    val V = new SVars + v

    // Sort on beginning index
    var list = sort(table.toList, v, follows, par)

    var list2 = list.tail

    var current = list.head

    while(!list2.isEmpty) {

      while(!list2.isEmpty && current(v*2 + 1) >= list2.head(v*2)) {

        current = mergeTuples(current, V, v, list2.head, V, v, V, v)
        list2 = list2.tail
      }

      result += current

      if(!list2.isEmpty) {

        list = list2
        current = list2.head
        list2 = list2.tail
      }
    }

    result += current

    result

  }

  def β(table:VSRelation[VSTuple], v:SVar, dist:Int, count:Int):List[VSTuple] = {

    if(table.size <= 1) return List[VSTuple]() ++ table

    val par = new Array[Int](1)
    par(0) = 0
    var result = List[VSTuple]()
    val V = new SVars + v

    // Sort on beginning index
    var list = sort(table.toList, v, follows, par)

    val pars = new Array[Int](2)
    pars(0) = 0
    pars(1) = dist

    var list2 = list.tail

    var current = list.head

    while(!list2.isEmpty) {

      var counter = count

      while(!list2.isEmpty && follows(current, v, list2.head, v, pars)) {

        current = mergeTuples(current, V, v, list2.head, V, v, V, v)
        list2 = list2.tail
        counter -= 1
      }

      if(counter <= 0)
        result = result :+ current

      if(!list2.isEmpty) {

        list = list2
        current = list2.head
        list2 = list2.tail
      }

    }

    result
  }

  /**
    * Adjusts the lists used in the β method.
    * @param prev
    * @param current
    * @return
    */
  def adjustLists(prev:List[VSTuple], current:List[VSTuple]): (List[VSTuple], List[VSTuple]) = {

    if(prev.head == current.head) {

      return (prev.tail, current.tail)
    }
    else {

      (prev, prev.head :: current.tail)
    }

  }

  /**
    * Performs the projection on the desired span variables of the given (V, s)-relation.
    *
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
    *
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
    var table3 = new VSRelation[VSTuple]

    // Do the hash join
    if(varsInt.size > 0) {
      val hashedT = if(table1.size < table2.size) table1 else table2
      val varsH = if(table1.size < table2.size) vars1 else vars2
      val scannedT = if(table1.size >= table2.size) table1 else table2
      val varsS = if(table1.size >= table2.size) vars1 else vars2

      val h = hashedT.groupBy((t:VSTuple) => hash(t, sortedVarsInt)) withDefaultValue new VSRelation[VSTuple]
      table3 = table3 ++ scannedT.flatMap((t:VSTuple) => h(hash(t, sortedVarsInt)).map(mergeTuples(t, varsS, -1, _, varsH, -1, vars3)))

    }
    // Do the cartesian product
    else {

      for(t1 <- table1; t2 <- table2) {

        table3 = table3 + mergeTuples(t1, vars1, -1, t2, vars2, -1, vars3)
      }
    }

    if(table3.size > 0) Some(table3)
    else None

  }

  /**
    * Merges two tuples that agree on common variables.
    *
    * @param t1
    * @param v1
    * @param t2
    * @param v2
    * @param vars
    * @return
    */
  def mergeTuples(t1:VSTuple, vs1:SVars[SVar], v1:SVar, t2:VSTuple, vs2:SVars[SVar], v2:SVar, vars:SVars[SVar], v3:SVar = -1):VSTuple = {

    var t3:StringPointerArray = null

    if(v3 == -1) {

      t3 = new StringPointerArray(vars)

      for(v <- vars) {

        if(vs1.contains(v)) {

          t3(v*2) = t1(v*2)
          t3(v*2 + 1) = t1(v*2 + 1)
        }
        else {

          t3(v*2) = t2(v*2)
          t3(v*2 + 1) = t2(v*2 + 1)
        }
      }
    }

    else {

      t3 = new StringPointerArray(new SVars + v3)
      t3(v3*2) = Math.min(t1(v1*2), t2(v2*2))
      t3(v3*2 + 1) = Math.max(t1(v1*2+1), t2(v2*2+1))

    }

    t3
  }
  /**
    * A simple hashing function to support hash-join.
    *
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
    *
    * @param table1
    * @param vars1
    * @param table2
    * @param vars2
    * @return
    */
  def ∪(table1:VSRelation[VSTuple], vars1:SVars[SVar], table2:VSRelation[VSTuple], vars2:SVars[SVar]):Option[VSRelation[VSTuple]] = {

    if(!table1.isEmpty && !table2.isEmpty && vars1 != vars2)
      return None

    val table3 = table1 ++ table2

    Some(table3)
  }

  /**
    * Performs a string equality selection on the given (V, s)-relation.
    *
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

  /**
    * Returns the join of two (V, s)-relation based on the 'follows' predicate. Uses sort-merge algorithm as implementation.
    * @param table1
    * @param vars1
    * @param var1
    * @param table2
    * @param vars2
    * @param var2
    * @param min
    * @param max
    * @return
    */
  def followsJoin(table1:VSRelation[VSTuple], vars1:SVars[SVar], var1:SVar, table2:VSRelation[VSTuple], vars2:SVars[SVar], var2:SVar, min:Int, max:Int, var3:SVar)
  :Option[VSRelation[VSTuple]] = {

    if(!vars1.contains(var1) || !vars2.contains(var2)) return None

    // When there are common span variables we
    // don't know how to merge, as here we use boolean predicates
    if(vars1.intersect(vars2).size > 0) return None

    var result = new VSRelation[VSTuple]

    // Sort the two input relations based on follows predicate
    val par1 = new Array[Int](1)
    par1(0) = 0
    val par2 = new Array[Int](1)
    par2(0) = 1
    var list1 = sort(table1.toList, var1, follows, par1)
    var list2 = sort(table2.toList, var2, follows, par2)

    val pars = new Array[Int](2)
    pars(0) = min
    pars(1) = max

    // Scan the two lists and add the matching pairs to output
    while(!list1.isEmpty && !list2.isEmpty) {

      var list2Sub = list2

      // Find the farthest tuple in the second list that satisfies the 'follows' predicate
      // merging the ones met with the current tuples from list1
      while(!list1.isEmpty && !list2Sub.isEmpty && follows(list1.head, var1, list2Sub.head, var2, pars)) {

        result += mergeTuples(list1.head, vars1, var1, list2Sub.head, vars2, var2, vars1 ++ vars2, var3)
        list2Sub = list2Sub.tail
      }

      if(!list1.isEmpty && !list2.isEmpty) {
        // if the first tuple of the second list is too near, advance second list
        if(list2.head(var2*2) - list1.head(var1*2 + 1) < min)
          list2 = list2.tail
        // if its too far instead, advance first list
        else
          list1 = list1.tail
      }

    }

    if(result.isEmpty) return None
    else Some(result)
  }

  /**
    * Joins two input (V, s)-relations based on a given boolean predicate.
    *
    * @param table1
    * @param vars1
    * @param var1
    * @param table2
    * @param vars2
    * @param var2
    * @param predicate
    * @return
    */
  def genericJoin(table1:VSRelation[VSTuple], vars1:SVars[SVar], var1:SVar, table2:VSRelation[VSTuple], vars2:SVars[SVar], var2:SVar, predicate:(VSTuple, SVar, VSTuple, SVar, Array[Int]) => Boolean, var3:SVar, pars:Array[Int] = null)
  :Option[VSRelation[VSTuple]] = {

    // When there are common span variables we
    // don't know how to merge, as here we use boolean predicates
    if(vars1.intersect(vars2).size > 0) return None

    val varsU = vars1 ++ vars2
    var table3 = new VSRelation[VSTuple]

    for(t1 <- table1; t2 <- table2) {

      // If the predicate holds, merge t1 and t2 and add
      // the result to the output relation
      if(predicate(t1, var1, t2, var2, pars))
        table3 = table3 + mergeTuples(t1, vars1, var1, t2, vars2, var2, varsU, var3)
    }

    if(table3.size == 0) return None
    else Some(table3)
  }

  /**
    * Tests if an s-tuple follows another. Can be parametrized with distance bound, or with left/right sort disambiguation.
    *
    * @param t1
    * @param v1
    * @param t2
    * @param v2
    * @param pars Bounds go here, min first.
    * @return
    */
  def follows(t1:VSTuple, v1:SVar, t2:VSTuple, v2:SVar, pars:Array[Int] = null):Boolean = {

    // If a tuple doesn't contain the specified span variable,
    // the predicate is false
    if(t1(v1*2) == -1 || t2(v2*2) == -1)
      return false

    val diff = t2(v2*2) - t1(v1*2 + 1)
    if(pars.size == 2) {

      if(diff >= pars(0) && diff <= pars(1))
        return true

      return false
    }
    else if(pars.size == 1) {

      // if left sort
      if(pars(0) == 0) {
        if(t2(v2*2+1) > t1(v1*2 + 1)) return true
        return false
      }
      else {

        if(t2(v2*2) > t1(v1*2)) return true
        return false
      }
    }

    throw new RuntimeException("Parameters needed for predicate 'follows'.")
  }

  /**
    * Tests if an s-tuple is within a distance between the bounds specified with respect to another.
 *
    * @param t1
    * @param v1
    * @param t2
    * @param v2
    * @param pars Bounds go here, min first.
    * @return
    */
  def isWithin(t1:VSTuple, v1:SVar, t2:VSTuple, v2:SVar, pars:Array[Int]):Boolean = {

    if(follows(t1, v1, t2, v2, pars) || follows(t2, v2, t1, v1, pars)) return true

    false
  }


  /**
    * Sorts a list of (V,S)-tuples based on a given predicate.
    * @param list
    * @param v
    * @param lessThan
    * @return
    */
  def sort(list: List[VSTuple], v:SVar, lessThan:(VSTuple, SVar, VSTuple, SVar, Array[Int])  => Boolean, pars:Array[Int] = null): List[VSTuple] = {

    if(list.isEmpty)
      return list

    val (less, more) = list.tail partition (lessThan(_, v, list.head, v, pars))
    sort(less, v, lessThan, pars) ::: list.head :: sort(more, v, lessThan, pars)
  }

}