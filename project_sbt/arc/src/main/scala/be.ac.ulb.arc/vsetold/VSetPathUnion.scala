package be.ac.ulb.arc.vsetold

import dk.brics.automaton.{Automaton, RegExp}

import scala.collection.mutable.{ArrayBuffer, Map}

class VSetPathUnion(var pathUnion: ArrayBuffer[ArrayBuffer[(String,  Map[String, String], Int)]], var vars:Array[String], var finalStates:Array[Int]) {
  
  /**
   * Transforms a vset path union into an hybrid path union.
   */
  def toHybridPathUnion():HybridPathUnion = {
  
  		// Replace each transition with an automaton
  		var hybridPathUnion = new ArrayBuffer[ArrayBuffer[(Automaton, Map[String, String], Int)]]()
  
  				for(path <- pathUnion) {
  
  					var newPath = new ArrayBuffer[(Automaton, Map[String, String], Int)]()
  
  							for((e, ops, t) <- path if ops != null) {
  
  								val a:Automaton = (new RegExp(e)).toAutomaton()
  										a.expandSingleton()
  
  										newPath += ((a, ops, t))
  							}
  
  					hybridPathUnion += newPath
  
  				}
  
  		new HybridPathUnion(hybridPathUnion, vars)
  
  }
}