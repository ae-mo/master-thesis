package be.ac.ulb.arc.runtime

import org.scalatest.FunSuite

class runtimeTestSuite extends FunSuite {
  
  def progs = new {
    
    val rgx1 = new Array[Instruction](3)
    
    // matches "ab"
    rgx1(0) = new Instruction(InstructionType.CHAR, 'a', -1, 0, null, null)
    rgx1(1) = new Instruction(InstructionType.CHAR, 'b', -1, 1, null, null)
    rgx1(2) = new Instruction(InstructionType.MATCH, -1, -1, 2, null, null)
    
    val rgx2 = new Array[Instruction](4)
    
    // matches "a+b"
    rgx2(0) = new Instruction(InstructionType.CHAR, 'a', -1, 0, null, null)
    rgx2(2) = new Instruction(InstructionType.CHAR, 'b', -1, 2, null, null)
    rgx2(1) = new Instruction(InstructionType.SPLIT, -1, -1, 1, rgx2(0), rgx2(2))
    rgx2(3) = new Instruction(InstructionType.MATCH, -1, -1, 3, null, null)
   
    val rgx3 = new Array[Instruction](5)
    
    // matches "a*b"
    rgx3(1) = new Instruction(InstructionType.CHAR, 'a', -1, 1, null, null)
    rgx3(3) = new Instruction(InstructionType.CHAR, 'b', -1, 3, null, null)
    rgx3(0) = new Instruction(InstructionType.SPLIT, -1, -1, 0, rgx3(1), rgx3(3))
    rgx3(2) = new Instruction(InstructionType.JMP, -1, -1, 2, rgx3(0), null)
    rgx3(4) = new Instruction(InstructionType.MATCH, -1, -1, 4, null, null)
    
    val rgx4 = new Array[Instruction](11)
    
    // matches "a*|(b*c+)"
    rgx4(2) = new Instruction(InstructionType.CHAR, 'a', -1, 2, null, null)
    rgx4(4) = new Instruction(InstructionType.MATCH, -1, -1, 4, null, null)
    rgx4(6) = new Instruction(InstructionType.CHAR, 'b', -1, 6, null, null)
    rgx4(8) = new Instruction(InstructionType.CHAR, 'c', -1, 8, null, null)
    rgx4(10) = new Instruction(InstructionType.MATCH, -1, -1, 10, null, null)
    rgx4(1) = new Instruction(InstructionType.SPLIT, -1, -1, 1, rgx4(2), rgx4(4))
    rgx4(3) = new Instruction(InstructionType.JMP, -1, -1, 3, rgx4(1), null)
    rgx4(5) = new Instruction(InstructionType.SPLIT, -1, -1, 5, rgx4(6), rgx4(8))
    rgx4(9) = new Instruction(InstructionType.SPLIT, -1, -1, 9, rgx4(8), rgx4(10))
    rgx4(7) = new Instruction(InstructionType.JMP, -1, -1, 7, rgx4(5), null)
    rgx4(0) = new Instruction(InstructionType.SPLIT, -1, -1, 0, rgx4(1), rgx4(5))
    
    // matches (a+)(b)
    val rgx5 = new Array[Instruction](8)
    
    rgx5(1) = new Instruction(InstructionType.CHAR, 'a', -1, 1, null, null)
    rgx5(5) = new Instruction(InstructionType.CHAR, 'b', -1, 5, null, null)
    rgx5(7) = new Instruction(InstructionType.MATCH, -1, -1, 7, null, null)
    rgx5(0) = new Instruction(InstructionType.SAVE, -1, 0, 0, rgx5(1), null)
    rgx5(4) = new Instruction(InstructionType.SAVE, -1, 2, 4, rgx5(5), null)
    rgx5(3) = new Instruction(InstructionType.SAVE, -1, 1, 3, rgx5(4), null)
    rgx5(6) = new Instruction(InstructionType.SAVE, -1, 3, 6, rgx5(7), null)
    rgx5(2) = new Instruction(InstructionType.SPLIT, -1, -1, 2, rgx5(1), rgx5(3))
    
    // matches \s\d+\s
    val rgx6 = new Array[Instruction](5)
    
    rgx6(0) = new Instruction(InstructionType.WHITESPACE, -1, -1, 0, null, null)
    rgx6(1) = new Instruction(InstructionType.DIGIT, -1, -1, 1, null, null)
    rgx6(3) = new Instruction(InstructionType.WHITESPACE, '\0', -1, 3, null, null)
    rgx6(2) = new Instruction(InstructionType.SPLIT, -1, -1, 2, rgx6(1), rgx6(3))
    rgx6(4) = new Instruction(InstructionType.MATCH, -1, -1, 4, null, null)
    
    // matches .*
    val rgx7 = new Array[Instruction](4)
    
    rgx7(1) = new Instruction(InstructionType.DOT, -1, -1, 1, null, null)
    rgx7(3) = new Instruction(InstructionType.MATCH, -1, -1, 3, null, null)
    rgx7(0) = new Instruction(InstructionType.SPLIT, -1, -1, 0, rgx7(1), rgx7(3))
    rgx7(2) = new Instruction(InstructionType.JMP, -1, -1, 2, rgx7(0), null)
  }
  
  def strings = new {
    
    val str1 = "ab".toCharArray()  :+ '\0'
    
    val str2 = "ba".toCharArray()  :+ '\0'
    
    val str3 = "aab".toCharArray()  :+ '\0'
   
    val str4 = "b".toCharArray()  :+ '\0'
    
    val str5 = " 923 ".toCharArray()  :+ '\0'
    
    val str6 = " aab ".toCharArray()  :+ '\0'
    
    val str7 = " 12a ".toCharArray()  :+ '\0'
    
    val str8 = "	1 ".toCharArray()  :+ '\0'
    
    val str9 = "".toCharArray()  :+ '\0'
    
  }
  
  
  test("The VM should say rgx1 matches str1, not str2, not str3") {
    
    vm.evaluate(progs.rgx1, strings.str1)
    assert(runtime.matched)
    
    vm.evaluate(progs.rgx1, strings.str2)
    assert(!runtime.matched)
    
    vm.evaluate(progs.rgx1, strings.str3)
    assert(!runtime.matched)
     
    
  }
  
  test("The VM should say rgx2 matches str1, not str2, not str4") {
    
    vm.evaluate(progs.rgx2, strings.str1)
    assert(runtime.matched)
    
    vm.evaluate(progs.rgx2, strings.str2)
    assert(!runtime.matched)
    
    vm.evaluate(progs.rgx2, strings.str4)
    assert(!runtime.matched)
    
  }
  
  test("The VM should say rgx3 matches str3, str4") {
    
    vm.evaluate(progs.rgx3, strings.str3) 
    assert(runtime.matched)
    
    vm.evaluate(progs.rgx3, strings.str4)
    assert(runtime.matched)
    
  }
  
  test("The VM should say rgx4 matches not str1, not str2, not str3, not str4") {
    
    vm.evaluate(progs.rgx4, strings.str1) 
    assert(!runtime.matched)
   
    vm.evaluate(progs.rgx4, strings.str2) 
    assert(!runtime.matched)
    
    vm.evaluate(progs.rgx4, strings.str3) 
    assert(!runtime.matched)
    
    vm.evaluate(progs.rgx4, strings.str4) 
    assert(!runtime.matched)
    
  }
  
  
  test("The VM should say rgx5 returns submatches (0,2) (2,3) on str3") {
    
    val saved:Array[Int] = vm.evaluate(progs.rgx5, strings.str3) 
    assert(runtime.matched)
    
    assert(saved(0) == 0)
    assert(saved(1) == 2)
    assert(saved(2) == 2)
    assert(saved(3) == 3)
    
  }
  
  test("The VM should say rgx6 matches str5, not str1, not str6, not str7, str8") {
    
    vm.evaluate(progs.rgx6, strings.str5) 
    assert(runtime.matched)
    
    vm.evaluate(progs.rgx6, strings.str1) 
    assert(!runtime.matched)
    
    vm.evaluate(progs.rgx6, strings.str6) 
    assert(!runtime.matched)
    
    vm.evaluate(progs.rgx6, strings.str7) 
    assert(!runtime.matched)
    
    vm.evaluate(progs.rgx6, strings.str8) 
    assert(runtime.matched)

  }
  
  test("The VM should say rgx7 matches str1, str2, str3, str4, str5, str6, str7, str8, str9") {
    
    vm.evaluate(progs.rgx7, strings.str1) 
    assert(runtime.matched)
    
    vm.evaluate(progs.rgx7, strings.str2) 
    assert(runtime.matched)
    
    vm.evaluate(progs.rgx7, strings.str3) 
    assert(runtime.matched)
    
    vm.evaluate(progs.rgx7, strings.str4) 
    assert(runtime.matched)
    
    vm.evaluate(progs.rgx7, strings.str5) 
    assert(runtime.matched)
    
    vm.evaluate(progs.rgx7, strings.str6) 
    assert(runtime.matched)
    
    vm.evaluate(progs.rgx7, strings.str7) 
    assert(runtime.matched)
    
    vm.evaluate(progs.rgx7, strings.str8) 
    assert(runtime.matched)
    
    vm.evaluate(progs.rgx7, strings.str9) 
    assert(runtime.matched)
  }
  
}