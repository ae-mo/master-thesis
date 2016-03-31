package runtime

import org.scalatest.FunSuite

class runtimeTestSuite extends FunSuite {
  
  def progs = new {
    
    val rgx1 = new Array[Instruction](3)
    
    // matches "ab"
    rgx1(0) = new Instruction(InstructionType.CHAR, 97, -1, 0, null, null)
    rgx1(1) = new Instruction(InstructionType.CHAR, 98, -1, 1, null, null)
    rgx1(2) = new Instruction(InstructionType.MATCH, -1, -1, 2, null, null)
    
    val rgx2 = new Array[Instruction](4)
    
    // matches "a+b"
    rgx2(0) = new Instruction(InstructionType.CHAR, 97, -1, 0, null, null)
    rgx2(2) = new Instruction(InstructionType.CHAR, 98, -1, 2, null, null)
    rgx2(1) = new Instruction(InstructionType.SPLIT, -1, -1, 1, rgx2(0), rgx2(2))
    rgx2(3) = new Instruction(InstructionType.MATCH, -1, -1, 3, null, null)
   
    val rgx3 = new Array[Instruction](5)
    
    // matches "a*b"
    rgx3(1) = new Instruction(InstructionType.CHAR, 97, -1, 1, null, null)
    rgx3(3) = new Instruction(InstructionType.CHAR, 98, -1, 3, null, null)
    rgx3(0) = new Instruction(InstructionType.SPLIT, -1, -1, 0, rgx3(1), rgx3(3))
    rgx3(2) = new Instruction(InstructionType.JMP, -1, -1, 2, rgx3(0), null)
    rgx3(4) = new Instruction(InstructionType.MATCH, -1, -1, 4, null, null)
    
    val rgx4 = new Array[Instruction](11)
    
    // matches "a*|b*c+"
    rgx4(2) = new Instruction(InstructionType.CHAR, 97, -1, 2, null, null)
    rgx4(4) = new Instruction(InstructionType.MATCH, -1, -1, 4, null, null)
    rgx4(6) = new Instruction(InstructionType.CHAR, 98, -1, 6, null, null)
    rgx4(8) = new Instruction(InstructionType.CHAR, 99, -1, 8, null, null)
    rgx4(10) = new Instruction(InstructionType.MATCH, -1, -1, 10, null, null)
    rgx4(1) = new Instruction(InstructionType.SPLIT, -1, -1, 1, rgx4(2), rgx4(4))
    rgx4(3) = new Instruction(InstructionType.JMP, -1, -1, 3, rgx4(1), null)
    rgx4(5) = new Instruction(InstructionType.SPLIT, -1, -1, 5, rgx4(6), rgx4(8))
    rgx4(9) = new Instruction(InstructionType.SPLIT, -1, -1, 9, rgx4(8), rgx4(10))
    rgx4(7) = new Instruction(InstructionType.JMP, -1, -1, 7, rgx4(5), null)
    rgx4(0) = new Instruction(InstructionType.SPLIT, -1, -1, 0, rgx4(1), rgx4(5))
  }
  
  def strings = new {
    
    val str1 = new Array[Int](3)
    str1(0) = 97
    str1(1) = 98
    str1(2) = -1
    
    val str2 = new Array[Int](3)
    str2(0) = 98
    str2(1) = 97
    str2(2) = -1
    
    val str3 = new Array[Int](4)
    str3(0) = 97
    str3(1) = 97
    str3(2) = 98
    str3(3) = -1
   
    val str4 = new Array[Int](2)
    str4(0) = 98
    str4(1) = -1
    
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
  
}