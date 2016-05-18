package be.ac.ulb.arc.runtime

object main {
  
  
    // matches "a*|b*c+"
    var rgx4 = new Array[Instruction](11)
    rgx4(2) = new Instruction(InstructionType.CHAR, 'a', -1, 2, null, null)
    rgx4(4) = new Instruction(InstructionType.MATCH, -1, -1, 4, null, null)
    rgx4(6) = new Instruction(InstructionType.CHAR, 'b', -1, 6, null, null)
    rgx4(8) = new Instruction(InstructionType.CHAR, 99, -1, 8, null, null)
    rgx4(10) = new Instruction(InstructionType.MATCH, -1, -1, 10, null, null)
    rgx4(1) = new Instruction(InstructionType.SPLIT, -1, -1, 1, rgx4(2), rgx4(4))
    rgx4(3) = new Instruction(InstructionType.JMP, -1, -1, 3, rgx4(1), null)
    rgx4(5) = new Instruction(InstructionType.SPLIT, -1, -1, 5, rgx4(6), rgx4(8))
    rgx4(9) = new Instruction(InstructionType.SPLIT, -1, -1, 9, rgx4(8), rgx4(10))
    rgx4(7) = new Instruction(InstructionType.JMP, -1, -1, 7, rgx4(5), null)
    rgx4(0) = new Instruction(InstructionType.SPLIT, -1, -1, 0, rgx4(1), rgx4(5))
    
    val str1 = new Array[Char](3)
    str1(0) = 'b'
    str1(1) = 'a'
    str1(2) = '\0'
    
    def main(args: Array[String]) {
    
      vm.evaluate(rgx4, str1)
    }
    
  
}