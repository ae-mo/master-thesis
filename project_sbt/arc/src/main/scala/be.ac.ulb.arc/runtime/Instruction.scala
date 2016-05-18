package be.ac.ulb.arc.runtime

class Instruction(val opCode: InstructionType.Value, val c: Int, var i: Int, val num: Int, var x: Instruction, var y: Instruction)

