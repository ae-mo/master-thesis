package be.ac.ulb.amorcian.arc.runtime

class Instruction(val opCode: InstructionType.Value, val c: Int, val i: Int, val num: Int, val x: Instruction, val y: Instruction)

