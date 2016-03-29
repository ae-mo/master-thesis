package runtime

/**
 * Represents a virtual machine for regular expression programs executions.
 */
object vm {
  
  /**
   * Executes a regular expression program with a given input.
   * @param prog the program to execute
   * @param input the input for the program
   * @param saved a list of saved input indices for submatch tracking
   */
  def evaluate(prog: Array[Instruction], input: Array[Int], saved: Array[Int]):Int =  {
    
    val len = prog.length
    var cList = new Array[Thread](len)
    var nList = new Array[Thread](len)
    
    addThread(cList, new Thread(prog(0), saved))
    
    for(sp <- input) {
      
      for(t <- cList) {
        
        
        
      }
      
    }
    
    len
    
  }
  
  /**
   * Adds a thread to a given thread list, only if a thread with the same program counter doesn't exist.
   * @param l the list
   * @param t the thread to add
   */
  def addThread(l:Array[Thread], t:Thread) {
    
    if(l(t.pc.num) != null)
      l(t.pc.num) = t;
    
  }
  
}