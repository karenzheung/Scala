package vm

import bc.ByteCode

/**
 * Created by user on 10/19/2015.
 *
 * primary constructor that creates a VirtualMachine
 * with a given list.
 */
class MyVirtualMachine(ls: List[Int]) extends VirtualMachine {
  val vms = ls

  /**
  * auxillary constructor that creates a VirtualMachine
   * with the stack to an empty list
   */
  def this() {
    this(List[Int]())
  }

  object MyVirtualMachine{
    def apply() = new MyVirtualMachine
  }

  /**
   * Executes a vector of bytecodes.
   *
   * Note, that this is an "immutable" object. That is, it
   * will return a new virtual machine after executing each
   * of the bytecode objects in the vector.
   *
   * @param bc a vector of bytecodes
   * @return a new virtual machine
   */
  def execute(bc: Vector[ByteCode]): VirtualMachine = {
    executeHelper(this, bc)
  }

  /**
   * Executes a vector of bytecodes.
   *
   * Recursive method that essentially execute each byteCode one by one
   * and recursively execute the next byteCode on the new VirtualMachine
   * until the Vector of bytecode has no more bytecode to execute
   *
   * @param bc a vector of bytecodes,  vm the virtual machine
   * @return a new virtual machine
   */
  def executeHelper(vm : VirtualMachine, bc: Vector[ByteCode]): VirtualMachine = {
    if(bc.length == 0)
      vm
    else {
      val meh = vm.executeOne(bc)
      executeHelper(meh._2,meh._1)
    }

  }
  /**
   * Executes the next bytecode in the vector of bytecodes.
   *
   * This method only executes a single byte code in the vector of bytecodes.
   * It returns a tuple of the new vector of bytecodes (with the executed
   * bytecode removed) and the new virtual machine.
   *
   * You may assume that `bc` contains at least 1 bytecode.
   *
   * @param bc the vector of bytecodes
   * @return a tuple of a new vector of bytecodes and virtual machine
   */
  def executeOne(bc: Vector[ByteCode]): (Vector[ByteCode], VirtualMachine) = {
      val result = bc(0).execute(this)
      (bc.tail, result)

  }

  /**
   * Pushes an integer value onto the virtual machine stack.
   *
   * @param value the integer to push
   * @return a new virtual machine with the integer `value` pushed
   */
  def push(value: Int): VirtualMachine = {
     val v = new MyVirtualMachine(value :: vms)
      v

  }
  /**
   * Pops an integer value off of the virtual machine stack.
   *
   * @return (i, vm), where i is the integer popped and vm is the
   *         new virtual machine
   */

  def pop(): (Int, VirtualMachine) = {
    if(vms.size == 0) throw new MachineUnderflowException("not enough arguments")
    val first = vms.head
    val v = new MyVirtualMachine(vms.tail)
    (first, v)
  }

  /**
   * Returns the state of the virtual machine stack.
   *
   * The value at index 0 is the value on the top of the stack.
   *
   * @return the state of the stack
   */

  def state: Vector[Int] = vms.toVector

}