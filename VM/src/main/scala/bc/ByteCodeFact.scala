package bc

import vm.VirtualMachine

/**
 * Created by user on 10/19/2015.
 */




  class iconst(arg: Integer) extends ByteCode {

  /**
   * A unique byte value representing iconst.
   */

    val code: Byte = bytecode("iconst")

  /**
   * Returns a new [[VirtualMachine]] after executing this bytecode operation.
   * takes an Integer and constructs it by pushing it onto the stack
   * @param vm the initial virtual machine
   * @return a new virtual machine
   */

    def execute(vm: VirtualMachine): VirtualMachine =
      vm.push(arg)
  }

  class iadd() extends ByteCode {
    /**
     * A unique byte value representing iadd.
     */
    val code: Byte = bytecode("iadd")
    /**
     * Returns a new [[VirtualMachine]] after adding 2 integers together
     * @param vm the initial virtual machine
     * @return a new virtual machine
     */

    def execute(vm: VirtualMachine): VirtualMachine = {
      val a = vm.pop()
      val b = (a._2).pop()

      b._2.push(a._1 + b._1)
    }

  }

  class isub() extends ByteCode {
    /**
     * A unique byte value representing isub.
     */
    val code: Byte = bytecode("isub")
    /**
     * Returns a new [[VirtualMachine]] after subtracting two integers
     * @param vm the initial virtual machine
     * @return a new virtual machine
     */
    def execute(vm: VirtualMachine): VirtualMachine = {
      val c = vm.pop()
      val d = (c._2).pop()
      d._2.push(c._1 - d._1)
    }
  }

  class imul() extends ByteCode {
    /**
     * A unique byte value representing imul.
     */
    val code: Byte = bytecode("imul")
    /**
     * Returns a new [[VirtualMachine]] after multiplying two integers
     * @param vm the initial virtual machine
     * @return a new virtual machine
     */
    def execute(vm: VirtualMachine): VirtualMachine = {
      val a = vm.pop()
      val b = (a._2).pop()
      b._2.push(a._1 * b._1)
    }
  }


  class idiv() extends ByteCode {
    /**
     * A unique byte value representing idiv.
     */
    val code: Byte = bytecode("idiv")
    /**
     * Returns a new [[VirtualMachine]] after dividing two integers
     * @param vm the initial virtual machine
     * @return a new virtual machine
     */
    def execute(vm: VirtualMachine): VirtualMachine = {
      val a = vm.pop()
      val b = (a._2).pop()
      b._2.push(a._1 / b._1)
    }
  }

  class irem() extends ByteCode {
    /**
     * A unique byte value representing irem.
     */
    val code: Byte = bytecode("irem")
    /**
     * Returns a new [[VirtualMachine]] after taking the remainder of two integers
     * @param vm the initial virtual machine
     * @return a new virtual machine
     */
    def execute(vm: VirtualMachine): VirtualMachine = {
      val a = vm.pop()
      val b = (a._2).pop()
      b._2.push(a._1 % b._1)
    }
  }

  class ineg() extends ByteCode {
    /**
     * A unique byte value representing ineg.
     */
    val code: Byte = bytecode("ineg")
    /**
     * Returns a new [[VirtualMachine]] after negating the first integer in the stack
     * If the integer is positive it will become negative and vice versa
     * @param vm the initial virtual machine
     * @return a new virtual machine
     */
    def execute(vm: VirtualMachine): VirtualMachine = {
      val a = vm.pop()
      a._2.push(a._1 * -1)
    }
  }

  class iinc() extends ByteCode {
    /**
     * A unique byte value representing iinc.
     */
    val code: Byte = bytecode("iinc")
    /**
     * Returns a new [[VirtualMachine]] after incrementing it by one
     * @param vm the initial virtual machine
     * @return a new virtual machine
     */
    def execute(vm: VirtualMachine): VirtualMachine = {
      val a = vm.pop()
      a._2.push(a._1 + 1)
    }
  }

  class idec() extends ByteCode {
    /**
     * A unique byte value representing idec.
     */
    val code: Byte = bytecode("idec")
    /**
     * Returns a new [[VirtualMachine]] after decrementing the integer by one
     * @param vm the initial virtual machine
     * @return a new virtual machine
     */
    def execute(vm: VirtualMachine): VirtualMachine = {
      val a = vm.pop()
      a._2.push(a._1 - 1)
    }
  }

  class iswap() extends ByteCode {
    /**
     * A unique byte value representing iswap.
     */
    val code: Byte = bytecode("iswap")
    /**
     * Returns a new [[VirtualMachine]] after swapping the first two integer
     * @param vm the initial virtual machine
     * @return a new virtual machine
     */
    def execute(vm: VirtualMachine): VirtualMachine = {
      val x = vm.pop()
      val y = (x._2).pop()
      val z = y._2.push(x._1);
      z.push(y._1)
    }
  }

  class idup() extends ByteCode {
    /**
     * A unique byte value representing idup.
     */
    val code: Byte = bytecode("idup")
    /**
     * Returns a new [[VirtualMachine]] after creating a duplicate of the first integer in stack
     * @param vm the initial virtual machine
     * @return a new virtual machine
     */
    def execute(vm: VirtualMachine): VirtualMachine = {
      val x = vm.pop()
      x._2.push(x._1);
      x._2.push(x._1);
    }
  }

  class print() extends ByteCode {
    /**
     * A unique byte value representing print.
     */
    val code: Byte = bytecode("print")
    /**
     * Returns a new [[VirtualMachine]] after printing the top integer to the console
     * @param vm the initial virtual machine
     * @return a new virtual machine
     */
    def execute(vm: VirtualMachine): VirtualMachine = {
      val x = vm.pop()
      println(x._1)
      vm
    }
  }



object ByteCodeFact extends ByteCodeFactory{
  /**
   * Returns a [[ByteCode]].
   *
   * This method creates a new [[ByteCode]] object given the `byte`
   * that corresponds to the bytecode (see [[ByteCodeValues]]. If
   * the bytecode requires arguments then an optional integer
   * argument is provided.
   *
   * This method should throw an [[InvalidBytecodeException]] if the
   * given bytecode value is unknown.
   *
   * Checks if the input byte has the same value of one of the 12 allowable byte commands
   * When match is found create that byte
   * if the given byte does not match with one of the 12 allowable bytes
   * then throw InvalidByteException
   *
   * @param byte  the byte code of a bytecode
   * @param args  an optional integer argument (depends on bytecode)
   * @return a new bytecode object
   */

  def make(byte: Byte, args: Int*): ByteCode = {
    if (byte == new iconst(0).code) new iconst(args(0))
    else if (byte == new iadd().code) new iadd()
    else if (byte == new isub().code) new isub()
    else if (byte == new imul().code) new imul()
    else if (byte == new idiv().code) new idiv()
    else if (byte == new irem().code) new irem()
    else if (byte == new iinc().code) new iinc()
    else if (byte == new idec().code) new idec()
    else if (byte == new ineg().code) new ineg()
    else if (byte == new idup().code) new idup()
    else if (byte == new iswap().code) new iswap()
    else if (byte == new print().code) new print()
    else throw new InvalidBytecodeException("Not a valid bytecode")
  }
  }




