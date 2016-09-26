package factory

import adapter.ProgramParserAdapter
import bc._
import vendor.{ProgramParser, VendorProgramParser}
import vm.{VirtualMachineParser, MyVirtualMachine, VirtualMachine}

/**
 * The `VirtualMachineFactory` follows the *factory pattern*. It provides
 * methods for each of the important parts in this assignment. You must
 * implement each method such that it returns an object of the correct type.
 */
object VirtualMachineFactory {
  // TODO
  def byteCodeFactory: ByteCodeFactory = ByteCodeFact
    // TODO

  def vendorParser: ProgramParser = VendorProgramParser
    // TODO

  def byteCodeParser: ByteCodeParser = MyByteCodeParser
    // TODO

  def virtualMachineParser: VirtualMachineParser = ProgramParserAdapter
  // TODO
  def virtualMachine: VirtualMachine = {
    val vm = new MyVirtualMachine()
    vm
  }
}
