package adapter

import bc._
import com.sun.org.apache.bcel.internal.generic.InstructionList
import vendor.{VendorProgramParser, InvalidInstructionFormatException, Instruction, ProgramParser}
import vm.VirtualMachineParser
import scala.io.Source

/**
 * Created by user on 10/18/2015.
 */
object ProgramParserAdapter extends VirtualMachineParser with ByteCodeValues {
  /**
   * Returns a vector of [[bc.ByteCode]].
   *
   * This method parses a file into a vector of bytecode objects.
   * Note, this method should throw a [[bc.InvalidBytecodeException]]
   * if it fails to parse a program file correctly.
   *
   * @param file the file containing a program
   * @return a vector of bytecodes
   */
  override def parse(file: String): Vector[ByteCode] = {
    var byte = Vector[ByteCode]()
      for (line <- Source.fromFile(file).getLines) {
        byte = byte ++: parseString(line)
      }
      byte

  }
  /**
   * Returns a vector of [[bc.ByteCode]].
   *
   * This method parses a string into a vector of bytecode objects.
   * Note, this method should throw a [[bc.InvalidBytecodeException]]
   * if it fails to parse a program string correctly.
   *
   * @param str a string containing a program
   * @return a vector of bytecodes
   */
  override def parseString(str: String): Vector[ByteCode] = {
    val vendorParser = VendorProgramParser
    val byteParser = MyByteCodeParser
    val instructions = vendorParser.parseString(str)

    var byteVector = Vector[Byte]()
    for (a <- instructions) {
      if (!names.contains(a.name)) {     //case for when the byte is not one of 12 commands. Ex: destroy
        throw new InvalidBytecodeException("not a valid command")
      }
      if (a.name == "iconst" && a.args.length != 1)  //iconst needs to have an Integer for argument
        throw new InvalidBytecodeException("iconst need an argument")
      byteVector = byteVector ++: Vector(bytecode(a.name)) //makes vector of byte names
      if (a.args.length != 0)                    //make vector of byte verison of Int
        byteVector = byteVector ++: Vector(a.args(0).toByte)
    }

    val bytes = byteParser.parse(byteVector)
    bytes


  }
}


