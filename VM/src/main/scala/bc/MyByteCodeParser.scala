package bc

/**
 * Created by user on 10/19/2015.
 */
object MyByteCodeParser extends ByteCodeParser {

  /**
   * Parses a vector of `Byte` into a vector of `ByteCode`.
   *
   * Reads a Vector[Byte] and outputs a Vector[ByteCode]
   * Iterates through each byte in the vector and uses
   * ByteCodeFactory to make a byteCode of the byte
   * Concatantes the ByteCode together to form a Vector[ByteCode]
   *
   * @param bc  a vector of bytes representing bytecodes
   * @return    a vector of `ByteCode` objects
   */

  def parse(bc: Vector[Byte]): Vector[ByteCode] = {
   var ByteCodess = Vector[ByteCode]()
    var index = 0
    while (index < bc.length){
      val bcf = ByteCodeFact
      if (bc(index) == bytecode("iconst")) {        //if the byte is iconst the the following byte will be the Integer argument
        ByteCodess = ByteCodess ++: Vector(bcf.make(bc(index), bc(index+1)))
       index+=2         //skip the next byte because it was used as an argument to construct iconst
      }
     else {
        ByteCodess = ByteCodess ++: Vector(bcf.make(bc(index)))
        index += 1
      }
    }
    ByteCodess
  }



}
