package vendor


import scala.io.Source

/**
 * Created by user on 10/19/2015.
 */
object VendorProgramParser extends ProgramParser{

  /**
   * Parses a file representation of a bytecode program
   * into an `InstructionList`.
   *
   * @param file the file to parse
   * @return an instruction list
   */

  def parse(file: String): InstructionList = {
    var instructions = Vector[Instruction]()
    val fileName = file
    try{
      for(line <- Source.fromFile(fileName).getLines) {
       instructions = instructions ++: parseString(line)
      }
      instructions
    }
    catch {
      case ex: InvalidInstructionFormatException => println("Please enter a valid file")
        instructions
    }
  }


  /**
   * Parses a string representation of a bytecode program
   * into an `InstructionList`.
   *
   * @param string the string to parse
   * @return an instruction list
   */
  def parseString(string: String): InstructionList = {
    var instructions : InstructionList = Vector[Instruction]()
    val lines = string.split("\n")
    for(i <- 0 until lines.length){
    val words = lines(i).split(" ")
    val name = words (0)
    if(words.length > 1) {          //if words have more than 1 entry then it must be iconst
      val args: Vector[Int] = Vector(words(1).toInt)   //grabs the int argument for iconst
      val inst = new Instruction(name, args)
      instructions = instructions ++: Vector(inst)
    }
    else {                //cases for when the byte command is not iconst and does not take parameters
      val inst = new Instruction(name, Vector())
      instructions = instructions ++: Vector(inst)
    }
    }
    instructions
  }

}
