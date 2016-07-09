package com.zackmarvel.bf

/**
 * From Wikipedia:
 *
 * >    increment the data pointer (to point to the next cell to the right).
 * <    decrement the data pointer (to point to the next cell to the left).
 * +    increment (increase by one) the byte at the data pointer.
 * -    decrement (decrease by one) the byte at the data pointer.
 * .    output the byte at the data pointer.
 * ,    accept one byte of input, storing its value in the byte at the data
 *        pointer.
 * [    if the byte at the data pointer is zero, then instead of moving the
 *        instruction pointer forward to the next command, jump it forward to the command after the matching ] command.
 * ]    if the byte at the data pointer is nonzero, then instead of moving the
 *        instruction pointer forward to the next command, jump it back to the command after the matching [ command.
 */

class InterpreterState(last: Option[InterpreterState] = None) {
  /*
   * val instructionPtr: Int = 0,
   * val dataPtr: Int = 0,
   * val data: Array[Int] = new Array(64)
   */

  val (instructionPtr, dataPtr) = last match {
    case Some(state) => (state.instructionPtr, state.dataPtr)
    case None => (0, 0)
  }
  val data: Array[Int] = last match {
    case Some(state) => state.dataPtr
    case None => new Array(64)
  }
  val bracketDepth = 0
  val looping = True


  def incDataPtr(): InterpreterState = {
    if (data.length <= dataPtr) {
      new InterpreterState(instructionPtr + 1, dataPtr + 1, data)
    }
    else {
      val moreData: Array[Int] = new Array(data.length)
      new InterpreterState(instructionPtr + 1, dataPtr + 1, data ++ moreData)
    }
  }

  def decDataPtr(): InterpreterState =
    new InterpreterState(instructionPtr + 1, dataPtr - 1, data)

  def incData(): InterpreterState = {
    data(dataPtr) = data(dataPtr) + 1
    this
  }

  def decData(): InterpreterState = {
    data(dataPtr) = data(dataPtr) - 1
    this
  }

  def printData(): InterpreterState = {
    print(data(dataPtr))
    this
  }

  def readData(): InterpreterState = {
    data(dataPtr) = Console.in.read()
    this
  }

}

object BFInterpreter(val state: InterpreterState = new InterpreterState) {

  private val commands = Map(
    ">" => () => 
      "<" => state.decrementDataPtr(),
      );

  def interpret(tape: List[Char]): Int = {
  }

}
