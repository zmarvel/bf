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

import java.util.logging.{Logger, Level};


object Interpreter {
  def main(args: Array[String]) {
    val logger = Logger.getLogger("com.zackmarvel.bf")
    logger.setLevel(Level.FINE)

    if (args.length < 1) {
      usage
      System.exit(1)
    }
    else {
      def run(state: InterpreterState): InterpreterState = {
        println(state.tapePtr, state.tape(state.tapePtr))
        if (state.hasNext) run(state.next)
        else state
      }

      val inFile = io.Source.fromFile(args(0))
      run(new InterpreterState(tape=inFile.toArray))
    }
  }

  def usage(): Unit = println("USAGE: bf [INPUT FILE]")
}

class InterpreterState(
  val tape: Seq[Char],
  val tapePtr: Int = 0,
  val data: Array[Int] = new Array(64),
  val dataPtr: Int = 0,
  val loops: Map[Int, Int] = Map()
) {

  def next(): InterpreterState = {
    tape(tapePtr) match {
      case '>' => incDataPtr
      case '<' => decDataPtr
      case '+' => incData
      case '-' => decData
      case '.' => printData
      case ',' => readData
      case '[' => beginLoop
      case ']' => endLoop
      case _ => this
    }
  }

  def hasNext(): Boolean = tapePtr + 1 < tape.length

  private def incTapePtr: InterpreterState = {
    new InterpreterState(tape, tapePtr + 1, data, dataPtr, loops)
  }

  private def incDataPtr: InterpreterState = {
    if (dataPtr >= data.length) {
      val newData: Array[Int] = new Array(data.length)
      new InterpreterState(tape, tapePtr + 1, data ++ newData, dataPtr + 1, loops)
    }
    else {
      new InterpreterState(tape, tapePtr + 1, data, dataPtr + 1, loops)
    }
  }

  private def decDataPtr: InterpreterState = {
    new InterpreterState(tape, tapePtr + 1, data, dataPtr - 1, loops)
  }

  private def incData: InterpreterState = {
    data(dataPtr) = data(dataPtr) + 1
    new InterpreterState(tape, tapePtr + 1, data, dataPtr, loops)
  }

  private def decData: InterpreterState = {
    data(dataPtr) = data(dataPtr) - 1
    new InterpreterState(tape, tapePtr + 1, data, dataPtr, loops)
  }

  private def printData: InterpreterState = {
    print(data(dataPtr))
    new InterpreterState(tape, tapePtr + 1, data, dataPtr, loops)
  }

  private def readData: InterpreterState = {
    data(dataPtr) = Console.in.read()
    new InterpreterState(tape, tapePtr + 1, data, dataPtr, loops)
  }

  private def beginLoop: InterpreterState = {
    def findClosingBracket(idx: Int = tapePtr, depth: Int = 0): Int = {
      tape(idx) match {
        case ']' if depth == 0 => idx
        case '[' => findClosingBracket(idx + 1, depth + 1)
        case ']' => findClosingBracket(idx + 1, depth - 1)
        case _ => findClosingBracket(idx + 1, depth)
      }
    }

    val (begin, end) = (tapePtr, findClosingBracket())

    if (data(dataPtr) == 0) 
      new InterpreterState(tape, end + 1, data, dataPtr, loops)
    else
      new InterpreterState(tape, tapePtr + 1, data, dataPtr, loops + (end -> begin))
  }

  private def endLoop: InterpreterState = {
    if (data(dataPtr) == 0) {
      new InterpreterState(tape, tapePtr + 1, data, dataPtr, loops)
    }
    else {
      val (begin, end) = (tapePtr, loops(tapePtr))
      new InterpreterState(tape, begin, data, dataPtr, loops)
    }
  }
}

class Interpreter(
  val tape: Array[Char],
  val tapePtr: Int = 0,
  val data: Array[Int] = new Array(64),
  val dataPtr: Int = 0,
  val loops: Map[Int, Int] = Map()
) {

  def dispatch(): Interpreter = {
    if (tapePtr < tape.length) {
      (tape(tapePtr) match {
        case '>' => incDataPtr
        case '<' => decDataPtr
        case '+' => incData
        case '-' => decData
        case '.' => printData
        case ',' => readData
        case '[' => beginLoop
        case ']' => endLoop
      }).dispatch
    }
    else {
      System.exit(0)
      this
    }
  }

  private def incDataPtr: Interpreter = {
    if (dataPtr >= data.length) {
      val newData: Array[Int] = new Array(data.length)
      new Interpreter(tape, tapePtr + 1, data ++ newData, dataPtr + 1, loops)
    }
    else {
      new Interpreter(tape, tapePtr + 1, data, dataPtr + 1, loops)
    }
  }

  private def decDataPtr: Interpreter = {
    new Interpreter(tape, tapePtr + 1, data, dataPtr - 1, loops)
  }

  private def incData: Interpreter = {
    data(dataPtr) = data(dataPtr) + 1
    new Interpreter(tape, tapePtr + 1, data, dataPtr, loops)
  }

  private def decData: Interpreter = {
    data(dataPtr) = data(dataPtr) - 1
    new Interpreter(tape, tapePtr + 1, data, dataPtr, loops)
  }

  private def printData: Interpreter = {
    print(data(dataPtr))
    new Interpreter(tape, tapePtr + 1, data, dataPtr, loops)
  }

  private def readData: Interpreter = {
    data(dataPtr) = Console.in.read()
    new Interpreter(tape, tapePtr + 1, data, dataPtr, loops)
  }

  private def beginLoop: Interpreter = {
    /*
    def sliceLoop(
      parentTape: Seq[Char] = tape,
      depth: Int = 0,
      tapeSlice: Seq[Char] = Nil): (Array[Char], Array[Char]) {
        val c = parentTape.head
        c match {
          case '[' => findClosingBracket(parentTape.tail, depth + 1, c :+ tapeSlice)
          case ']' if depth == 0 => (c :+ tapeSlice, parentTape)
          case ']' => findClosingBracket(parentTape.tail, depth - 1, c :+ tapeSlice)
        }
      }

      val (loopTape, restTape) = sliceLoop
      */

     /* The below approach will not work as it stands because 1) the left
      * bracket the slice begins with will cause `beginLoop` to run
      * infinitely, and 2) what do we do when we end the loop, since we have
      * already discarded the rest of the tape? The instruction pointer may
      * be unavoidable?
      *
      * Another approach would be using a while loop, but we would need a
      * mutable data pointer.
      */

     /*
      * The solution I'm going to play with is what I did initially: although
      * it's handy to forget about the instruction pointer and just advance
      * the code, we'll just advance the instruction pointer and keep the
      * whole tape during execution. Sadly, this means we can't just use
      * `foldLeft`.
      */

     def findClosingBracket(idx: Int = tapePtr, depth: Int = 0): Int = {
       tape(idx) match {
         case ']' if depth == 0 => idx
         case '[' => findClosingBracket(idx + 1, depth + 1)
         case ']' => findClosingBracket(idx + 1, depth - 1)
         case _ => findClosingBracket(idx + 1, depth)
       }
     }

     val (begin, end) = (tapePtr, findClosingBracket())

     if (data(dataPtr) == 0) 
       new Interpreter(tape, end + 1, data, dataPtr, loops)
     else
       new Interpreter(tape, tapePtr + 1, data, dataPtr, loops + (end -> begin))
  }

  private def endLoop: Interpreter = {
    if (data(dataPtr) == 0) {
      new Interpreter(tape, tapePtr + 1, data, dataPtr, loops)
    }
    else {
      val (begin, end) = (tapePtr, loops(tapePtr))
      new Interpreter(tape, begin, data, dataPtr, loops)
    }
  }
}

class BFRuntimeException extends Exception {
  
}
