import Stack._

import annotation.tailrec

def executeProg(program: String, inputs: Seq[String]): Unit =
  //todo add flags to read from input, read from a file, etc.
  val initStack = inputs.foldLeft(SNil: Stack)((stack, input) => parseStackLiteral(input) -: stack)
  val progCmd = programToCommand(program)
  try
    progCmd(initStack)
  catch
    case e: StackOverflowError => throw new Error(
        "Congratulations! You have demonstrated your mastery in Stack Exchange by causing a stack overflow!",
        e
      )

/**
 * Parse a stack literal in the form of curly braces containing other stack literals,
 * separated by nothing, e.g. {{}{{{}}{}}}
 */
def parseStackLiteral(s: String): Stack =
  def helper(startInd: Int): (Stack, Int) =
    if s.charAt(startInd) != '{' then
      throw new Error(s"Invalid stack literal, found '${s.charAt(startInd)}', expected '{'")

    var stack: Stack = SNil
    var currInd = startInd + 1
    while s.charAt(currInd) != '}' do
      //Skip spaces within stack literals
      if s.charAt(currInd) == ' ' then
        currInd += 1
      else
        val (nextElem, nextInd) = helper(currInd)
        stack -:= nextElem
        currInd = nextInd
        if currInd == s.size then throw new Error("No closing brace for stack literal")
    
    (stack, currInd + 1)
  
  val (stack, ind) = helper(0)
  if ind != s.size then throw new Error(s"Extra characters in input $s at index $ind")
  
  stack

/**
 * Convert a program (in the form of a string) to a single command
 */
def programToCommand(prog: String): Command =
  def helper(prevCmd: Command, index: Int): (Command, Int) =
    println(s"In helper, ind=$index")
    if index == prog.size then
      (prevCmd, index)
    else
      prog.charAt(index) match
        case '[' =>   //It's a while loop
          var currInd = index + 1
          var cmd = noop
          while prog.charAt(currInd) != ']' do
            val (nextCmd, nextInd) = helper(cmd, currInd)
            cmd = nextCmd
            currInd = nextInd
            if currInd == prog.size then throw new Error("No closing ']' for loop")
          
          @tailrec
          def actualCmd(stack: Stack): Stack =
            stack match
              case SNil | SNil -: _ => stack
              case _ => actualCmd(cmd(stack))

          (prevCmd.andThen(actualCmd), currInd + 1)
        case '(' =>    //Treat the top stack as the main stack
          var currInd = index + 1
          var cmd = noop
          while prog.charAt(currInd) != ')' do
            val (nextCmd, nextInd) = helper(cmd, currInd)
            cmd = nextCmd
            currInd = nextInd
            if currInd == prog.size then throw new Error("No closing ')'")
          
          val actualCmd: Command =
            case top -: rest => cmd(top) -: rest
          
          (prevCmd.andThen(actualCmd), currInd + 1)
        case ')' | ']' => println("got here!!");(prevCmd, index)
        case c =>
          println(s"Character $c}")
          helper(prevCmd.andThen(command(c)), index + 1)
  
  val (progCmd, ind) = helper(noop, 0)
  if ind != prog.size then throw new Error(s"Extra characters in program at index $ind")
  progCmd