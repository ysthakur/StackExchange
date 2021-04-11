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
    case e: MatchError => throw new Error(
      "Stack has invalid shape",
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
          var loopCmd = noop
          while prog.charAt(currInd) != ']' do
            val (nextCmd, nextInd) = helper(loopCmd, currInd)
            loopCmd = nextCmd
            currInd = nextInd
            if currInd == prog.size then throw Error("No closing ']' for loop")
          
          @tailrec
          def actualCmd(stack: Stack): Stack =
            stack match
              case SNil | SNil -: _ => stack
              case _ => actualCmd(loopCmd(stack))

          (prevCmd.andThen(actualCmd), currInd + 1)
        case '(' =>    //Treat the top stack as the main stack
          var currInd = index + 1
          var cmd = noop
          while prog.charAt(currInd) != ')' do
            val (nextCmd, nextInd) = helper(cmd, currInd)
            cmd = nextCmd
            currInd = nextInd
            if currInd == prog.size then throw Error("No closing ')'")
          
          val actualCmd: PartialCommand =
            case top -: rest => cmd(top) -: rest
          
          (prevCmd.andThen(completePartialCmd(actualCmd, "Run on top stack")), currInd + 1)
        case '{' =>   //It's a try-catch block
          var currInd = index + 1
          var tryCmd = noop
          while prog.charAt(currInd) != '|' do
            val (nextCmd, nextInd) = helper(tryCmd, currInd)
            tryCmd = nextCmd
            currInd = nextInd
            if currInd == prog.size then throw Error("No  '|' for try-catch block")
          
          var catchCmd = noop
          while prog.charAt(currInd) != '}' do
            val (nextCmd, nextInd) = helper(catchCmd, currInd)
            catchCmd = nextCmd
            currInd = nextInd
            if currInd == prog.size then throw Error("No closing '}' for try-catch block")
          
          val actualCmd: Command = stack =>
            try tryCmd(stack)
            catch case _ => catchCmd(stack)

          (actualCmd, currInd + 1)
        case ')' | ']' | '}' | '|' => (prevCmd, index)
        case c =>
          // println(s"Character $c}")
          helper(prevCmd.andThen(command(c)), index + 1)
  
  val (progCmd, ind) = helper(noop, 0)
  if ind != prog.size then println(s"Warning: Extra characters in program at index $ind")
  progCmd