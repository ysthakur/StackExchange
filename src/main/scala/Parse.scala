import Stack._

import annotation.tailrec

/**
 * Parse a stack literal in the form of curly braces containing other stack literals,
 * separated by nothing, e.g. {{}{{{}}{}}}
 */
def parseStackLiteral(s: String): Stack =
  def helper(startInd: Int): (Stack, Int) =
    val startChar = s.charAt(startInd)
    if startChar == ' ' then return helper(startInd + 1)
    if startChar != '{' then throw new Error("Invalid stack literal")
    var stack: Stack = SNil
    println(s"startInd=$startInd, stack=$stack")
    var currInd = startInd + 1
    while s.charAt(currInd) != '}' do
      val (nextElem, nextInd) = helper(currInd)
      stack -:= nextElem
      currInd = nextInd
      println(s"ind=$currInd, stack=$stack")
      if currInd == s.size then throw new Error("No closing brace for stack literal")
    
    (stack, currInd + 1)
  
  val (stack, ind) = helper(0)
  println(s"endStack=$stack, ind=$ind, s=$s")
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
        case '[' =>
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
        case '(' =>
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