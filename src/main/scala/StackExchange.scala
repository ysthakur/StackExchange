import Stack._

@main def stackExchange(program: String, inputs: String*): Unit =
  //todo add flags to read from input, read from a file, etc.
  val initStack = inputs.foldLeft(SNil: Stack)((stack, input) => parseStackLiteral(input) -: stack)
  try
    val res = execCommands(program, initStack)
    println(res)
  catch
    case e: StackOverflowError => throw new Error(
        "Congratulations! You have demonstrated your mastery in Stack Exchange by causing a stack overflow!",
        e
      )

def parseStackLiteral(s: String): Stack =
  def helper(startInd: Int): (Stack, Int) =
    if s.charAt(startInd) != '{' then throw new Error("Invalid stack literal")
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
  

def execCommands(prog: String, initStack: Stack): Stack =
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
          
          @annotation.tailrec
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
        case ')' | ']' => (prevCmd, index)
        case c =>
          println(s"Character $c}")
          helper(prevCmd.andThen(command(c)), index + 1)

  val (progCmd, ind) = helper(noop, 0)
  if ind != prog.size then throw new Error(s"Extra characters in program at index $ind")
  progCmd(initStack)