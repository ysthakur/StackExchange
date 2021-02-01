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

val commands: Map[String, Stack => Stack] = Map(
  "s" -> { case a -: b -: s => b -: a -: s },                            //swap a and b
  "S" -> { case a -: b -: s => b -: a -: s },                            //swap a and c
  "d" -> topToBottom,                                                    //send top stack to bottom of stack
  "u" -> bottomToTop,                                                    //send bottom stack to top of stack
  "d" -> { case a -: b -: c -: s => c -: b -: a -: s },                  //duplicate a
  "D" -> { case _ -: stack => stack },                                   //discard the top element
  "e" -> (SNil -: _),                                                    //push an empty stack
  "r" -> reverse,                                                        //reverse the stack
  "E" -> (_ -: SNil),                                                    //enclose entire stack in a stack
  " " -> Predef.identity,                                                 //no-op
  ")" -> Predef.identity,
  "(" -> Predef.identity
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
  def helper(mainStack: Stack, index: Int): (Stack, Int) =
    println(s"In helper, ind=$index, mainStack=$mainStack")
    if index == prog.size then (mainStack, index)
    prog.charAt(index) match
      case '[' =>
        var currInd = index + 1
        var stack = mainStack
        while prog.charAt(currInd) != ']' do
          val (nextStack, nextInd) = helper(stack, currInd)
          stack = nextStack
          currInd = nextInd
          if currInd == prog.size then throw new Error("No closing ']' for loop")
        (stack, currInd + 1)
      case '(' =>
        var currInd = index + 1
        var top -: rest = mainStack
        while prog.charAt(currInd) != ')' do
          val (nextStack, nextInd) = helper(top, currInd)
          top = nextStack
          currInd = nextInd
          if currInd == prog.size then throw new Error("No closing ')'")
        (top -: rest, currInd + 1)
      case ')' | ']' => (mainStack, index)
      case c =>
        println(s"Character $c, after transformation=${commands(c.toString)(mainStack)}")
        helper(commands(c.toString)(mainStack), index + 1)
  val (res, ind) = helper(initStack, 0)
  if ind != prog.size then throw new Error(s"Extra characters in program at index $ind")
  res