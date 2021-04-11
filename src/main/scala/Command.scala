import Stack._

type Command = Stack => Stack
type PartialCommand = PartialFunction[Stack, Stack]

val noop = (stack: Stack) => stack

def command(c: Char): Command = c match
  case n if '0' <= n && n <= '9' => swapWithTop(n.toInt)
  case _ => allCommands(c)

private val fullCommands: Map[Char, Command] = Map(
  'd' -> topToBottom,                                                    //send top stack to bottom of stack
  'E' -> (_ -: SNil),                                                    //enclose entire stack in a stack
  'e' -> (SNil -: _),                                                    //push an empty stack
  'r' -> reverse,                                                        //reverse the stack
  'u' -> bottomToTop,                                                    //send bottom stack to top of stack
  ' ' -> noop                                                            //no-op
)

private val partialCommands: Map[Char, Command] = Map[Char, PartialCommand](
  'D' -> { case a -: b -: c -: s => c -: b -: a -: s },                  //duplicate a
  'h' -> { case a -: stack => a },                                       //discard all but the top stack
  'm' -> { case (a -: s) -: b -: t => s -: (a -: b) -: t },              //move top stack's top stack to second stack
  'P' -> { case a -: stack => foldLeft(stack)((s, e) => e -: s)(a) },    //dump top stack's elements onto stack in reverse
  'p' -> { case _ -: s => s },                                           //discard the top element
  '-' -> { case a -: b -: s => stackDiff(a, b) -: s },                   //find the difference between top 2 stacks
  'M' -> { case a -: b -: s => maxByLen(a, b) -: s },                    //keep the longer stack of the top 2 stacks
  'c' -> { case a -: b -: s => concat(a, b) -: s },                      //concatenate top 2 stacks
).map:
  case name -> command => name -> completePartialCmd(command, name.toString)

val allCommands: Map[Char, Command] = fullCommands ++ partialCommands

def completePartialCmd(partialCmd: PartialCommand, name: String) = partialCmd.orElse:
  case stack => throw IllegalArgumentException(s"Stack is of incorrect shape for command '$name': $stack")