import Stack._

type Command = Stack => Stack

val noop = (stack: Stack) => stack

def command(c: Char): Command = c match
  case n if '0' <= n && n <= '9' => swapWithTop(n.toInt)
  case _ => commands(c)

val commands: Map[Char, Command] = Map(
  's' -> { case a -: b -: s => b -: a -: s },                            //swap a and b
  'S' -> { case a -: b -: s => b -: a -: s },                            //swap a and c
  'd' -> topToBottom,                                                    //send top stack to bottom of stack
  'u' -> bottomToTop,                                                    //send bottom stack to top of stack
  'D' -> { case a -: b -: c -: s => c -: b -: a -: s },                  //duplicate a
  'p' -> { case _ -: stack => stack },                                   //discard the top element
  'P' -> { case a -: stack => foldLeft(stack)((s, e) => e -: s)(a) },    //dump top stack's elements onto stack in reverse
  'e' -> (SNil -: _),                                                    //push an empty stack
  'r' -> reverse,                                                        //reverse the stack
  'E' -> (_ -: SNil),                                                    //enclose entire stack in a stack
  'h' -> { case a -: stack => a },                                       //discard all but the top stack
  ' ' -> noop                                                            //no-op
)
