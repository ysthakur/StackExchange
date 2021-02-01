def main(args: Array[String]): Unit =
  //todo add flags to read from input, read from a file, etc.
  var program = args.last
  var inputs = args.init

  val initStack = inputs.foldLeft(SNil: Stack)((stack, input) => parseStackLiteral(input) -: stack)


sealed trait Stack { def -:(head: Stack) = new -:(head, this) }
case class -:(head: Stack, tail: Stack) extends Stack
object SNil extends Stack

val commands: Map[String, Stack => Stack] = Map(
  "s" -> { case a -: b -: stack => b -: a -: stack }, //swap
  "d" -> { case stack@(a -: _) => a -: stack },       //duplicate
  "e" -> (SNil -: _),                                 //Push an empty stack
  "n" -> Predef.identity                              //no-op
)

def parseStackLiteral(s: String): Stack = ???

def execCommands(prog: String, mainStack: Stack): Stack = ???