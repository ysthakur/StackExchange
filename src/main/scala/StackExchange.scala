import Stack._

@main def stackExchange(program: String, inputs: String*): Unit =
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