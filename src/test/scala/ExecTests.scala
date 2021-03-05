import Stack._

// import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.flatspec.AnyFlatSpec

class ExecTests extends AnyFlatSpec:

  val `programs, inputs, and expected outputs` = List(
    ("e", Seq(), "{ {} }"),
    ("(e)", Seq("{}"), "{ {{}} }"),
    ("(r(r))", Seq("{ { {}{{}} } {} }"), "{ { {} { {{}}{} } } }")
  )

  for (prog, inputs, expected) <- `programs, inputs, and expected outputs` do
    s"Output of $prog with inputs ${inputs.mkString(",")}" should s"be $expected" in {
      println("------------")
      assertResult(parseStackLiteral(expected))(executeProgram(prog, inputs: _*))
    }

  def executeProgram(program: String, inputs: String*): Stack =
    val initStack = inputs.foldLeft(SNil: Stack)((stack, input) => parseStackLiteral(input) -: stack)
    val progCmd = programToCommand(program)
    try
      progCmd(initStack)
    catch
      case e: StackOverflowError =>
        throw new Error(
          "Congratulations! You have demonstrated your mastery in Stack Exchange by causing a stack overflow!",
          e
        )