import scala.collection.mutable.HashMap

object Exp:
    @main def main(args: String*): Unit =
        println(s"${formatOutput(evaluate(args))}")

    enum Command:
        case Unary, Binary, Stack

    def evaluate(exp: Seq[String]): String =
        val s = exp.foldLeft(List[String]()) {process(_, _)}
        s mkString " "

    def process(s: List[String], op: String): List[String] =
        isCommand(op) match
            case Some(Command.Unary) =>
                val a = s(0).toDouble
                cmds_unary(op)(a).toString :: s.tail
            case Some(Command.Binary) =>
                val b = s(0).toDouble
                val a = s(1).toDouble
                cmds_binary(op)(a, b).toString :: s.slice(2, s.length)
            case Some(Command.Stack) => cmds_stack(op)(s)
            case _ => op :: s // add value to stack

    /* unary operators */
    val cmds_unary = HashMap[String, Double => Double]()
    cmds_unary.put("sqrt", (a: Double) => Math sqrt a)
    cmds_unary.put("inv", (a: Double) => 1 / a)

    /* binary operators */
    val cmds_binary = HashMap[String, (Double, Double) => Double]()
    cmds_binary.put("+", (a: Double, b: Double) => a + b)
    cmds_binary.put("-", (a: Double, b: Double) => a - b)
    cmds_binary.put("x", (a: Double, b: Double) => a * b)
    cmds_binary.put("/", (a: Double, b: Double) => a / b)

    /* stack manipulation */
    val cmds_stack = HashMap[String, List[String] => List[String]]()
    cmds_stack.put("dup", (s: List[String]) => {s(0) :: s})
    cmds_stack.put("sum", (s: List[String]) => List(s.foldLeft(0.0){_+_.toDouble}.toString))
    cmds_stack.put("prod", (s: List[String]) => List(s.foldLeft(1.0){_*_.toDouble}.toString))
    cmds_stack.put("io", (s: List[String]) => {
        (1 to s(0).toInt)
        .reverse
        .map {_.toString}
        .toList ::: s.tail
    })

    def isCommand(op: String): Option[Command] = op match
        case op if cmds_unary contains op => Some(Command.Unary)
        case op if cmds_binary contains op => Some(Command.Binary)
        case op if cmds_stack contains op => Some(Command.Stack)
        case _ => None

    def formatOutput(out: String): String = (out split " ").reverse mkString "\n"