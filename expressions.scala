import scala.collection.mutable.HashMap

object Exp:
    @main def main(args: String*): Unit =
        println(s"${formatOutput(evaluate(args))}")

    enum Command:
        case Unary, Binary, Stack

    def evaluate(exp: Seq[String]): String =
        var stack = List[String]()
        exp foreach {op => stack = process(stack, op)}
        stack mkString " "

    def process(stack: List[String], op: String): List[String] =
        isCommand(op) match
            case Some(Command.Unary) =>
                val a = stack(0).toDouble
                List({(cmds_unary(op)(a)).toString}) :++ stack.tail
            case Some(Command.Binary) =>
                val b = stack(0).toDouble
                val a = stack(1).toDouble
                List({(cmds_binary(op)(a, b)).toString}) :++ stack.slice(2, stack.length)
            case Some(Command.Stack) =>
                cmds_stack(op)(stack)
            case _ => // add value to stack
                List(op) :++ stack

    /* unary operators */
    var cmds_unary = HashMap[String, Double => Double]()
    cmds_unary.put("sqrt", (a: Double) => Math sqrt a)
    cmds_unary.put("inv", (a: Double) => 1 / a)

    /* binary operators */
    var cmds_binary = HashMap[String, (Double, Double) => Double]()
    cmds_binary.put("+", (a: Double, b: Double) => a + b)
    cmds_binary.put("-", (a: Double, b: Double) => a - b)
    cmds_binary.put("x", (a: Double, b: Double) => a * b)
    cmds_binary.put("/", (a: Double, b: Double) => a / b)

    /* stack manipulation */
    var cmds_stack = HashMap[String, List[String] => List[String]]()
    cmds_stack.put("dup", (stack: List[String]) => {stack :++ List(stack(0))})
    cmds_stack.put("sum", (stack: List[String]) => List(stack.foldLeft(0.0){_+_.toDouble}.toString))
    cmds_stack.put("prod", (stack: List[String]) => List(stack.foldLeft(1.0){_*_.toDouble}.toString))

    def isCommand(op: String): Option[Command] =
        if (cmds_unary contains op) return Some(Command.Unary)
        if (cmds_binary contains op) return Some(Command.Binary)
        if (cmds_stack contains op) return Some(Command.Stack)
        None

    def formatOutput(out: String): String =
        (out split " ").reverse mkString "\n"