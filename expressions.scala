import scala.collection.mutable.{HashMap, Stack}

object Exp:
    @main def main(args: String*): Unit =
        println(s"${formatOutput(evaluate(args))}")

    enum Command:
        case Unary, Binary, Stack

    def evaluate(exp: Seq[String]): String =
        var stack = Stack[String]()
        exp foreach {process(stack, _)}
        stack mkString " "

    def process(stack: Stack[String], op: String): Stack[String] =
        var local = stack
        isCommand(op) match
            case Some(Command.Unary) =>
                val a = local.pop.toDouble
                local push {(cmds_unary(op)(a)).toString}
            case Some(Command.Binary) =>
                val b = local.pop.toDouble
                val a = local.pop.toDouble
                local push {(cmds_binary(op)(a, b)).toString}
            case Some(Command.Stack) =>
                val a = local.pop
                local push a
                local push a
            case _ =>
                local push op
        local

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
    var cmds_stack = HashMap[String, Stack[String] => Stack[String]]()
    cmds_stack.put("dup", (stack: Stack[String]) => stack.push(stack.top))

    def isCommand(op: String): Option[Command] =
        if (cmds_unary contains op) return Some(Command.Unary)
        if (cmds_binary contains op) return Some(Command.Binary)
        if (cmds_stack contains op) return Some(Command.Stack)
        None

    def formatOutput(out: String): String =
        (out split " ").reverse mkString "\n"