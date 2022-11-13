import scala.collection.mutable.{HashMap, Stack}

object Exp:
    @main def main(args: String*): Unit =
        println(s"  ${evaluate(args)}")

    enum Command:
        case Unary, Binary

    def evaluate(exp: Seq[String]): String =
        var stack = Stack[String]()
        exp foreach {process(stack, _)}
        stack mkString " "

    def process(stack: Stack[String], op: String): Stack[String] =
        var local = stack
        isCommand(op) match
            case Some(Command.Unary) =>
                val a = local.pop.toDouble
                local push {(cmds(op)(a)).toString}
            case Some(Command.Binary) =>
                val b = local.pop.toDouble
                val a = local.pop.toDouble
                local push {(cmds2(op)(a, b)).toString}
            case _ =>
                local push op
        local

    /* unary operators */
    var cmds = HashMap[String, Double => Double]()
    cmds.put("sqrt", (a: Double) => Math sqrt a)
    cmds.put("inv", (a: Double) => 1 / a)

    /* binary operators */
    var cmds2 = HashMap[String, (Double, Double) => Double]()
    cmds2.put("+", (a: Double, b: Double) => a + b)
    cmds2.put("-", (a: Double, b: Double) => a - b)
    cmds2.put("x", (a: Double, b: Double) => a * b)
    cmds2.put("/", (a: Double, b: Double) => a / b)

    def isCommand(op: String): Option[Command] =
        if (cmds contains op) return Some(Command.Unary)
        if (cmds2 contains op) return Some(Command.Binary)
        None