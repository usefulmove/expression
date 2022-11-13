import scala.collection.mutable.{HashMap, Stack}

object Exp:
    @main def main(args: String*): Unit =
        println(s"  ${evaluate(args)}")

    def evaluate(exp: Seq[String]): String =
        var stack = Stack[String]()
        for op <- exp do
            isCmd(op) match
                case true => process(stack, op)
                case _ => stack push op
        stack mkString " "

    def process(stack: Stack[String], op: String): Stack[String] =
        var local = stack
        (cmds contains op) match
            case true => // unary
                val a = local.pop.toDouble
                local push {(cmds(op)(a)).toString}
            case _ => // binary
                val b = local.pop.toDouble
                val a = local.pop.toDouble
                local push {(cmds2(op)(a, b)).toString}
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

    def isCmd(op: String): Boolean =
        (cmds contains op) || (cmds2 contains op)