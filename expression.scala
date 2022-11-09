import scala.collection.mutable.{HashMap, Stack}

object Exp:
    @main
    def main(args: String*): Unit =
        def express(exp: Seq[String]): String =
            var stack = Stack[String]()
            exp foreach {op =>
                (cmds contains op) ||
                (cmds2 contains op) match
                    case true =>
                        (cmds contains op) match
                            case true => stack = process(stack, cmds(op))
                            case _ => stack = process2(stack, cmds2(op))
                    case _ => stack push op
            }
            stack.head
        println(express(args))

    def process(stack: Stack[String], f: Double => Double): Stack[String] =
        var local = stack
        val a = local.pop.toDouble
        local push {(f(a)).toString}
        local

    def process2(stack: Stack[String], f: (Double, Double) => Double): Stack[String] =
        var local = stack
        val b = local.pop.toDouble
        val a = local.pop.toDouble
        local push {(f(a, b)).toString}
        local

    /* unary operators */
    val sqrt = (a: Double) => Math sqrt a
    val inv = (a: Double) => 1 / a

    var cmds = HashMap[String, Double => Double]()
    cmds.put("sqrt", sqrt)
    cmds.put("inv", inv)

    /* binary operators */
    val add = (a: Double, b: Double) => a + b
    val sub = (a: Double, b: Double) => a - b
    val mul = (a: Double, b: Double) => a * b
    val div = (a: Double, b: Double) => a / b

    var cmds2 = HashMap[String, (Double, Double) => Double]()
    cmds2.put("+", add)
    cmds2.put("-", sub)
    cmds2.put("x", mul)
    cmds2.put("/", div)