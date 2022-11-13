import scala.collection.mutable.HashMap

object Exp:
    @main def main(args: String*): Unit =
        println(s"${formatOutput(evaluate(args))}")

    enum Command:
        case Unary, Binary, Stack

    def evaluate(exp: Seq[String]): String =
        var st = List[String]()
        exp foreach {op => st = process(st, op)}
        st mkString " "

    def process(st: List[String], op: String): List[String] =
        isCommand(op) match
            case Some(Command.Unary) =>
                val a = st(0).toDouble
                List({(cmds_unary(op)(a)).toString}) :++ st.tail
            case Some(Command.Binary) =>
                val b = st(0).toDouble
                val a = st(1).toDouble
                List({(cmds_binary(op)(a, b)).toString}) :++ st.slice(2, st.length)
            case Some(Command.Stack) =>
                cmds_stack(op)(st)
            case _ => // add value to stack
                List(op) :++ st

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
    cmds_stack.put("dup", (st: List[String]) => {st :++ List(st(0))})
    cmds_stack.put("sum", (st: List[String]) => List(st.foldLeft(0.0){_+_.toDouble}.toString))
    cmds_stack.put("prod", (st: List[String]) => List(st.foldLeft(1.0){_*_.toDouble}.toString))

    def isCommand(op: String): Option[Command] = op match
        case op if cmds_unary contains op => Some(Command.Unary)
        case op if cmds_binary contains op => Some(Command.Binary)
        case op if cmds_stack contains op => Some(Command.Stack)
        case _ => None

    def formatOutput(out: String): String = (out split " ").reverse mkString "\n"