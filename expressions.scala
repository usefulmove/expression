import scala.collection.mutable.{HashMap, HashSet}

object Exp:
    @main def main(args: String*): Unit =
        println(s"${ formatOutput(evaluateOps(args, List[String]())) }")

    enum Command:
        case Unary, Binary, General

    object Command:
        def isCommand(op: String): Option[Command] = op match
            case op if cmds_unary contains op => Some(Command.Unary)
            case op if cmds_binary contains op => Some(Command.Binary)
            case op if cmds contains op => Some(Command.General)
            case _ => None

    val delim = " "
    var lambda = Seq[String]() // anonymous function (function literal)

    def evaluateOps(ops: Seq[String], st: List[String]): String =
        var recording = false
        val out_st = (ops foldLeft st) {(acc, op) =>
            isSpecialOp(op) match
                case true =>
                    op match
                        case "[" => // start recording anonymous function
                            lambda = Seq[String]()
                            recording = true
                            acc
                        case "]" => // stop recording
                            recording = false
                            acc
                        case "_" => // evaluate anonymous function on current stack
                            (evaluateOps(lambda, acc) split delim).toList
                        case _ => ???
                case _ =>
                    recording match
                        case false => processOp(op, acc)
                        case _ =>
                            lambda = lambda :+ op // append op to stored anonymous function
                            acc
        }
        out_st mkString delim

    def processOp(op: String, s: List[String]): List[String] =
        Command.isCommand(op) match
            case Some(Command.Unary) =>
                val a = s(0).toDouble
                cmds_unary(op)(a).toString :: s.tail
            case Some(Command.Binary) =>
                val b = s(0).toDouble
                val a = s(1).toDouble
                cmds_binary(op)(a, b).toString :: s.slice(2, s.length)
            case Some(Command.General) => cmds(op)(s)
            case _ => op :: s // add value to stack

    /* unary operators */
    val cmds_unary = HashMap[String, Double => Double]()
    cmds_unary.put("abs", (a: Double) => a.abs)
    cmds_unary.put("inv", (a: Double) => 1 / a)
    cmds_unary.put("sgn", (a: Double) => a.sign)
    cmds_unary.put("sqrt", (a: Double) => Math sqrt a)

    /* binary operators */
    val cmds_binary = HashMap[String, (Double, Double) => Double]()
    cmds_binary.put("+", (a: Double, b: Double) => a + b)
    cmds_binary.put("-", (a: Double, b: Double) => a - b)
    cmds_binary.put("x", (a: Double, b: Double) => a * b)
    cmds_binary.put("/", (a: Double, b: Double) => a / b)

    /* stack manipulation */
    val cmds = HashMap[String, List[String] => List[String]]()
    cmds.put("dup", (s: List[String]) => s(0) :: s)
    cmds.put("drop", (s: List[String]) => s.tail)
    cmds.put("io", (s: List[String]) =>
        (1 to s(0).toInt)
        .reverse
        .map {_.toString}
        .toList ::: s.tail
    )
    cmds.put("map", (s: List[String]) => s map {op => evaluateOps(lambda, List[String](op))})
    cmds.put("prod", (s: List[String]) => List(s.foldLeft(1.0){_*_.toDouble}.toString))
    cmds.put("sum", (s: List[String]) => List(s.foldLeft(0.0){_+_.toDouble}.toString))
    cmds.put("swap", (s: List[String]) => s(1) :: s(0) :: s.slice(2, s.length))

    /* special ops */
    val cmds_ops = HashSet[String]("[", "]", "_")
    def isSpecialOp(op: String): Boolean = cmds_ops contains op


    def formatOutput(out: String): String =
        (out split delim)
        .zipWithIndex
        .reverse
        .map {case (element, i) => s"${(i + 'a').toChar}.  $element"}
        .mkString("\n")