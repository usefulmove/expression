import scala.collection.mutable.{HashMap, HashSet}

object Expressions:
    @main def exp(args: String*): Unit =
        println(s"${ formatOutput(evaluateOps(args, List())) }")

    val exp_version = "0.1.0d"
    val delim = " "

    enum Command:
        case Binary, Display, General, Unary, Variable

    object Command:
        def isCommand(op: String): Option[Command] = op match
            case op if cmds contains op => Some(Command.General)
            case op if cmds_unary contains op => Some(Command.Unary)
            case op if cmds_binary contains op => Some(Command.Binary)
            case op if mem contains op => Some(Command.Variable)
            case _ => None

        /**
         * unary operators (double -> double)
         */
        val cmds_unary = HashMap[String, Double => Double]()
        cmds_unary.put("!", a => ((1 to a.toInt) foldLeft 1.0) {_ * _.toDouble})
        cmds_unary.put("abs", _.abs)
        cmds_unary.put("chs", -_)
        cmds_unary.put("inv", 1 / _)
        cmds_unary.put("sqrt", Math sqrt _)

        /**
         * binary operators (double, double -> double)
         */
        val cmds_binary = HashMap[String, (Double, Double) => Double]()
        cmds_binary.put("+", _ + _)
        cmds_binary.put("-", _ - _)
        cmds_binary.put("x", _ * _)
        cmds_binary.put("/", _ / _)
        cmds_binary.put("max", _.max(_))
        cmds_binary.put("min", _.min(_))
        cmds_binary.put("%", _ % _)

        /**
         * general (stack manipulation)
         */
        val cmds = HashMap[String, List[String] => List[String]]()
        cmds.put("cls", st => Nil)
        cmds.put("count", st => st.length.toString :: st)
        cmds.put("dup", st => st.head :: st)
        cmds.put("drop", _.tail)
        cmds.put("dropn", st => st.drop(st.head.toInt + 1))
        cmds.put("e", Math.E.toString :: _)
        cmds.put("red", st =>
            var out_st = st
            for _ <- st.indices.tail do
                out_st = (evaluateOps(lambda, out_st) split delim).toList
            out_st
        )
        cmds.put("rev", st => st.reverse)
        cmds.put("roll", st => st.tail :+ st.head)
        cmds.put("rolln", st =>
            val n = st.head.toInt
            var out_st = st.tail
            for _ <- 1 to n do
                out_st = out_st.tail :+ out_st.head
            out_st
        )
        cmds.put("rot", st => (st takeRight 1) ::: (st dropRight 1))
        cmds.put("rotn", st =>
            val n = st.head.toInt
            var out_st = st.tail
            for _ <- 1 to n do
                out_st = (out_st takeRight 1) ::: (out_st dropRight 1)
            out_st
        )
        cmds.put("io", st =>
            (1 to st.head.toInt)
            .reverse
            .map {_.toString}
            .toList ::: st.tail
        )
        cmds.put("map", _ map {op => evaluateOps(lambda, List[String](op))})
        cmds.put("sum", st => List(st.foldLeft(0.0){_ + _.toDouble}.toString))
        cmds.put("pi", Math.PI.toString :: _)
        cmds.put("prod", st => List(st.foldLeft(1.0){_ * _.toDouble}.toString))
        cmds.put("store", st =>
            val name :: value :: rem_st = st : @unchecked
            mem.put(name, value) // store value string in hashmap
            rem_st
        )
        cmds.put("swap", st => st.tail.head :: st.head :: st.tail.tail)
        cmds.put("take", _.take(1))
        cmds.put("taken", st => st.tail.take(st.head.toInt))
        cmds.put("version", exp_version :: _)

        /**
         * special ops
         */
        val cmds_ops = HashSet[String]("[", "]", "_")
        def isSpecialOp(op: String): Boolean = cmds_ops contains op

    var lambda = Seq[String]() // anonymous function (function literal)
    var mem = HashMap[String, String]() // variable memory

    def evaluateOps(ops: Seq[String], st: List[String]): String =
        var recording = false
        val out_st = (ops foldLeft st) {(acc, op) =>
            Command.isSpecialOp(op) match
                case true =>
                    op match
                        case "[" => // start recording anonymous function
                            lambda = Seq()
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

    def processOp(op: String, st: List[String]): List[String] =
        Command.isCommand(op) match
            case Some(Command.General) => Command.cmds(op)(st)
            case Some(Command.Unary) =>
                val a = st.head.toDouble
                Command.cmds_unary(op)(a).toString :: st.tail
            case Some(Command.Binary) =>
                val b :: a :: rest_st = st : @unchecked
                Command.cmds_binary(op)(a.toDouble, b.toDouble).toString :: rest_st
            case Some(Command.Variable) => mem(op) :: st
            case _ => op :: st // add value to stack

    def formatOutput(output: String): String =
        (output split delim)
        .zipWithIndex
        .reverse
        .map {(element, i) =>
            val pref = if i <= 'z' - 'a' then (i + 'a').toChar else "~"
            s"${pref}.  $element"
        } mkString "\n"