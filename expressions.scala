import scala.collection.mutable.{HashMap, HashSet}

object Expressions:
    @main def exp(args: String*): Unit =
        println(s"${ formatOutput(evaluateOps(args, List())) }")

    val exp_version = "0.1.0e"
    val delim = " "

    enum Command:
        case Binary, Const, Display, General, Unary, Variable

    object Command:
        def isCommand(op: String): Option[Command] = op match
            case op if cmds contains op => Some(Command.General)
            case op if cmds_unary contains op => Some(Command.Unary)
            case op if cmds_binary contains op => Some(Command.Binary)
            case op if cmds_const contains op => Some(Command.Const)
            case op if mem contains op => Some(Command.Variable)
            case _ => None

        /* support functions */
        def unary_double(f: Double => Double): String => String =
            (a: String) => f(a.toDouble).toString
        def unary_int(f: Int => Int): String => String =
            (a: String) => f(a.toInt).toString
        def binary_double(f: (Double, Double) => Double): (String, String) => String =
            (a: String, b: String) => f(a.toDouble, b.toDouble).toString
        def binary_int(f: (Int, Int) => Int): (String, String) => String =
            (a: String, b: String) => f(a.toInt, b.toInt).toString

        /**
         * constant functions
         * push constant to top of stack
         */
        val cmds_const = HashMap[String, String]()
        cmds_const.put("pi", Math.PI.toString)
        cmds_const.put("e", Math.E.toString)

        /**
         * unary functions (string => string)
         * remove top element from top of stack and push result
         */
        val cmds_unary = HashMap[String, String => String]()
        cmds_unary.put("!", unary_double {a => ((1 to a.toInt) foldLeft 1.0) {_ * _.toDouble}})
        cmds_unary.put("abs", unary_double {_.abs})
        cmds_unary.put("ceil", unary_double {Math.ceil})
        cmds_unary.put("chs", unary_double {-_})
        cmds_unary.put("floor", unary_double {Math.floor})
        cmds_unary.put("inv", unary_double {1 / _})
        cmds_unary.put("sqrt", unary_double {Math.sqrt})
        cmds_unary.put("round", unary_double {a => (Math round a).toDouble})
        /* trigonometric functions */
        cmds_unary.put("sin", unary_double {Math.sin})
        cmds_unary.put("cos", unary_double {Math.cos})
        cmds_unary.put("tan", unary_double {Math.tan})
        cmds_unary.put("asin", unary_double {Math.asin})
        cmds_unary.put("acos", unary_double {Math.acos})
        cmds_unary.put("atan", unary_double {Math.atan})
        /* logarithmic functions */
        cmds_unary.put("ln", unary_double {Math.log})
        cmds_unary.put("log2", unary_double {a => (Math log10 a) / (Math log10 2)})
        cmds_unary.put("log", unary_double {Math.log10})
        /* conversion functions */
        cmds_unary.put("rad_deg", unary_double {Math.toDegrees})
        cmds_unary.put("deg_rad", unary_double {Math.toRadians})
        /* bitwise operations */
        cmds_unary.put("not", unary_int {~_})

        /**
         * binary functions (string, string => string)
         * remove two elements from top of stack and push result
         */
        val cmds_binary = HashMap[String, (String, String) => String]()
        cmds_binary.put("+", binary_double {_ + _})
        cmds_binary.put("-", binary_double {_ - _})
        cmds_binary.put("x", binary_double {_ * _})
        cmds_binary.put("/", binary_double {_ / _})
        cmds_binary.put("^", binary_double {Math.pow})
        cmds_binary.put("%", binary_double {_ % _})
        cmds_binary.put("max", binary_double {Math.max})
        cmds_binary.put("min", binary_double {Math.min})
        /* logarithmic functions */
        cmds_binary.put("logn", binary_double {(a, b) => (Math log10 a) / (Math log10 b)})
        /* bitwise operations */
        cmds_binary.put("and", binary_int {_ & _})
        cmds_binary.put("or", binary_int {_ | _})
        cmds_binary.put("xor", binary_int {_ ^ _})

        /**
         * general (stack manipulation)
         */
        val cmds = HashMap[String, List[String] => List[String]]()
        cmds.put("cls", st => Nil)
        cmds.put("count", st => st.length.toString :: st)
        cmds.put("dup", st => st.head :: st)
        cmds.put("drop", _.tail)
        cmds.put("dropn", st => st drop {st.head.toInt + 1})
        cmds.put("io", st =>
            (1 to st.head.toInt)
            .reverse
            .map {_.toString}
            .toList ::: st.tail
        )
        cmds.put("map", _ map {op => evaluateOps(lambda, List[String](op))})
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
        cmds.put("sum", st => List((st foldLeft 0.0){_ + _.toDouble}.toString))
        cmds.put("prod", st => List((st foldLeft 1.0){_ * _.toDouble}.toString))
        cmds.put("store", st =>
            val name :: value :: rem_st = st : @unchecked
            mem.put(name, value) // store value string in hashmap
            rem_st
        )
        cmds.put("swap", st => st.tail.head :: st.head :: st.tail.tail)
        cmds.put("take", _.take(1))
        cmds.put("taken", st => st.tail take st.head.toInt)
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
                val a = st.head
                Command.cmds_unary(op)(a) :: st.tail
            case Some(Command.Binary) =>
                val b :: a :: rest_st = st : @unchecked
                Command.cmds_binary(op)(a, b) :: rest_st
            case Some(Command.Const) => (Command cmds_const op) :: st
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