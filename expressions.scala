import scala.collection.mutable.{HashMap, HashSet}

object Expressions:
    @main def exp(args: String*): Unit =
        println(s"${ formatOutput(evaluateOps(args, List())) }")

    val exp_version = "0.2.0a"
    val delim = " "

    enum Command:
        case General, Memory

    object Command:
        def isCommand(op: String): Option[Command] = op match
            case op if cmds contains op => Some(Command.General)
            case op if mem contains op => Some(Command.Memory)
            case _ => None

        /* support functions */
        def unaryInt(st: List[String])(f: Int => Int): List[String] =
            val a :: rest = st : @unchecked
            f(a.toInt).toString :: rest
        def unaryDouble(st: List[String])(f: Double => Double): List[String] =
            val a :: rest = st : @unchecked
            f(a.toDouble).toString :: rest
        def binaryInt(st: List[String])(f: (Int, Int) => Int): List[String] =
            val b :: a :: rest = st : @unchecked
            f(a.toInt, b.toInt).toString :: rest
        def binaryDouble(st: List[String])(f: (Double, Double) => Double): List[String] =
            val b :: a :: rest = st : @unchecked
            f(a.toDouble, b.toDouble).toString :: rest

        /**
         * command definitions
         */
        val cmds = HashMap[String, List[String] => List[String]]()

        /*** stack manipulation ***/
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
        cmds.put("swap", st => st.tail.head :: st.head :: st.tail.tail)
        cmds.put("take", _ take 1)
        cmds.put("taken", st => st.tail take st.head.toInt)
        /* memory usage */
        cmds.put("store", st =>
            val name :: value :: rem_st = st : @unchecked
            mem.put(name, value) // store value string in hashmap
            rem_st
        )

        /*** maths operations ***/
        cmds.put("+", binaryDouble(_)(_ + _))
        cmds.put("-", binaryDouble(_)(_ - _))
        cmds.put("x", binaryDouble(_)(_ * _))
        cmds.put("/", binaryDouble(_)(_ / _))
        cmds.put("!", unaryDouble(_)(a => ((1 to a.toInt) foldLeft 1.0) {_ * _.toDouble}))
        cmds.put("^", binaryDouble(_)(Math.pow))
        cmds.put("%", binaryDouble(_)(_ % _))
        cmds.put("abs", unaryDouble(_)(Math.abs))
        cmds.put("ceil", unaryDouble(_)(Math.ceil))
        cmds.put("chs", unaryDouble(_)(-_))
        cmds.put("e", Math.E.toString :: _)
        cmds.put("floor", unaryDouble(_)(Math.floor))
        cmds.put("inv", unaryDouble(_)(1 / _))
        cmds.put("max", binaryDouble(_)(Math.max))
        cmds.put("min", binaryDouble(_)(Math.min))
        cmds.put("pi", Math.PI.toString :: _)
        cmds.put("prod", st => List((st foldLeft 1.0){_ * _.toDouble}.toString))
        cmds.put("round", unaryDouble(_)(a => (Math round a).toDouble))
        cmds.put("sqrt", unaryDouble(_)(Math.sqrt))
        cmds.put("sum", st => List((st foldLeft 0.0){_ + _.toDouble}.toString))

        /* trigonometric functions */
        cmds.put("sin", unaryDouble(_)(Math.sin))
        cmds.put("cos", unaryDouble(_)(Math.cos))
        cmds.put("tan", unaryDouble(_)(Math.tan))
        cmds.put("asin", unaryDouble(_)(Math.asin))
        cmds.put("acos", unaryDouble(_)(Math.acos))
        cmds.put("atan", unaryDouble(_)(Math.atan))
        /* logarithmic functions */
        cmds.put("ln", unaryDouble(_)(Math.log))
        cmds.put("log", unaryDouble(_)(Math.log10))
        cmds.put("log2", unaryDouble(_)(a => (Math log10 a) / (Math log10 2)))
        cmds.put("logn", binaryDouble(_)((a, b) => (Math log10 a) / (Math log10 b)))

        /*** control flow (?) ***/

        /*** conversion functions ***/
        cmds.put("rad_deg", unaryDouble(_)(Math.toDegrees))
        cmds.put("deg_rad", unaryDouble(_)(Math.toRadians))

        /*** bit operations ***/
        cmds.put("and", binaryInt(_)(_ & _))
        cmds.put("not", unaryInt(_)(~_))
        cmds.put("or", binaryInt(_)(_ | _))
        cmds.put("xor", binaryInt(_)(_ ^ _))

        /*** RGB colors (?) ***/

        /*** higher order functions ***/
        cmds.put("map", _ map {op => evaluateOps(lambda, List[String](op))})
        cmds.put("red", st =>
            var out_st = st
            for _ <- st.indices.tail do
                out_st = (evaluateOps(lambda, out_st) split delim).toList
            out_st
        )

        /*** output ***/
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
            !Command.isSpecialOp(op) match
                case true => // general case
                    !recording match
                        case true => processOp(op, acc)
                        case _ => // recording
                            lambda = lambda :+ op // append op to stored anonymous function
                            acc
                case _ => // special op
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
                        case _ => throw new Exception("unknown special op: " + op)
        }
        out_st mkString delim

    def processOp(op: String, st: List[String]): List[String] =
        Command.isCommand(op) match
            case Some(Command.General) => Command.cmds(op)(st)
            case Some(Command.Memory) => mem(op) :: st
            case _ => op :: st // add value to stack

    def formatOutput(output: String): String =
        (output split delim)
        .zipWithIndex
        .reverse
        .map {(element, i) =>
            val pref = if i <= 'z' - 'a' then (i + 'a').toChar else "~"
            s"${pref}.  $element"
        } mkString "\n"