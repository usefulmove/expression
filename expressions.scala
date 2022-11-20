import scala.collection.mutable.{HashMap, HashSet}

object Expressions:
    @main def exp(args: String*): Unit =
        val output = formatOutput(evaluateOps(args, Nil))
        if !output.isEmpty then println(s"$output")

    val exp_version = "0.2.0a"
    val delim = " "

    enum Command:
        case Standard, Memory

    object Command:
        def isCommand(op: String): Option[Command] = op match
            case op if cmds contains op => Some(Command.Standard)
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
         *  command definitions
         */
        val cmds = HashMap[String, List[String] => List[String]]()

        /*** stack manipulation ***/
        cmds.put("cls", st => Nil)
        cmds.put("count", st => st.length.toString :: st)
        cmds.put("dup", st => st.head :: st)
        cmds.put("drop", _.tail)
        cmds.put("dropn", st => st drop {st.head.toInt + 1})
        cmds.put("rev", st => st.reverse)
        cmds.put("roll", st => st.tail :+ st.head)
        cmds.put("rolln", st =>
            val a :: rest = st : @unchecked
            var out_st = rest
            for _ <- 1 to a.toInt do
                out_st = out_st.tail :+ out_st.head
            out_st
        )
        cmds.put("rot", st => (st takeRight 1) ::: (st dropRight 1))
        cmds.put("rotn", st =>
            val a :: rest = st : @unchecked
            var out_st = rest
            for _ <- 1 to a.toInt do
                out_st = (out_st takeRight 1) ::: (out_st dropRight 1)
            out_st
        )
        cmds.put("swap", st =>
            val b :: a :: rest = st : @unchecked
            a :: b :: rest
        )
        cmds.put("take", _ take 1)
        cmds.put("taken", st =>
            val a :: rest = st : @unchecked
            rest take a.toInt
        )
        /* ranges */
        cmds.put("io", st =>
            val a :: rest = st : @unchecked
            (1 to a.toInt)
            .reverse
            .map {_.toString}
            .toList ::: rest
        )
        cmds.put("to", st =>
            val c :: b :: a :: rest = st : @unchecked
            (a.toInt to b.toInt by c.toInt)
            .reverse
            .map {_.toString}
            .toList ::: rest
        )

        /*** memory usage ***/
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
        cmds.put("nroot", binaryDouble(_)((a, b) => Math.pow(a, 1.0 / b)))
        cmds.put("pi", Math.PI.toString :: _)
        cmds.put("prod", st => List((st foldLeft 1.0){_ * _.toDouble}.toString))
        cmds.put("proot", st =>
            val sc :: sb :: sa :: rest = st : @unchecked
            val (a, b, c) = (sa.toDouble, sb.toDouble, sc.toDouble)
            val dsc = b * b - 4 * a * c // discriminant
            val out = dsc < 0 match
                case true =>
                    val r1r = -b / (2 * a)
                    val r1i = Math.sqrt(-dsc) / (2 * a)
                    val r2r = r1r
                    val r2i = -r1i
                    List(r2i, r2r, r1i, r1r)
                case _ =>
                    val r1r = (-b + Math.sqrt(dsc)) / (2 * a)
                    val r2r = (-b - Math.sqrt(dsc)) / (2 * a)
                    val (r1i, r2i) = (0.0, 0.0)
                    List(r2i, r2r, r1i, r1r)
            (out map {_.toString}) ::: rest
        )
        cmds.put("round", unaryDouble(_)(a => (Math round a).toDouble))
        cmds.put("sgn", unaryDouble(_)(_.sign))
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
        cmds.put("dec_bin", st =>
            val a :: rest = st : @unchecked
            a.toInt.toBinaryString :: rest
        )
        cmds.put("bin_dec", st =>
            val a :: rest = st : @unchecked
            Integer.parseInt(a, 2).toString :: rest
        )

        /*** bit operations ***/
        cmds.put("and", binaryInt(_)(_ & _))
        cmds.put("nand", binaryInt(_)((a, b) => ~(a & b)))
        cmds.put("not", unaryInt(_)(~_))
        cmds.put("ones", unaryInt(_)(Integer.bitCount))
        cmds.put("or", binaryInt(_)(_ | _))
        cmds.put("nor", binaryInt(_)((a, b) => ~(a | b)))
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
        cmds.put("--", st =>
            println {cmds.keys.toList.sorted.mkString(" ")}
            st
        )
        cmds.put("ascii", st =>
            (0 to 255)
            .filterNot {_.toChar.isControl}
            .map {c => s"  '${c.toChar}' ${c.toInt}"}
            .foreach {println}
            st
        )
        cmds.put("version", st =>
            println {s"  expressions ${exp_version}"}
            st
        )

        /**
         *  special ops
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
            case Some(Command.Standard) => Command.cmds(op)(st)
            case Some(Command.Memory) => mem(op) :: st
            case _ => op :: st // add value to stack

    def formatOutput(output: String): String =
        if output.isEmpty then return ""

        (output split delim)
        .zipWithIndex
        .reverse
        .map {(element, i) =>
            val pref = if i <= 'z' - 'a' then (i + 'a').toChar else "~"
            s"${pref}.  $element"
        } mkString "\n"