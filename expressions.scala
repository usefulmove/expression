import scala.collection.mutable.{HashMap, HashSet}

object Expressions:
    @main def exp(args: String*): Unit =
        val output = formatOutput(evaluateOps(args, Nil))
        if !output.isEmpty then println {s"$output"}

    val exp_version = "0.3.0a"
    val delim = " "

    enum Command:
        case Standard, Memory, UserFunction

    object Command:
        def isCommand(op: String): Option[Command] =
            op match
                case op if cmds contains op => Some(Command.Standard)
                case op if mem contains op => Some(Command.Memory)
                case op if usercmds contains op => Some(Command.UserFunction)
                case _ => None

        /* support functions */
        def unaryInt(stck: List[String])(f: Int => Int): List[String] =
            val a :: rest = stck : @unchecked
            f(a.toInt).toString :: rest
        def unaryDouble(stck: List[String])(f: Double => Double): List[String] =
            val a :: rest = stck : @unchecked
            f(a.toDouble).toString :: rest
        def binaryInt(stck: List[String])(f: (Int, Int) => Int): List[String] =
            val b :: a :: rest = stck : @unchecked
            f(a.toInt, b.toInt).toString :: rest
        def binaryDouble(stck: List[String])(f: (Double, Double) => Double): List[String] =
            val b :: a :: rest = stck : @unchecked
            f(a.toDouble, b.toDouble).toString :: rest


        /**
         *  user defined function map
         */
        val usercmds = HashMap[String, Seq[String]]()

        /**
         *  command definitions
         */
        val cmds = HashMap[String, List[String] => List[String]]()

        /*** stack manipulation ***/
        cmds put ("cls", stck => Nil)
        cmds put ("count", stck => stck.length.toString :: stck)
        cmds put ("dup", stck => stck.head :: stck)
        cmds put ("drop", _.tail)
        cmds put ("dropn", stck => stck drop {stck.head.toInt + 1})
        cmds put ("rev", stck => stck.reverse)
        cmds put ("roll", stck => stck.tail :+ stck.head)
        cmds put ("rolln", stck =>
            val a :: rest = stck : @unchecked
            var out_st = rest
            for _ <- 1 to a.toInt do
                out_st = out_st.tail :+ out_st.head
            out_st
        )
        cmds put ("rot", stck => (stck takeRight 1) ::: (stck dropRight 1))
        cmds put ("rotn", stck =>
            val a :: rest = stck : @unchecked
            var out_st = rest
            for _ <- 1 to a.toInt do
                out_st = (out_st takeRight 1) ::: (out_st dropRight 1)
            out_st
        )
        cmds put ("swap", stck =>
            val b :: a :: rest = stck : @unchecked
            a :: b :: rest
        )
        cmds put ("take", _ take 1)
        cmds put ("taken", stck =>
            val a :: rest = stck : @unchecked
            rest take a.toInt
        )
        /* ranges */
        cmds put ("io", stck =>
            val a :: rest = stck : @unchecked
            (1 to a.toInt)
            .reverse
            .map {_.toString}
            .toList ::: rest
        )
        cmds put ("to", stck =>
            val c :: b :: a :: rest = stck : @unchecked
            (a.toInt to b.toInt by c.toInt)
            .reverse
            .map {_.toString}
            .toList ::: rest
        )

        /*** memory usage ***/
        cmds put ("assign", stck =>
            val key :: value :: rem_st = stck : @unchecked
            mem.put(key, value) // assign value to string in hashmap
            rem_st
        )
        cmds put ("store", cmds("assign"))

        /*** maths operations ***/
        cmds put ("+", binaryDouble(_)(_ + _))
        cmds put ("-", binaryDouble(_)(_ - _))
        cmds put ("x", binaryDouble(_)(_ * _))
        cmds put ("/", binaryDouble(_)(_ / _))
        cmds put ("!", unaryDouble(_)(a => ((1 to a.toInt) foldLeft 1.0) {_ * _.toDouble}))
        cmds put ("^", binaryDouble(_)(Math.pow))
        cmds put ("%", binaryDouble(_)(_ % _))
        cmds put ("abs", unaryDouble(_)(Math.abs))
        cmds put ("ceil", unaryDouble(_)(Math.ceil))
        cmds put ("chs", unaryDouble(_)(-_))
        cmds put ("e", Math.E.toString :: _)
        cmds put ("frac", unaryDouble(_)(a => a - Math.floor(a)))
        cmds put ("floor", unaryDouble(_)(Math.floor))
        cmds put ("inv", unaryDouble(_)(1 / _))
        cmds put ("mean", stck => ((stck foldLeft 0.0){_ + _.toDouble} / stck.length).toString +: Nil)
        cmds put ("max", binaryDouble(_)(Math.max))
        cmds put ("min", binaryDouble(_)(Math.min))
        cmds put ("minmax", stck =>
            (stck foldLeft List(Double.MinValue, Double.MaxValue)) {(acc, s) =>
                val n = s.toDouble
                List(n max acc(0), n min acc(1))
            } map {_.toString}
        )
        cmds put ("nroot", binaryDouble(_)((a, b) => Math.pow(a, 1.0 / b)))
        cmds put ("pi", Math.PI.toString :: _)
        cmds put ("prod", stck => List((stck foldLeft 1.0){_ * _.toDouble}.toString))
        cmds put ("proot", stck =>
            val sc :: sb :: sa :: rest = stck : @unchecked
            val (a, b, c) = (sa.toDouble, sb.toDouble, sc.toDouble)
            val dsc = b * b - 4 * a * c // discriminant
            val out = Math signum dsc match
                case -1 =>
                    val r1r = -b / (2 * a)
                    val r1i = (Math sqrt -dsc) / (2 * a)
                    val r2r = r1r
                    val r2i = -r1i
                    List(r2i, r2r, r1i, r1r)
                case _ =>
                    val r1r = (-b + (Math sqrt dsc)) / (2 * a)
                    val r2r = (-b - (Math sqrt dsc)) / (2 * a)
                    val (r1i, r2i) = (0.0, 0.0)
                    List(r2i, r2r, r1i, r1r)
            (out map {_.toString}) ::: rest
        )
        cmds put ("rand", unaryInt(_)(a =>
            val rand = scala.util.Random
            rand.between(1, a + 1)
        ))
        cmds put ("round", unaryDouble(_)(a => (Math round a).toDouble))
        cmds put ("sgn", unaryDouble(_)(_.sign))
        cmds put ("sqrt", unaryDouble(_)(Math.sqrt))
        cmds put ("sum", stck => List((stck foldLeft 0.0){_ + _.toDouble}.toString))

        /* trigonometric functions */
        cmds put ("sin", unaryDouble(_)(Math.sin))
        cmds put ("cos", unaryDouble(_)(Math.cos))
        cmds put ("tan", unaryDouble(_)(Math.tan))
        cmds put ("asin", unaryDouble(_)(Math.asin))
        cmds put ("acos", unaryDouble(_)(Math.acos))
        cmds put ("atan", unaryDouble(_)(Math.atan))
        /* logarithmic functions */
        cmds put ("ln", unaryDouble(_)(Math.log))
        cmds put ("log", unaryDouble(_)(Math.log10))
        cmds put ("log2", unaryDouble(_)(a => (Math log10 a) / (Math log10 2)))
        cmds put ("logn", binaryDouble(_)((a, b) => (Math log10 a) / (Math log10 b)))

        /*** conversion functions ***/
        /* degrees to radians */
        cmds put ("deg_rad", unaryDouble(_)(Math.toRadians))
        cmds put ("rad_deg", unaryDouble(_)(Math.toDegrees))
        /* decimal to binary to hexadecimal */
        cmds put ("dec_bin", stck =>
            val a :: rest = stck : @unchecked
            a.toInt.toBinaryString :: rest
        )
        cmds put ("bin_dec", stck =>
            val a :: rest = stck : @unchecked
            Integer.parseInt(a, 2).toString :: rest
        )
        cmds put ("dec_hex", stck =>
            val a :: rest = stck : @unchecked
            a.toInt.toHexString :: rest
        )
        cmds put ("hex_dec", stck =>
            val a :: rest = stck : @unchecked
            Integer.parseInt(a, 16).toString :: rest
        )
        /* temperature */
        cmds put ("c_f", unaryDouble(_)(_ * 9.0 / 5.0 + 32.0))
        cmds put ("f_c", unaryDouble(_)(a => (a - 32.0) * 5.0 / 9.0))
        /* distances */
        cmds put ("ft_m", unaryDouble(_)(_ * 0.3048))
        cmds put ("m_ft", unaryDouble(_)(_ / 0.3048))

        /*** bit operations ***/
        cmds put ("and", binaryInt(_)(_ & _))
        cmds put ("nand", binaryInt(_)((a, b) => ~(a & b)))
        cmds put ("not", unaryInt(_)(~_))
        cmds put ("ones", unaryInt(_)(Integer.bitCount))
        cmds put ("or", binaryInt(_)(_ | _))
        cmds put ("nor", binaryInt(_)((a, b) => ~(a | b)))
        cmds put ("xor", binaryInt(_)(_ ^ _))

        /*** RGB colors (?) ***/

        /*** higher order functions ***/
        cmds put ("map", _ map {op => evaluateOps(λ, List[String](op))})
        cmds put ("fold", stck =>
            var out_st = stck
            for _ <- stck.indices.tail do
                out_st = (evaluateOps(λ, out_st) split delim).toList
            out_st
        )

        /*** output ***/
        cmds put ("--", stck =>
            println {cmds.keys.toList.sorted mkString " "}
            stck
        )
        cmds put ("ascii", stck =>
            val out = (0 to 255)
                .filterNot {_.toChar.isControl}
                .map {c => s"('${c.toChar}' ${c.toInt})"} // "('é'  233")
                .mkString("   ")
            println {out}

            stck
        )
        cmds put ("version", stck =>
            println {s"  expressions ${exp_version}"}
            stck
        )

        /*** finance ***/
        cmds put ("cmpnd", stck => // future value (compound interest)
            val sc :: sb :: sa :: rest = stck : @unchecked
            val (presentValue, interestRate, numPeriods) = (sa.toDouble, sb.toDouble, sc.toDouble)
            val out = presentValue * Math.pow(1 + interestRate, numPeriods)
            out.toString :: rest
        )
        cmds put ("pmt", stck => // future value (payments)
            val sc :: sb :: sa :: rest = stck : @unchecked
            val (paymentAmount, interestRate, numPeriods) = (sa.toDouble, -sb.toDouble, sc.toDouble)
            val out = paymentAmount * (1 - Math.pow(1 + interestRate, -numPeriods)) / interestRate
            out.toString :: rest
        )

        /**
         *  special operations
         */
        val specialOps = HashSet[String]("[", "]", "_", "fn")
        def isSpecialOp(op: String): Boolean = specialOps contains op

    end Command

    var λ = Seq[String]() // anonymous (lambda) function
    var mem = HashMap[String, String]() // variable memory

    def evaluateOps(ops: Seq[String], stck: List[String]): String =
        var recording = false
        val out_st = (ops foldLeft stck) {(acc, op) =>
            !(Command isSpecialOp op) match
                case true => // general case
                    !recording match
                        case true => processOp(op, acc)
                        case _ => // recording
                            λ = λ :+ op // append op to stored anonymous function
                            acc
                case _ => op match // special op
                    case "fn" => // add user defined function
                        val name :: rest = acc : @unchecked
                        Command.usercmds put (name, λ)
                        rest
                    case "[" => // start recording anonymous function
                        λ = Seq()
                        recording = true
                        acc
                    case "]" => // stop recording
                        recording = false
                        acc
                    case "_" => (evaluateOps(λ, acc) split delim).toList // evaluate anonymous function
                    case _ => throw new Exception("unknown special op: " + op)
        }
        out_st mkString delim

    def processOp(op: String, stck: List[String]): List[String] =
        (Command isCommand op) match
            case Some(Command.Standard) => Command.cmds(op)(stck)
            case Some(Command.Memory) => mem(op) :: stck
            case Some(Command.UserFunction) => (evaluateOps(Command usercmds op, stck) split delim).toList
            case _ => op :: stck // add value to stack

    def formatOutput(output: String): String =
        if output.isEmpty then return ""

        (output split delim)
        .zipWithIndex
        .reverse
        .map {(element, i) =>
            val pref = if i <= 'z' - 'a' then (i + 'a').toChar else "~"
            s"${pref}.  $element"
        } mkString "\n"