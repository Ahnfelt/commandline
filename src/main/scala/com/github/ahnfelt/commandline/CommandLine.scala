package com.github.ahnfelt.commandline
import CommandLine._

import scala.util.Try

object CommandLineExample {

    sealed abstract class Mode
    case class RunMode(verbose : Boolean, recursive : Boolean, files : List[String]) extends Mode
    case class ReportMode(file : String, port : Long) extends Mode

    case class Arguments(
        enhedsnummer : Option[Long],
        entityType : Option[Boolean],
        mode : Mode
    )

    object EntityTypeParser extends Parser[Boolean] {
        override def parse(v : String) =
            if(v == "Person") true
            else if(v == "Organization") false
            else throw CommandLineException("Expected " + format + ", got " + v)
        override def format = "{Person, Organization}"
    }

    val commandLine = CommandLine("Process files.",
        optional(LongParser, "-e", "--enhedsnummer"),
        optional(EntityTypeParser, "-t", "--entitytype", "ENTITY_TYPE"),
        branch[Mode](
            "run" -> CommandLine("Run scripts.",
                flag("-v", "--verbose"),
                flag("-r", "--recursive"),
                requiredList(StringParser, "file"),
            )(RunMode),
            "report" -> CommandLine("Report progress.",
                required(StringParser, "--file"),
                required(LongParser, "--port")
            )(ReportMode),
        )
    )(Arguments)

    def main(arguments : Array[String]) : Unit = {
        println(commandLine.parseOrExit(arguments))

        //println(commandLine.usage)
        //println()
        //println(commandLine.parseOrExit("-e 42 --entitytype Person run foo.txt".split(" ")))
        //println(commandLine.parseOrExit("--entitytype Person run -v foo.txt".split(" ")))
        //println(commandLine.parseOrExit("run -vr --file foo.txt".split(" ")))

        /*println()
        for(n <- 0 to 22) println(generateApply(n) + "\n")*/
    }

    def generateApply(n : Int) : String = {
        val xs = 1 to n
        s"""    def apply[${xs.map(x => s"T$x, ").mkString}R](
        description : String${xs.map(x => s",\n        p$x : Parameter[T$x]").mkString}
    )(f : (${xs.map(x => s"T$x").mkString(", ")}) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                ${xs.map(x => s"p$x").mkString(", ")}
            ).map(_.usage(indentation)).mkString("\\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
${xs.map(x => s"            val r$x = Reference(p$x)").mkString("\n")}
            parseReferences(arguments, environment${xs.map(x => s", r$x").mkString})
            f(${xs.map(x => s"r$x.state.get").mkString(", ")})
        }
    }"""
    }
}

object CommandLine extends CommandLineGenerated {

    def flag(labels : String*) =
        FlagParameter(labels.toList)

    def required[T](parser : Parser[T], label : String, labels : String*) =
        if(!label.headOption.exists(_.isLower)) RequiredParameter(parser, label :: labels.toList)
        else if(labels.nonEmpty) throw CommandLineException("Multiple names not permitted for value: " + label)
        else RequiredValueParameter(parser, label)

    def optional[T](parser : Parser[T], label : String, labels : String*) =
        if(!label.headOption.exists(_.isLower)) OptionalParameter(parser, label :: labels.toList)
        else if(labels.nonEmpty) throw CommandLineException("Multiple names not permitted for value: " + label)
        else OptionalValueParameter(parser, label)

    def requiredList[T](parser : Parser[T], name : String) =
        MultiValueParameter(parser, name, nonEmpty = true)

    def optionalList[T](parser : Parser[T], name : String) =
        MultiValueParameter(parser, name, nonEmpty = false)

    def branch[T](branches : (String, CommandLine[T])*) =
        BranchParameter(branches.toList)

}

trait CommandLine[T] { self =>

    def usage : String = "USAGE:\n" + usage(1)

    def usage(indentation : Int) : String =
        help.lines.map("    " * indentation + _ + "\n").mkString +
            parameterUsage(indentation)

    def parameterUsage(indentation : Int) : String

    def consume(arguments : List[String], environment : Map[String, String]) : T

    def parse(arguments : Array[String], environment : Map[String, String] = env()) : T = {
        val beforeDashDash = arguments.takeWhile(_ != "--")
        val rest = arguments.drop(beforeDashDash.length).toList
        val expanded = beforeDashDash.toList.flatMap { a =>
            if(a.startsWith("-") && a.drop(1).headOption.exists(_.isLower)) {
                a.drop(1).toList.map("-" + _)
            } else if(a.startsWith("--") && a.contains("=")) {
                a.split("=", 2).toList
            } else List(a)
        }
        consume(expanded ++ rest, environment)
    }

    def parseEither(arguments : Array[String], environment : Map[String, String] = env()) : Either[String, T] =
        try {
            Right(parse(arguments, environment))
        } catch {
            case e : CommandLineException => Left(e.getMessage)
        }

    def parseOrExit(arguments : Array[String], environment : Map[String, String] = env(), errorExitCode : Int = 1) : T =
        try {
            arguments match {
                case Array("-?") =>
                    System.err.println("USAGE: (get help: -?)\n" + usage(1))
                    System.exit(0)
                    throw CommandLineException("This never happens.")
                case _ =>
                    parse(arguments, environment)
            }
        } catch {
            case e : CommandLineException =>
                System.err.println("ERROR: (get help: -?)")
                System.err.println("    " + e.getMessage)
                System.exit(errorExitCode)
                throw CommandLineException("This never happens.")
        }

    private def env() : Map[String, String] = {
        var environment = Map[String, String]()
        System.getenv().forEach((k, v) => environment += k -> v)
        environment
    }

    def help : String = ""

    def help(description : String) : CommandLine[T] = new CommandLine[T] {
        override def parameterUsage(indentation : Int) = self.parameterUsage(indentation)
        override def consume(arguments : List[String], environment : Map[String, String]) =
            self.consume(arguments, environment)
        override def help = description
    }

}

trait Parameter[T] {
    val positional : Boolean
    val labels : Set[String]
    def usage(indentation : Int) : String
    def consume(arguments : List[String], environment : Map[String, String]) : (T, List[String])
    def missing(environment : Map[String, String]) : T
    def help(description : String) : DocumentedParameter[T] = DocumentedParameter(this, description)
}

case class DocumentedParameter[T](parameter : Parameter[T], description : String) extends Parameter[T] {
    override val positional = parameter.positional
    override val labels = parameter.labels
    override def usage(indentation : Int) =
        parameter.usage(indentation) + "\n" +
            description.lines.map("    " * (indentation + 1) + _).mkString("\n")
    override def consume(arguments : List[String], environment : Map[String, String]) =
        parameter.consume(arguments, environment)
    override def missing(environment : Map[String, String]) = parameter.missing(environment)
    override def help(description : String) : DocumentedParameter[T] = copy(description = description)
}

case class RequiredParameter[T](parser : Parser[T], labelList : List[String]) extends Parameter[T] {
    val positional = false
    val labels = labelList.toSet
    def usage(indentation : Int) =
        "    " * indentation + labels.mkString(", ") + " " + parser.format + " (required)"
    def consume(arguments : List[String], environment : Map[String, String]) = {
        arguments.drop(1).headOption match {
            case Some(value) => parser.parse(value) -> arguments.drop(2)
            case None => throw CommandLineException("Expected argument after: " + arguments.head)
        }
    }
    override def missing(environment : Map[String, String]) = {
        labelList.filter(_.headOption.exists(_.isUpper)).collectFirst {
            case l if environment.contains(l) =>
                parser.parse(environment(l))
        }.getOrElse {
            throw CommandLineException("Missing parameter: " + usage(0))
        }
    }

}

case class OptionalParameter[T](parser : Parser[T], labelList : List[String]) extends Parameter[Option[T]] {
    val positional = false
    val labels = labelList.toSet
    def usage(indentation : Int) =
        "    " * indentation + labels.mkString(", ") + " " + parser.format
    def consume(arguments : List[String], environment : Map[String, String]) = {
        arguments.drop(1).headOption match {
            case Some(value) => Some(parser.parse(value)) -> arguments.drop(2)
            case None => throw CommandLineException("Expected argument after: " + arguments.head)
        }
    }
    override def missing(environment : Map[String, String]) = {
        labelList.filter(_.headOption.exists(_.isUpper)).collectFirst {
            case l if environment.contains(l) =>
                parser.parse(environment(l))
        }
    }
}

case class FlagParameter(labelList : List[String]) extends Parameter[Boolean] {
    val positional = false
    val labels = labelList.toSet
    def usage(indentation : Int) =
        "    " * indentation + labels.mkString(", ")
    def consume(arguments : List[String], environment : Map[String, String]) = {
        true -> arguments.drop(1)
    }
    override def missing(environment : Map[String, String]) = {
        labelList.filter(_.headOption.exists(_.isUpper)).exists { environment.contains }
    }
}

case class OptionalValueParameter[T](parser : Parser[T], name : String) extends Parameter[Option[T]] {
    val positional = true
    val labels = Set()
    def usage(indentation : Int) =
        "    " * indentation + "[" + name + "] " + parser.format
    def consume(arguments : List[String], environment : Map[String, String]) = {
        Some(parser.parse(arguments.head)) -> arguments.tail
    }
    override def missing(environment : Map[String, String]) =
        None
}

case class RequiredValueParameter[T](parser : Parser[T], name : String) extends Parameter[T] {
    val positional = true
    val labels = Set()
    def usage(indentation : Int) =
        "    " * indentation + name + " " + parser.format
    def consume(arguments : List[String], environment : Map[String, String]) = {
        parser.parse(arguments.head) -> arguments.tail
    }
    override def missing(environment : Map[String, String]) =
        throw CommandLineException("Missing value: " + usage(0))
}

case class MultiValueParameter[T](parser : Parser[T], name : String, nonEmpty : Boolean) extends Parameter[List[T]] {
    val positional = true
    val labels = Set()
    def usage(indentation : Int) =
        if(nonEmpty) "    " * indentation + name + " [...] " + parser.format
        else "    " * indentation + "[" + name + " ...] " + parser.format
    def consume(arguments : List[String], environment : Map[String, String]) = {
        var remaining = arguments
        var result = List[T]()
        while(remaining.nonEmpty) {
            result ::= parser.parse(remaining.head)
            remaining = remaining.tail
        }
        result.reverse -> remaining
    }
    override def missing(environment : Map[String, String]) =
        throw CommandLineException("Missing value: " + usage(0))
}

case class BranchParameter[T](branches : List[(String, CommandLine[T])]) extends Parameter[T] {
    val positional = false
    val labels = branches.map(_._1).toSet
    def usage(indentation : Int) =
        branches.map { case (key, value) =>
            "    " * indentation + key + ":\n" + value.usage(indentation + 1)
        }.mkString("\n")
    def consume(arguments : List[String], environment : Map[String, String]) = {
        branches.find(_._1 == arguments.head).get._2.consume(arguments.tail, environment) -> List()
    }
    override def missing(environment : Map[String, String]) =
        throw CommandLineException("Missing command: " + branches.map(_._1).mkString(", "))
}

trait Parser[T] {
    def format : String
    def parse(argument : String) : T
}

object StringParser extends Parser[String] {
    override def parse(v : String) = v
    override def format = "<string>"
}

object IntParser extends Parser[Int] {
    override def parse(v : String) = Try(v.toInt).getOrElse {
        throw CommandLineException("Expected " + format + ", got " + v)
    }
    override def format = "<integer>"
}

object LongParser extends Parser[Long] {
    override def parse(v : String) = Try(v.toLong).getOrElse {
        throw CommandLineException("Expected " + format + ", got " + v)
    }
    override def format = "<integer>"
}

object FloatParser extends Parser[Float] {
    override def parse(v : String) = Try(v.toFloat).getOrElse {
        throw CommandLineException("Expected " + format + ", got " + v)
    }
    override def format = "<decimal>"
}

object DoubleParser extends Parser[Double] {
    override def parse(v : String) = Try(v.toDouble).getOrElse {
        throw CommandLineException("Expected " + format + ", got " + v)
    }
    override def format = "<decimal>"
}

case class CommandLineException(message : String) extends RuntimeException(message)

object CommandLineGenerated {

    protected def parseReferences(
        arguments : List[String],
        environment : Map[String, String],
        references : Reference[_]*
    ) : Unit = {
        var dashDash = false
        var remaining = arguments
        val referenceMap = references.flatMap(r => r.parameter.labels.toList.map(_ -> r)).toMap[String, Reference[_]]
        while(remaining.nonEmpty) {
            val argument = remaining.head
            if(argument == "--" && !dashDash) {
                dashDash = true
                remaining = remaining.tail
            } else {
                referenceMap.get(argument) match {
                    case Some(r) if !dashDash =>
                        if(r.seen) throw CommandLineException("Duplicate parameter: " + argument)
                        remaining = r.consume(remaining, environment)
                    case None if !dashDash && argument.startsWith("-") && argument != "-" =>
                        throw CommandLineException("Unknown parameter: " + argument)
                    case _ =>
                        references.find(r => !r.seen && r.parameter.positional) match {
                            case Some(r) =>
                                val before = remaining.takeWhile(a => !a.startsWith("-") || a == "-" || dashDash)
                                val after = remaining.drop(before.length)
                                remaining = r.consume(before, environment) ++ after
                            case None =>
                                throw CommandLineException("Unexpected parameter: " + argument)
                        }
                }
            }
        }
        references.foreach { r => if(!r.seen) r.missing(environment) }
    }

    protected case class Reference[T](parameter : Parameter[T]) {
        var state : Option[T] = None
        var seen : Boolean = false
        def consume(arguments : List[String], environment : Map[String, String]) : List[String] = {
            seen = true
            val (result, remaining) = parameter.consume(arguments, environment)
            state = Some(result)
            remaining
        }
        def missing(environment : Map[String, String]) : Unit = {
            state = Some(parameter.missing(environment))
        }
    }

}

class CommandLineGenerated {

    import CommandLineGenerated._

    def apply[R](
        description : String
    )(f : () => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](

            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {

            parseReferences(arguments, environment)
            f()
        }
    }

    def apply[T1, R](
        description : String,
        p1 : Parameter[T1]
    )(f : (T1) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            parseReferences(arguments, environment, r1)
            f(r1.state.get)
        }
    }

    def apply[T1, T2, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2]
    )(f : (T1, T2) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            parseReferences(arguments, environment, r1, r2)
            f(r1.state.get, r2.state.get)
        }
    }

    def apply[T1, T2, T3, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3]
    )(f : (T1, T2, T3) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            parseReferences(arguments, environment, r1, r2, r3)
            f(r1.state.get, r2.state.get, r3.state.get)
        }
    }

    def apply[T1, T2, T3, T4, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4]
    )(f : (T1, T2, T3, T4) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            parseReferences(arguments, environment, r1, r2, r3, r4)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5]
    )(f : (T1, T2, T3, T4, T5) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6]
    )(f : (T1, T2, T3, T4, T5, T6) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7]
    )(f : (T1, T2, T3, T4, T5, T6, T7) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10],
        p11 : Parameter[T11]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            val r11 = Reference(p11)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get, r11.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10],
        p11 : Parameter[T11],
        p12 : Parameter[T12]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            val r11 = Reference(p11)
            val r12 = Reference(p12)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get, r11.state.get, r12.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10],
        p11 : Parameter[T11],
        p12 : Parameter[T12],
        p13 : Parameter[T13]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            val r11 = Reference(p11)
            val r12 = Reference(p12)
            val r13 = Reference(p13)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get, r11.state.get, r12.state.get, r13.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10],
        p11 : Parameter[T11],
        p12 : Parameter[T12],
        p13 : Parameter[T13],
        p14 : Parameter[T14]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            val r11 = Reference(p11)
            val r12 = Reference(p12)
            val r13 = Reference(p13)
            val r14 = Reference(p14)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get, r11.state.get, r12.state.get, r13.state.get, r14.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10],
        p11 : Parameter[T11],
        p12 : Parameter[T12],
        p13 : Parameter[T13],
        p14 : Parameter[T14],
        p15 : Parameter[T15]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            val r11 = Reference(p11)
            val r12 = Reference(p12)
            val r13 = Reference(p13)
            val r14 = Reference(p14)
            val r15 = Reference(p15)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get, r11.state.get, r12.state.get, r13.state.get, r14.state.get, r15.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10],
        p11 : Parameter[T11],
        p12 : Parameter[T12],
        p13 : Parameter[T13],
        p14 : Parameter[T14],
        p15 : Parameter[T15],
        p16 : Parameter[T16]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            val r11 = Reference(p11)
            val r12 = Reference(p12)
            val r13 = Reference(p13)
            val r14 = Reference(p14)
            val r15 = Reference(p15)
            val r16 = Reference(p16)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get, r11.state.get, r12.state.get, r13.state.get, r14.state.get, r15.state.get, r16.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10],
        p11 : Parameter[T11],
        p12 : Parameter[T12],
        p13 : Parameter[T13],
        p14 : Parameter[T14],
        p15 : Parameter[T15],
        p16 : Parameter[T16],
        p17 : Parameter[T17]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            val r11 = Reference(p11)
            val r12 = Reference(p12)
            val r13 = Reference(p13)
            val r14 = Reference(p14)
            val r15 = Reference(p15)
            val r16 = Reference(p16)
            val r17 = Reference(p17)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get, r11.state.get, r12.state.get, r13.state.get, r14.state.get, r15.state.get, r16.state.get, r17.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10],
        p11 : Parameter[T11],
        p12 : Parameter[T12],
        p13 : Parameter[T13],
        p14 : Parameter[T14],
        p15 : Parameter[T15],
        p16 : Parameter[T16],
        p17 : Parameter[T17],
        p18 : Parameter[T18]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            val r11 = Reference(p11)
            val r12 = Reference(p12)
            val r13 = Reference(p13)
            val r14 = Reference(p14)
            val r15 = Reference(p15)
            val r16 = Reference(p16)
            val r17 = Reference(p17)
            val r18 = Reference(p18)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get, r11.state.get, r12.state.get, r13.state.get, r14.state.get, r15.state.get, r16.state.get, r17.state.get, r18.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10],
        p11 : Parameter[T11],
        p12 : Parameter[T12],
        p13 : Parameter[T13],
        p14 : Parameter[T14],
        p15 : Parameter[T15],
        p16 : Parameter[T16],
        p17 : Parameter[T17],
        p18 : Parameter[T18],
        p19 : Parameter[T19]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            val r11 = Reference(p11)
            val r12 = Reference(p12)
            val r13 = Reference(p13)
            val r14 = Reference(p14)
            val r15 = Reference(p15)
            val r16 = Reference(p16)
            val r17 = Reference(p17)
            val r18 = Reference(p18)
            val r19 = Reference(p19)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get, r11.state.get, r12.state.get, r13.state.get, r14.state.get, r15.state.get, r16.state.get, r17.state.get, r18.state.get, r19.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10],
        p11 : Parameter[T11],
        p12 : Parameter[T12],
        p13 : Parameter[T13],
        p14 : Parameter[T14],
        p15 : Parameter[T15],
        p16 : Parameter[T16],
        p17 : Parameter[T17],
        p18 : Parameter[T18],
        p19 : Parameter[T19],
        p20 : Parameter[T20]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            val r11 = Reference(p11)
            val r12 = Reference(p12)
            val r13 = Reference(p13)
            val r14 = Reference(p14)
            val r15 = Reference(p15)
            val r16 = Reference(p16)
            val r17 = Reference(p17)
            val r18 = Reference(p18)
            val r19 = Reference(p19)
            val r20 = Reference(p20)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get, r11.state.get, r12.state.get, r13.state.get, r14.state.get, r15.state.get, r16.state.get, r17.state.get, r18.state.get, r19.state.get, r20.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10],
        p11 : Parameter[T11],
        p12 : Parameter[T12],
        p13 : Parameter[T13],
        p14 : Parameter[T14],
        p15 : Parameter[T15],
        p16 : Parameter[T16],
        p17 : Parameter[T17],
        p18 : Parameter[T18],
        p19 : Parameter[T19],
        p20 : Parameter[T20],
        p21 : Parameter[T21]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            val r11 = Reference(p11)
            val r12 = Reference(p12)
            val r13 = Reference(p13)
            val r14 = Reference(p14)
            val r15 = Reference(p15)
            val r16 = Reference(p16)
            val r17 = Reference(p17)
            val r18 = Reference(p18)
            val r19 = Reference(p19)
            val r20 = Reference(p20)
            val r21 = Reference(p21)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get, r11.state.get, r12.state.get, r13.state.get, r14.state.get, r15.state.get, r16.state.get, r17.state.get, r18.state.get, r19.state.get, r20.state.get, r21.state.get)
        }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](
        description : String,
        p1 : Parameter[T1],
        p2 : Parameter[T2],
        p3 : Parameter[T3],
        p4 : Parameter[T4],
        p5 : Parameter[T5],
        p6 : Parameter[T6],
        p7 : Parameter[T7],
        p8 : Parameter[T8],
        p9 : Parameter[T9],
        p10 : Parameter[T10],
        p11 : Parameter[T11],
        p12 : Parameter[T12],
        p13 : Parameter[T13],
        p14 : Parameter[T14],
        p15 : Parameter[T15],
        p16 : Parameter[T16],
        p17 : Parameter[T17],
        p18 : Parameter[T18],
        p19 : Parameter[T19],
        p20 : Parameter[T20],
        p21 : Parameter[T21],
        p22 : Parameter[T22]
    )(f : (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R) = new CommandLine[R] {
        override def help = description
        override def parameterUsage(indentation : Int) =
            Seq[Parameter[_]](
                p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22
            ).map(_.usage(indentation)).mkString("\n")
        override def consume(arguments : List[String], environment : Map[String, String]) : R = {
            val r1 = Reference(p1)
            val r2 = Reference(p2)
            val r3 = Reference(p3)
            val r4 = Reference(p4)
            val r5 = Reference(p5)
            val r6 = Reference(p6)
            val r7 = Reference(p7)
            val r8 = Reference(p8)
            val r9 = Reference(p9)
            val r10 = Reference(p10)
            val r11 = Reference(p11)
            val r12 = Reference(p12)
            val r13 = Reference(p13)
            val r14 = Reference(p14)
            val r15 = Reference(p15)
            val r16 = Reference(p16)
            val r17 = Reference(p17)
            val r18 = Reference(p18)
            val r19 = Reference(p19)
            val r20 = Reference(p20)
            val r21 = Reference(p21)
            val r22 = Reference(p22)
            parseReferences(arguments, environment, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22)
            f(r1.state.get, r2.state.get, r3.state.get, r4.state.get, r5.state.get, r6.state.get, r7.state.get, r8.state.get, r9.state.get, r10.state.get, r11.state.get, r12.state.get, r13.state.get, r14.state.get, r15.state.get, r16.state.get, r17.state.get, r18.state.get, r19.state.get, r20.state.get, r21.state.get, r22.state.get)
        }
    }

}
