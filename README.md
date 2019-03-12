# commandline
A command line parser for Scala that follows [POSIX and GNU conventions](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html). No dependencies, no macros and no implicits.

# Declaring the command line
```scala
val commandLine = CommandLine("A tool to process files.",
    optional(LongParser, "-e", "--enhedsnummer"),
    optional(EntityTypeParser, "-t", "--entitytype", "ENTITY_TYPE"),
    branch(
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
```

The line `optional(EntityTypeParser, "-t", "--entitytype", "ENTITY_TYPE")` uses a custom argument parser `EntityTypeParser`. It's optional, and the argument is given as `-t ...`, `--entitytype ...` or as a fallback, the environment variable `ENTITY_TYPE`.

The `branch` is used to branch on subcommands, eg. `commit` in `git commit`.

# Parsing the command line
```scala
val result = commandLine.parseOrExit(args)
```

When using parseOrExit, `-?` prints usage:
```
USAGE: (get help: -?)
    A tool to process files.
    -e, --enhedsnummer <integer>
    -t, --entitytype, ENTITY_TYPE {Person, Organization}
    run:
        Run scripts.
        -v, --verbose
        -r, --recursive
        file [...] <string>
    report:
        Report progress.
        --file <string> (required)
        --port <integer> (required)
```

And `-*` prints flags, options and commands, suitable for bash completion via `complete -D -W "..." mycommand`:
```
--enhedsnummer --entitytype --file --port --recursive --verbose -e -r -t -v report run
```

# Parse into your own data structures
```scala
sealed abstract class Mode
case class RunMode(verbose : Boolean, recursive : Boolean, files : List[String]) extends Mode
case class ReportMode(file : String, port : Long) extends Mode

case class Arguments(
    enhedsnummer : Option[Long],
    entityType : Option[Boolean],
    mode : Mode
)
```

# Define your own argument parsers
```scala
object EntityTypeParser extends Parser[Boolean] {
    override def parse(v : String) =
        if(v == "Person") true
        else if(v == "Organization") false
        else throw CommandLineException("Expected " + format + ", got " + v)
    override def format = "{Person, Organization}"
}
```

# To be implemented
In POSIX, it's possible to give arguments to short options without a separating space, eg. `-n10`. This remains to be implemented.

Currently, environment variables for flags is treated as `true` when defined and `false` otherwise. Perhaps the value should be parsed as a boolean value, ie. "", "false", "no", "n" or "0" for false and "true", "yes", "y" or "1" for true.

It would also be nice with an even prettier USAGE text, and man page generation.
