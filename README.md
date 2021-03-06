# commandline
A command line parser for Scala that follows [POSIX and GNU conventions](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html). No dependencies, no macros and no implicits.

```
resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "com.github.ahnfelt" %% "commandline" % "0.4-SNAPSHOT"
```

# Declaring the command line
```scala
val commandLine = CommandLine("A tool with command line parameters.",
    optional(LongParser, "-n", "--count"),
    optional(EntityTypeParser, "-t", "--entitytype", "ENTITYTYPE"),
    branch[Mode](
        "run" -> CommandLine("Run scripts.",
            optional(StringParser, "-p", "--path"),
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

The line `optional(EntityTypeParser, "-t", "--entitytype", "ENTITYTYPE")` uses a custom argument parser `EntityTypeParser`. It's optional, and the argument is given as `-t ...`, `--entitytype ...` or as a fallback, the value of the environment variable `ENTITYTYPE`.

The `branch` is used to branch on subcommands, eg. `commit` in `git commit`.

# Parsing the command line
```scala
val result = commandLine.parseOrExit(args)
```

Note that `result` is your own case class, in this case `Arguments`, which is defined in the following section.

# Parse into your own data structures

The `commandLine.parseOrExit(args)` returns your own data structure, eg:

```scala
sealed abstract class Mode
case class RunMode(path : Option[String], verbose : Boolean, recursive : Boolean, files : List[String]) extends Mode
case class ReportMode(file : String, port : Long) extends Mode

case class Arguments(
    number : Option[Long],
    entityType : Option[Boolean],
    mode : Mode
)
```

# Printing out USAGE style help

When using parseOrExit, `-?` prints usage:
```
Process files.

-n, --number <integer>

-t, --entitytype, ENTITYTYPE <type>

run:
    Run scripts.

    -p, --path <string>

    -v, --verbose

    -r, --recursive

    file [...]

report:
    Report progress.

    --file <string> (required)

    --port <integer> (required)

In addition, -? prints this information and -* prints a list of keywords
suitable for bash completion, eg. complete -D -W "..." mycommand.
```

# Bash completion for your command line

Use `-*` to print flags, options and commands, suitable for bash completion via `complete -D -W "..." mycommand`:
```
--count --entitytype --file --path --port --recursive --verbose -n -p -r -t -v report run
```


# Define your own argument parsers
```scala
object EntityTypeParser extends Parser[Boolean] {
    override def parse(v : String) =
        if(v == "Person") true
        else if(v == "Organization") false
        else throw CommandLineException("Expected " + format + ", got " + v)
    override def format = "<type>"
}
```

# POSIX and GNU compatibility
Support for `-xyz`, equivalent to `-x -y -z`.

Support for `-n10`, equivalent to `-n 10`.

Support for `--foo=bar`, equivalent to `--foo bar`.

After a lone `--`, everything is treated as positional arguments.

A lone `-` is treated as a positional argument.

# Environment variables and flags
When using environment variables for flags, the following values
are accepted (in both upper, lower and mixed case):

 * `"true"`, `"t"`, `"yes"`, `"y"` and `"1"` is considered true.
 * `"false"`, `"f"`, `"no"`, `"n"`, `"0"` and `""` is considered false.
 * All other values cause an error.
