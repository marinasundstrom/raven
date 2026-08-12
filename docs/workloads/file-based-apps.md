# Build a file-based application

A Raven application can be a single `.rvn` source file. Execute it directly
with `rvn` without creating a project file or running a separate build command
first.

File-based applications are useful for scripts, local automation, small
command-line tools, learning exercises, and experiments that do not need
project-level dependencies or configuration.

## Write one source file

Create `hello.rvn`:

```raven
import System.*

func Main(args: string[]) {
    Console.WriteLine("Hello from Raven!")

    for argument in args {
        Console.WriteLine("Argument: ${argument}")
    }
}
```

`Main` receives command-line arguments as an ordinary .NET string array. A
file-based application imports namespaces and calls .NET APIs just like a
project-based Raven application.

## Execute the file directly

Run the source and pass arguments after `--`:

```bash
rvn run hello.rvn -- Raven
```

The source path itself is shorthand for `run`:

```bash
rvn hello.rvn Raven
```

There is no explicit compilation or build step. `rvn` compiles the source into
isolated temporary artifacts, runs the resulting .NET application, returns its
exit code, and removes the artifacts afterward.

## Use a shebang on macOS or Linux

Add a shebang as the first line:

```raven
#!/usr/bin/env rvn
```

Make the source executable and run it like a script:

```bash
chmod +x hello.rvn
./hello.rvn Raven
```

The `rvn` launcher must be available on `PATH`. In a Raven source checkout,
sourcing `scripts/raven-env.sh` provides the repository-local launcher.

## Move to a project when needed

Move the application into a `.rvnproj` when you need:

- several source files with an explicit project structure
- NuGet package or framework references
- target-framework, build, or publishing configuration
- analyzers, source generators, or other project extensions
- repeatable build artifacts instead of temporary execution artifacts

Continue with the [getting-started guide](../getting-started.md) to create a
project. The checked-in
[`samples/scripts/hello.rvn`](https://github.com/marinasundstrom/raven/blob/main/samples/scripts/hello.rvn)
is a runnable example of this workflow.
