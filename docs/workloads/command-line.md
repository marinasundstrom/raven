# Build a command-line application

Raven can run a source file directly, which is useful for command-line tools,
automation, and learning exercises that do not need project configuration or
package references.

The complete example is [`samples/scripts/hello.rvn`](https://github.com/marinasundstrom/raven/blob/main/samples/scripts/hello.rvn).

## Write the application

```raven
#!/usr/bin/env rvn

import System.*

func Main(args: string[]) {
    Console.WriteLine("Hello from a single Raven file!")

    for argument in args {
        Console.WriteLine("Argument: ${argument}")
    }
}
```

`Main` receives command-line arguments as an ordinary .NET string array. The
program imports the `System` namespace, calls `Console.WriteLine`, iterates with
Raven's `for` statement, and uses string interpolation for output.

## Run it

After building the repository toolchain and sourcing `scripts/raven-env.sh`:

```bash
rvn run samples/scripts/hello.rvn -- Raven
```

The source path is also shorthand for `run`:

```bash
rvn samples/scripts/hello.rvn Raven
```

On macOS or Linux, the shebang lets the checked-in sample run as an executable:

```bash
./samples/scripts/hello.rvn Raven
```

Use a `.rvnproj` when the application grows to multiple files, needs package
references, or requires build configuration. Continue with the
[getting-started guide](../getting-started.md) to create a project.
