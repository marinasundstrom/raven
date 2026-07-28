# File-based application samples

These examples run directly through the Raven frontend without a `.rvnproj`
project file.

After building the tools and sourcing `scripts/raven-env.sh` from the repository
root:

```bash
rvn run samples/scripts/hello.rvn -- Raven
```

The source path itself is shorthand for `run`:

```bash
rvn samples/scripts/hello.rvn Raven
```

On macOS or Linux, the sample is executable and its shebang resolves the same
frontend through the development `PATH` configured by `scripts/raven-env.sh`:

```bash
./samples/scripts/hello.rvn Raven
```

The explicit `rvn run` form passes arguments after `--` to the application's
`Main(args: string[])` entry point. The shorthand and direct shebang forms pass
the arguments following the source path naturally. Compilation artifacts are
isolated in a temporary directory and removed after the application exits.
