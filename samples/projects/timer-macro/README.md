# Timer Macro

This project uses Raven's standard `timer` macro to surround an authored Raven
block with `System.Diagnostics.Stopwatch` boilerplate:

```raven
let indexName = "products"
timer! "$indexName index rebuilt in {time}" {
    RebuildIndex(indexName)
    WriteLine("Index saved")
}
```

Conceptually, the invocation expands to:

```raven
{
    let __message = "$indexName index rebuilt in {time}"
    let __stopwatch = System.Diagnostics.Stopwatch.StartNew()
    try {
        {
            let indexName = "products"
            RebuildIndex(indexName)
            WriteLine("Index saved")
        }
    }
    finally {
        __stopwatch.Stop()
        System.Console.WriteLine(
            __message.Replace("{time}", __stopwatch.Elapsed.ToString()))
    }
}
```

The optional expression-header message is an ordinary Raven string expression.
Here `$indexName` uses normal caller-scope interpolation, while the timer macro
replaces the literal `{time}` placeholder with the elapsed duration. The body is
parsed as an ordinary Raven block. The expansion starts a stopwatch, executes
the authored statements in place, then stops and reports the message from a
`finally` clause. The actual generated names avoid collisions. Release builds
also report `TIMER002` so instrumentation left in release code is visible.

Run the sample:

```bash
dotnet run --project samples/projects/timer-macro/TimerMacro.rvnproj \
  --property WarningLevel=0
```

The output contains `Index rebuilt`, `Index saved`, and a final message such as
`products index rebuilt in 00:00:00.0012345`.
