# Timer Macro

This project uses Raven's standard `timer` macro to surround an authored Raven
block with `System.Diagnostics.Stopwatch` boilerplate:

```raven
timer! {
    let indexName = "products"
    RebuildIndex(indexName)
    WriteLine("Index saved")
}
```

Conceptually, the invocation expands to:

```raven
{
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
        System.Console.WriteLine(__stopwatch.Elapsed)
    }
}
```

The body is parsed as an ordinary Raven block. The expansion starts a stopwatch,
executes the authored statements in place, then stops and reports the elapsed
duration from a `finally` clause. The actual stopwatch name is generated to
avoid collisions. Release builds also report `TIMER002` so instrumentation left
in release code is visible.

Run the sample:

```bash
dotnet run --project samples/projects/timer-macro/TimerMacro.rvnproj \
  --property WarningLevel=0
```

The output contains `Index rebuilt`, `Index saved`, and the elapsed duration.
