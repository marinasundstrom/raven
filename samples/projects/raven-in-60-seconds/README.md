# Raven in 60 seconds

A compact, screenshot-friendly example intended for talks and social posts. It
shows Raven's core proposition in one domain workflow:

- immutable lexical bindings with `let`
- records and discriminated unions for domain modeling
- exhaustive pattern matching with payload extraction
- `Option<T>` instead of nullable domain state
- `Result<T, E>` and `?` instead of exceptions for expected failure
- expression-oriented `if` and `match`
- direct reuse of .NET collections and console APIs

The output is:

```text
✓ REQ-1002: 400 cents
```

## Build and run

```bash
dotnet build RavenIn60Seconds.rvnproj --property WarningLevel=0
dotnet run --project RavenIn60Seconds.rvnproj --property WarningLevel=0
```

For a LinkedIn post, use the source through the final `Quote` record as the
image and put the two small unions in a second image or the first comment.
