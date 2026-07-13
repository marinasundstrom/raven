# Getting Started

This guide reflects Raven's current syntax and style used in `samples/cases` and the language spec.

## Prerequisites

- .NET SDK 9.0+
- Repository cloned locally

## 1. Build compiler essentials

```bash
scripts/codex-build.sh
```

Optional session helpers:

```bash
source scripts/raven-env.sh
```

This defines `rvn` and `rvnc` shell functions for the current terminal. Without
the helpers, use the explicit `dotnet run --project ... --` commands shown
below.

## 2. Compile and run a real Raven case sample

```bash
dotnet run -f net10.0 --project src/Raven.Compiler --property WarningLevel=0 -- \
  samples/cases/quote-summary-linq-result-option.rav -o /tmp/raven-case.dll
dotnet /tmp/raven-case.dll
```

Helpful debugging flags:

- `rvn dev syntax` print syntax tree
- `rvn dev dump pretty` pretty syntax dump
- `rvn dev bound-tree` print binder and bound tree
- `--no-emit` stop after analysis

Example:

```bash
dotnet run -f net10.0 --project src/Raven --property WarningLevel=0 -- \
  dev bound-tree samples/cases/quote-summary-linq-result-option.rav
```

## 3. Write your first Raven file

Create `hello.rav`:

```raven
import System.Console.*

func Main() -> () {
    val message = BuildGreeting("Raven")
    WriteLine(message)
}

func BuildGreeting(name: string) -> string {
    return "Hello, $name"
}
```

Compile and run:

```bash
dotnet run -f net10.0 --project src/Raven.Compiler --property WarningLevel=0 -- \
  hello.rav -o /tmp/hello.dll
dotnet /tmp/hello.dll
```

## 4. Current Raven style at a glance

- Use `val` for immutable bindings, `var` for mutable bindings.
- Members are `public` by default; use `private`/`internal`/`protected` only to narrow access.
- Use `match` for explicit branching over values, `Option`, and `Result`.
- Use `Ok(...)` / `Error(...)` and `Some(...)` / `None` for carrier-based flow.
- Function type signatures use arrow notation (for example `val op: (int, int) -> int`).
- Functions are one callable concept: named declarations can be top-level functions in namespaces, methods, or local functions, and expression forms produce function values (`func (...) => ...`, `func (...) { ... }`, or `(...) => ...`).
- Function expressions may include modifiers before `func`: `async func (...) ...`, `static func (...) ...`, or `static async func (...) ...`.
- Shorthand without `func` is mostly an ergonomic call-site form for higher-order methods (for example `items.Where(x => x > 0)`).
- `static` function expressions cannot capture locals/parameters from outer scopes.
- Use explicit constructor invocation syntax: `Foo(...)`.
- For classes/records with primary constructors:
  - `val`/`var` parameters are promoted properties.
  - For `record class`/`record struct`, positional parameters are promoted by default.
  - A record's instance data and value semantics come only from its primary-constructor parameters; use computed members or static factory methods instead of body storage or secondary constructors.

Example:

```raven
record class ShipmentRequest(val Id: string, val Carrier: string, val WeightKg: int)

func Resolve(requests: ShipmentRequest[]) -> Result<ShipmentRequest, string> {
    val req = match requests.FirstOrNone(r => r.Id == "REQ-1002") {
        Some(val item) => item
        None => return Error("Request not found")
    }

    return Ok(req)
}
```

## 5. Optional: project scaffolding workflow

```bash
mkdir hello-raven
cd hello-raven

dotnet run --project ../src/Raven -- init
dotnet build *.rvnproj
dotnet run --project *.rvnproj
```

`dotnet build` and `dotnet run --project` are the expected application workflow.
After sourcing `scripts/raven-env.sh`, `rvn build`, `rvn run`, and `rvn clean`
provide convenience wrappers over the same SDK commands.
For `net11.0` projects, add a project-local `global.json` that selects a .NET
SDK with `net11.0` support.

For more details:

- [Language introduction](introduction.md)
- [Language specification](lang/spec/language-specification.md)
- [Classes and members spec](lang/spec/classes-and-members.md)
