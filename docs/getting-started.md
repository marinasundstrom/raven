# Getting Started

This guide reflects Raven's current syntax and style used in `samples/cases` and the language spec.

## Prerequisites

- .NET SDK 9.0+
- Repository cloned locally

## 1. Build compiler essentials

```bash
scripts/codex-build.sh
```

## 2. Compile and run a real Raven case sample

```bash
dotnet run -f net9.0 --project src/Raven.Compiler --property WarningLevel=0 -- \
  samples/cases/quote-summary-linq-result-option.rav -o /tmp/raven-case.dll --run
```

Helpful debugging flags:

- `-s` print syntax tree
- `-d pretty` pretty syntax dump
- `-bt` print binder and bound tree
- `--no-emit` stop after analysis

Example:

```bash
dotnet run -f net9.0 --project src/Raven.Compiler --property WarningLevel=0 -- \
  samples/cases/quote-summary-linq-result-option.rav -d pretty -bt --no-emit
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
dotnet run -f net9.0 --project src/Raven.Compiler --property WarningLevel=0 -- \
  hello.rav -o /tmp/hello.dll --run
```

## 4. Current Raven style at a glance

- Use `val` for immutable bindings, `var` for mutable bindings.
- Members are `public` by default; use `private`/`internal`/`protected` only to narrow access.
- Use `match` for explicit branching over values, `Option`, and `Result`.
- Use `Ok(...)` / `Error(...)` and `Some(...)` / `None` for carrier-based flow.
- Function type signatures start with `func` (for example `val op: func (int, int) -> int`).
- Functions come in two forms: named declarations (`func Add(...)`) and unnamed function expressions (`func (...) => ...`, `func (...) { ... }`, or `(...) => ...`).
- Function expressions may include modifiers before `func`: `async func (...) ...`, `static func (...) ...`, or `static async func (...) ...`.
- Shorthand without `func` is mostly an ergonomic call-site form for higher-order methods (for example `items.Where(x => x > 0)`).
- `static` function expressions cannot capture locals/parameters from outer scopes.
- Use explicit constructor invocation syntax: `Foo(...)`.
- For classes/records with primary constructors:
  - `val`/`var` parameters are promoted properties.
  - For `record class`/`record struct`, positional parameters are promoted by default.

Example:

```raven
record class ShipmentRequest(val Id: string, val Carrier: string, val WeightKg: int)

func Resolve(requests: ShipmentRequest[]) -> Result<ShipmentRequest, string> {
    val req = requests.FirstOrNone(r => r.Id == "REQ-1002") match {
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

dotnet run --project ../src/Raven.Compiler -- init
dotnet run --project ../src/Raven.Compiler -- *.ravenproj
```

For more details:

- [Language introduction](introduction.md)
- [Language specification](lang/spec/language-specification.md)
- [Classes and members spec](lang/spec/classes-and-members.md)
