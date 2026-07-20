# Getting Started

This walkthrough takes a fresh source checkout from "can I build it?" to a
small Raven program and a scaffolded `.rvnproj` application.

Raven is currently source-build oriented. The commands below run the compiler
and project frontend from this repository instead of assuming a globally
installed SDK package.

If you are coming from C#, read this as more than a command checklist. The
walkthrough uses familiar C# problem shapes and shows the Raven idioms for them:
plain top-level functions instead of class wrappers, unions instead of
enum-plus-state objects, `match` instead of scattered type/enum tests,
`Result<T, E>` instead of throwing for expected failure, and `Option<T>`
instead of nullable-heavy domain code. Raven also leans on declaration keywords
so a reader can scan source and immediately see what each declaration is.

Part of learning Raven is unlearning ceremony that C# can make feel inherent to
program structure. You are not unlearning object-oriented programming. You are
learning to distinguish a real object—with identity, state, lifecycle, or
polymorphic behavior—from a class that exists only to contain `Main` or a set of
utility functions.

For a broader collection of side-by-side translations, see [Raven for C#
developers](raven-for-csharp-developers.md).

## Prerequisites

- A .NET SDK with `net10.0` targeting support.
- A local clone of this repository.
- A shell that can run the scripts in `scripts/`.

Some sample projects target `net11.0`. For those, use a project-local
`global.json` that selects an SDK with `net11.0` support.

## 1. Build the compiler

From the repository root:

```bash
scripts/codex-build.sh
```

This is the broad build entry point for a fresh checkout. It restores packages,
builds the compiler, and keeps generated syntax and bound-model files current.

After the initial build, most local iterations can use narrower commands, such
as:

```bash
dotnet build src/Raven.CodeAnalysis/Raven.CodeAnalysis.csproj --property WarningLevel=0
dotnet build src/Raven.Compiler/Raven.Compiler.csproj --property WarningLevel=0
```

## 2. Choose how to run tools

You can run tools directly through `dotnet run`:

```bash
dotnet run -f net10.0 --project src/Raven -- dev syntax path/to/file.rav
dotnet run -f net10.0 --project src/Raven.Compiler -- path/to/file.rav -o /tmp/app.dll
```

Or build the command projects once and source helper functions for the current
shell:

```bash
dotnet build src/Raven/Raven.csproj -f net10.0
dotnet build src/Raven.Compiler/Raven.Compiler.csproj -f net10.0
source scripts/raven-env.sh
```

That defines:

- `rvn` - project and developer frontend.
- `rvnc` - direct compiler-driver command.

The walkthrough keeps using explicit `dotnet run` commands so it works without
shell setup. If you sourced `scripts/raven-env.sh`, you can replace those with
the shorter `rvn` and `rvnc` forms.

## 3. Compile and run a known sample

Start with a sample that exercises .NET interop, LINQ-style extensions,
`Option`, and `Result`:

```bash
dotnet run -f net10.0 --project src/Raven.Compiler --property WarningLevel=0 -- \
  samples/cases/quote-summary-linq-result-option.rav -o /tmp/raven-case.dll
dotnet /tmp/raven-case.dll
```

To analyze without emitting an assembly, add `--no-emit`:

```bash
dotnet run -f net10.0 --project src/Raven.Compiler --property WarningLevel=0 -- \
  samples/cases/quote-summary-linq-result-option.rav --no-emit
```

To get source-highlighted diagnostics from the compiler driver, add
`--highlight`.

## 4. What to notice if you write C#

The sample is intentionally shaped like a small C# service: load a request, find
a rate plan, apply optional discounts/surcharges, and return a decision. Raven's
approach is different in a few important places.

The first adjustment is conceptual: do not begin by asking which class should
contain the code. Begin with the values and operations in the problem, then add
a class when the domain gives you a reason for one.

| Common C# shape | Raven idiom |
| --- | --- |
| Class-based `Program.Main` entry point | Top-level statements or a plain `Main` function |
| Static helper classes used only to hold functions | Plain top-level functions |
| One-method service interface | A function parameter describing the required operation |
| Declaration shape inferred mostly from context | Keywords such as `func`, `val`, `var`, `event`, `class`, `union`, and `case` |
| `FirstOrDefault()` followed by `null` checks | `FirstOrNone()` returns `Option<T>` |
| Throwing for expected validation or lookup failure | Return `Result<T, E>` |
| `try`/`catch` around ordinary parsing or service calls | `try expr` produces a `Result` value |
| `enum` plus extra properties, or a small inheritance hierarchy | `union` cases with typed payloads |
| `switch` expressions mixed with null/type checks | `match` over values, options, results, and unions |
| Mutable locals unless marked `readonly` or avoided by convention | `val` by default; `var` when mutation is intentional |
| `void` | `()` (`unit`) |

You do not need to invent a class just to write a function. Raven supports
top-level functions directly, so a small operation can stay at file or namespace
scope until it has a real reason to live on a type:

```raven
func NormalizeCarrier(name: string) -> string {
    return name.Trim().ToUpperInvariant()
}

func HasTag(tags: string[], tag: string) -> bool {
    match tags.FirstOrNone(t => t == tag) {
        Some(_) => true
        None => false
    }
}
```

Use types when they model data or behavior that belongs together. Use plain
functions when the operation is just a named transformation, lookup, validation,
or workflow step.

This is not a preference against classes. A device connection, stateful
aggregate, cache, UI component, or resource owner may naturally be a class.
Raven asks whether the object represents something, not whether code needs a
container.

For a dependency with one operation, a function parameter can state the needed
capability without inventing an interface:

```raven
func ReportTemperature(
    read: () -> Result<decimal, string>,
    publish: (decimal) -> ()) -> Result<decimal, string> {
    val temperature = read()?
    publish(temperature)
    return Ok(temperature)
}
```

Use an interface when several related operations form a real, open protocol.

Raven also makes declaration kinds visible. A function starts with `func`; an
immutable value or property starts with `val`; a mutable value or property starts
with `var`; an event starts with `event`; union alternatives start with `case`.

```raven
import System.*

class ConsoleLogger {
    event Logged: Action<string>?
    val Prefix: string = "log"
    var Count: int = 0

    func Log(message: string) -> () {
        Count = Count + 1
        Logged?.Invoke("$Prefix: $message")
    }
}
```

That consistency is intentional. Raven uses keywords to announce declarations
instead of making the reader infer too much from punctuation, modifiers, or
where a member happens to appear.

For example, a C# version of a shipment decision often starts as an enum plus
separate nullable detail fields:

```csharp
enum DecisionKind { Approve, ManualReview, Reject }

sealed record Decision(DecisionKind Kind, string? Reason);
```

Raven models the same idea as one union. Cases that need data carry that data;
cases that do not need data stay empty.

```raven
union Decision {
    case Approve
    case ManualReview(reason: string)
    case Reject(reason: string)
}
```

Consumers handle every visible shape in one `match` expression:

```raven
func FormatDecision(decision: Decision) -> string {
    match decision {
        .Approve => "Approved"
        .ManualReview(val reason) => "Review: $reason"
        .Reject(val reason) => "Rejected: $reason"
    }
}
```

Lookup and validation use `Result<T, E>` when failure is part of the domain,
not an exceptional crash path:

```raven
record class QuoteError(val Message: string)

func FindRatePlan(plans: IEnumerable<RatePlan>, carrier: string) -> Result<RatePlan, QuoteError> {
    return plans.FirstOrError(
        p => p.Carrier == carrier,
        () => QuoteError("No rate plan for carrier: $carrier"))
}
```

The caller can keep the happy path linear with `?`. If `FindRatePlan` returns
`Error`, the enclosing `Result` function returns that error immediately.

```raven
func QuoteShipment(request: ShipmentRequest, plans: IEnumerable<RatePlan>) -> Result<Quote, QuoteError> {
    val plan = FindRatePlan(plans, request.Carrier)?
    val total = plan.BaseCents + (request.WeightKg * plan.PerKgCents)
    return Ok(Quote(request.Id, request.Carrier, total))
}
```

Optional values are explicit too. Instead of using `string?` throughout domain
logic, use `Option<string>` and match it where the decision matters:

```raven
func PromoCents(code: Option<string>) -> Option<int> {
    val raw = code?
    val normalized = raw.Trim().ToUpperInvariant()

    match normalized {
        "SAVE5" => Some(500)
        "FREESHIP" => Some(200)
        _ => None
    }
}
```

This is still ordinary .NET code. The sample imports `System.Linq.*`, uses
`IEnumerable<T>`, calls string APIs, and emits IL. Raven changes the source
model for domain flow; it does not ask you to leave the .NET ecosystem.

## 5. Inspect syntax and binding

The `rvn dev` commands are useful when learning the language or debugging the
compiler.

Print the parsed syntax tree:

```bash
dotnet run -f net10.0 --project src/Raven --property WarningLevel=0 -- \
  dev syntax samples/cases/quote-summary-linq-result-option.rav
```

Print the bound tree:

```bash
dotnet run -f net10.0 --project src/Raven --property WarningLevel=0 -- \
  dev bound-tree samples/cases/quote-summary-linq-result-option.rav
```

Other useful views include:

- `rvn dev dump` - pretty syntax dump.
- `rvn dev symbols` - symbol information for a file or project.
- `rvn dev quote` - syntax factory quote output.

Creating a `.debug/` directory in the current or a parent folder also causes
`rvnc` to write debug dumps while compiling.

## 6. Write a first Raven file

Create `hello.rav` in the repository root or another scratch directory:

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

Compile and run it:

```bash
dotnet run -f net10.0 --project src/Raven.Compiler --property WarningLevel=0 -- \
  hello.rav -o /tmp/hello.dll
dotnet /tmp/hello.dll
```

The example uses `()` as the empty result type. Raven does not use `void`.

## 7. Read current Raven style

The language is still experimental, but current docs and samples follow these
rules:

- Use `let` for immutable lexical bindings and `var` for mutable lexical bindings.
- Prefer plain top-level functions for standalone operations; do not create a
  class only to hold methods.
- Use classes and interfaces when identity, lifecycle, encapsulated state, or
  open polymorphism are part of the model.
- Consider a function parameter for a dependency that consists of one
  operation.
- Let declaration keywords carry meaning: `func` declares behavior, `val` and
  `var` declare storage/bindings, `event` declares events, and `case` declares
  union alternatives.
- Members are public by default; use access modifiers to narrow visibility.
- Use `match` when branching should stay visible.
- Use `Option<T>` for absence in Raven domain code.
- Use `Result<T, E>` for expected failure and `?` to propagate it.
- Prefer explicit pattern bindings: `Some(val value)`, `val (a, b) = pair`,
  `if let Some(item) = maybe`.
- Use function type arrows, such as `val op: (int, int) -> int`.
- Use explicit constructors such as `ShipmentRequest(...)` unless the target
  type is already obvious.
- Use target-typed shorthand such as `.Active` and `.(...)` only when the
  surrounding type is clear.

Example:

```raven
import System.Linq.*

record class ShipmentRequest(val Id: string, val Carrier: string, val WeightKg: int)

func Resolve(requests: ShipmentRequest[]) -> Result<ShipmentRequest, string> {
    val request = requests.FirstOrError(
        r => r.Id == "REQ-1002",
        () => "Request not found")?

    return Ok(request)
}
```

## 8. Create a project

Project scaffolding lives behind the `rvn init` command. If you sourced
`scripts/raven-env.sh`, run:

```bash
mkdir hello-raven
cd hello-raven
rvn init --type console --name HelloRaven
rvn build HelloRaven.rvnproj
rvn run HelloRaven.rvnproj
```

Without shell helpers, keep the repository path in a variable and call the
frontend through `dotnet run`:

```bash
REPO_ROOT=/path/to/Raven
mkdir hello-raven
cd hello-raven
dotnet run -f net10.0 --project "$REPO_ROOT/src/Raven" -- init --type console --name HelloRaven
dotnet run -f net10.0 --project "$REPO_ROOT/src/Raven" -- build HelloRaven.rvnproj
dotnet run -f net10.0 --project "$REPO_ROOT/src/Raven" -- run HelloRaven.rvnproj
```

Create a class library scaffold instead:

```bash
rvn init --type classlib --name MyLibrary
```

`.rvnproj` files also participate in the normal .NET SDK workflow:

```bash
dotnet build path/to/App.rvnproj
dotnet run --project path/to/App.rvnproj
```

## 9. Where to go next

- [Raven for absolute beginners](raven-for-absolute-beginners.md) if you are new
  to programming itself.
- [Language introduction](introduction.md) for a guided feature overview.
- [Raven for C# developers](raven-for-csharp-developers.md) for side-by-side
  migration guidance.
- [Domain modeling](lang/domain-modeling.md) for choosing among functions,
  records, unions, classes, and interfaces.
- [Language philosophy](lang/philosophy.md) for design principles.
- [Language specification](lang/spec/language-specification.md) for precise
  rules.
- [Style guide](lang/style-guide.md) for source layout conventions.
- [Compiler project system](compiler/project-system.md) for `.rvnproj` details.
- [Samples](../samples/) for runnable examples.
