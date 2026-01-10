# Raven Programming Language

Raven is a modern, general-purpose programming language for the .NET platform.
It emphasizes clarity, safety, and expressive APIs while remaining deeply
interoperable with the CLR ecosystem. Raven is designed for everyday application
development, with language features that help model absence, failure, and control
flow explicitly—without excessive ceremony.

## Influences at a glance

- **Swift & Kotlin** influence Raven’s concise, expression-oriented syntax and
  focus on readable, intention-revealing code.
- **Rust** contributes ideas around explicit error handling, exhaustiveness, and
  flow-sensitive analysis.
- **F# and functional heritage** inspire immutable-by-default bindings,
  composable abstractions, and small, reusable expressions.
- **The .NET ecosystem** anchors Raven in pragmatic interop: existing libraries,
  metadata, and tooling work seamlessly alongside Raven code.

## Language philosophy

Raven aims to make common programming concerns explicit and safe:

- **Model absence and failure directly.**  
  Instead of relying on sentinel values or unchecked exceptions, Raven encourages
  the use of discriminated unions such as `Option<T>` and `Result<T>` to represent
  missing values and recoverable errors.

- **Expression-oriented by default.**  
  Control-flow constructs like `if`, `match`, loops, and blocks are expressions
  that produce values. Statement forms remain available when clarity or control
  flow demands them.

- **Interop without friction.**  
  Raven maps cleanly onto .NET concepts such as delegates, async workflows,
  generics, and extension methods, allowing Raven and C# code to coexist naturally
  in the same solution.

- **Tooling built in.**  
  The compiler follows a Roslyn-style architecture, enabling analyzers, IDE
  services, and incremental compilation from the start.

## A quick taste of Raven

Raven uses familiar syntax, but encourages explicit handling of absence and
failure through standard library types like `Option<T>` and `Result<T>`:

```raven
import System.Console.*
import Raven.Core.*

func parsePort(text: string) -> Result<int> {
    if text.Length == 0 {
        return .Error("Port is required")
    }

    let value = int.TryParse(text)
    value match {
        .Some(port) => .Ok(port)
        .None => .Error("Invalid number")
    }
}

let result = parsePort("8080")

let message =
    result match {
        .Ok(port) => "Listening on port ${port}"
        .Error(msg) => "Configuration error: ${msg}"
    }

WriteLine(message)
````

Here, `Result<int>` makes success and failure explicit. The `match` expression is
exhaustive, so both outcomes must be handled. Each arm narrows the value to the
appropriate case, and any bindings introduced by the pattern are scoped to that
arm.

String interpolation uses `${...}` inside ordinary string literals, producing:

```
Listening on port 8080
```

> **Status:** Discriminated unions such as `Option<T>` and `Result<T>` are fully
> supported, including pattern matching and exhaustiveness checking. Code
> generation emits case structs and helpers used by `match` expressions. Follow
> the [discriminated unions investigation](investigations/discriminated-unions.md)
> for ongoing work.

## Working with extensions

Raven provides first-class language support for extension members—methods and
properties that attach to existing types without modifying their original
definitions. Extensions are declared using the `extension` keyword and target a
specific receiver type.

```raven
extension OptionExtensions for Option<int>
{
    OrZero() -> int {
        self match {
            .Some(value) => value
            .None => 0
        }
    }
}
```

Inside an extension member, `self` is a synthesized immutable parameter whose
type matches the receiver. Extension members may be functions or computed
properties and default to `public` accessibility.

Once imported, extension members participate in ordinary member lookup:

```raven
import OptionExtensions.*

let port = parsePort("invalid")
    |> Result.ToOption()
    |> OrZero()
```

When resolving `expr.Member(...)`, the compiler first considers instance members
on the receiver’s type. If no instance member matches, it searches imported
extensions whose receiver type is compatible. Instance members always win over
extensions when both are applicable.

Under the hood, extension invocations are rewritten to static method calls that
pass the receiver as the leading argument. Raven emits standard CLR metadata, so
extensions authored in Raven and extensions imported from referenced assemblies
(such as `System.Linq.Enumerable`) participate uniformly in overload resolution
and code generation.

## Pipelines

Raven supports a pipeline operator (`|>`) that feeds a value into a call,
encouraging a clear, left-to-right style when working with transformations:

```raven
let port =
    readConfig("PORT")
        |> parsePort
        |> Result.ToOption()
        |> Option.OrElse(8080)
```

When the pipeline targets an extension method, the left-hand value becomes the
extension receiver. Otherwise, it is passed as the first argument to the call.
Normal overload resolution and type inference rules apply.

## File-scope code

Raven supports file-scope code—no `Main` function is required for simple
programs:

```raven
import System.Console.*

WriteLine("Hello, World!")
```

File-scope statements form the application entry point for console programs and
coexist naturally with functions, types, and namespaces in the same file.

## Where to go next

* The **language specification** defines Raven’s syntax and semantics in detail:
  see the [Language specification](specification.md).
* Implementation details describing how Raven maps to .NET are documented in
  [dotnet-implementation.md](dotnet-implementation.md).
* Runnable examples live in the repository’s [`samples/`](../../../samples/)
  directory and exercise real language features such as `Option`, `Result`,
  pipelines, and .NET interop.

> ⚠️ This is a living document. Raven is actively evolving, and details may
> change as the language and tooling mature.
