# Raven Programming Language

Raven is a modern, general-purpose language that embraces expressive syntax, strong static typing, and first-class tooling. It is designed for day-to-day application development on .NET while borrowing the best ideas from contemporary languages.

## Influences at a glance

- **Swift & Kotlin** inform Raven's expression-oriented surface syntax and emphasis on concise declarations.
- **Rust** contributes exhaustiveness checking, algebraic patterns, and flow-sensitive type analysis that keep code safe without ceremony.
- **F# and functional heritage** inspire immutable-by-default semantics, higher-order programming, and a focus on composing small expressions into larger behaviours.
- **The .NET ecosystem** anchors Raven in pragmatic interop: every Raven project can call into existing libraries and surface analyzers for C# developers.

## Language philosophy

Raven aims to balance expressive power with approachability:

- **Expression-first, imperative when you need it.** Most constructs produce values, enabling pipeline-style code, yet familiar statements like `return` remain available when clarity calls for them.
- **Types that flow with your logic.** Unions, literal types, and bidirectional inference let values carry precise information through branches, matches, and guards.
- **Interop without friction.** Raven maps cleanly onto .NET metadata, so classes, generics, and async workflows feel at home alongside existing CLR code.
- **Tooling built in.** The compiler follows the Roslyn architecture, unlocking analyzers, IDE support, and incremental compilation from the start.

## A quick taste of Raven

Raven uses familiar keywords, but patterns and unions make branching concise:

```raven
import System.Console.*

func describe(input: string | int | null) -> string {
    input match {
        null => "Nothing to report."
        string text when text.Length > 0 => "Saw \"${text}\""
        int number => "Counted ${number}"
        _ => "Empty input."
    }
}

let first = describe("Raven")
let second = describe(3)
let third = describe(null)

let summary = "${first}; ${second}; ${third}"

WriteLine(summary)
```

Because string interpolation uses `${...}` inside ordinary quotes, the final line prints `Saw "Raven"; Counted 3; Nothing to report.`. Flow-sensitive analysis ensures that `text` and `number` have the right types inside each `match` arm, and the `_` discard keeps the expression exhaustive without introducing a binding.

> **Status:** Discriminated unions parse, bind, and participate in pattern
> matching (including case-pattern exhaustiveness). Code generation emits the
> case structs, implicit conversions, and `TryGet*` helpers used by `match`
> expressions. Follow the [discriminated unions investigation](investigations/discriminated-unions.md)
> for ongoing work.

## Working with extension methods

Raven leans on the familiar .NET extension model so you can add helpers to
existing types without modifying their definitions. Declare an extension by
placing `[Extension]` on a `static` method declared inside a module or static
class and giving the receiver as the first parameter. Because extensions live in
ordinary types, you bring them into scope with the same `import` directives used
for metadata types. Raven emits the standard `ExtensionAttribute` metadata, so
both Raven and C# callers recognize the helper.【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L197-L233】【F:src/Raven.CodeAnalysis/Binder/NamespaceBinder.cs†L33-L61】

```raven
import System.Runtime.CompilerServices.*

public static class DescribeExtensions {
    [Extension]
    public static Describe(x: string) -> string {
        return $"{x} !"
    }
}

public static class EnumerableExtensions {
    [Extension]
    public static Where<T>(source: IEnumerable<T>, predicate: Func<T, bool>)
        -> IEnumerable<T> {
        // implementation omitted
    }
}

let greeting = "Raven".Describe()
let evens = [1, 2, 3, 4].Where(n => n % 2 == 0)
```

When you call the method-style syntax, Raven checks the receiver for instance
members first and then considers any imported extensions whose first parameter
matches the receiver's type. Metadata extensions delivered through referenced
assemblies—including the LINQ-inspired fixture that ships with the compiler—flow
through the same pipeline as Raven-authored helpers, so overload resolution sees
a unified candidate set. The compiler rewrites successful extension invocations
to pass the receiver as the leading argument to the static method before
lowering, producing the same IL that C# would emit.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1946-L2001】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/Lowerer.Invocation.cs†L8-L29】

> **Note:** The CLI automatically references `System.Linq.dll`, but support for
> the full BCL surface still depends on ongoing delegate inference polish. Until
> the remaining lambda compatibility work lands, the bundled fixture provides a
> stable way to exercise LINQ-style pipelines. Progress is now tracked in the
> [extension methods and lambda integration investigation](investigations/extension-lambda-integration.md).

