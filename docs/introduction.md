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
func describe(input: string | int | null) -> string {
    match input {
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

System.Console.WriteLine(summary)
```

Because string interpolation uses `${...}` inside ordinary quotes, the final line prints `Saw "Raven"; Counted 3; Nothing to report.`. Flow-sensitive analysis ensures that `text` and `number` have the right types inside each `match` arm, and the `_` discard keeps the expression exhaustive without introducing a binding.

