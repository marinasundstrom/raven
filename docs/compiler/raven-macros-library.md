# Raven Macro Library

`Raven.Macros` is the standard compiler-plugin library distributed with Raven.
It contains reusable macros such as `quote`, `compile`, `timer`, and the
attached `Error` macro without making them intrinsic compiler declarations or
members of `Raven.Core`. These macros demonstrate the broader purpose of the
feature: concise forms can hide repetitive or domain-specific expansion code
and grow into DSLs that integrate naturally with Raven.

The standard library is also a proving ground rather than a permanent home for
every useful DSL. A macro family can move into its own package when it needs an
independent API, dependency set, compatibility policy, or release cadence;
Raven's carrier and application model remains the same.

Applications opt into the short aliases by importing the macro namespace:

```raven
import Raven.Macros.*

let syntax = quote! {
    left + right
}
```

The canonical names `Raven.Macros.Quote!` and `Raven.Macros.Compile!` remain
available when an alias is shadowed or the wildcard namespace is not imported.
Merely referencing the standard library does not place its aliases in lexical
scope.

`#[Error]` derives Raven's ordinary `System.IError` interface for a union. It
adds default `Message` and `Cause` properties only when the union does not
already declare them:

```raven
import Raven.Macros.*

#[Error]
union ParseError {
    #[ErrorMessage("Invalid value: $value")]
    case InvalidValue(value: string)

    #[ErrorMessage("A value is required")]
    case MissingValue
}
```

`ErrorMessage` accepts Raven expression syntax. In the common string form,
ordinary Raven interpolation can refer to the payload names of that case; no
macro-specific formatting language is involved. A case without
`ErrorMessage` falls back to the union's normal case-aware string
representation. Its validation is implemented in Raven: the attached macro
inspects the authored expression and containing union, reports diagnostics for
invalid use, and returns an empty expansion because `Error` consumes the
annotation when it derives the union implementation.

Conceptually, the macros above expand to ordinary Raven code:

```raven
union ParseError: System.IError {
    case InvalidValue(value: string)
    case MissingValue

    val Message: string => self match {
        InvalidValue(let value) => "Invalid value: $value"
        MissingValue => "A value is required"
        _ => self.ToString()
    }

    val Cause: System.IError? => null
}
```

The expansion shown here explains the behavior rather than promising an exact
lowered syntax-tree shape. `Error` adds `Message` or `Cause` only when the union
does not already declare that property, so an authored implementation always
takes precedence. `ErrorMessage` is valid only on a case nested in an
`#[Error]` union and accepts a string literal or interpolated string.

## `timer!`

`timer!` removes the usual `Stopwatch` setup and cleanup around a block of Raven
statements:

```raven
import Raven.Macros.*

timer! {
    let index = LoadIndex()
    Rebuild(index)
    Save(index)
}
```

Conceptually, it expands to the following boilerplate:

```raven
{
    let __stopwatch = System.Diagnostics.Stopwatch.StartNew()
    try {
        {
            let index = LoadIndex()
            Rebuild(index)
            Save(index)
        }
    }
    finally {
        __stopwatch.Stop()
        System.Console.WriteLine(__stopwatch.Elapsed)
    }
}
```

The macro parses its token body as an ordinary Raven block, preserves that
block's lexical scope, and emits the elapsed duration after the body finishes.
It also publishes the body as a block fragment, so hover and other ordinary
Raven editor features remain available within the braces.
It uses `try`/`finally` so timing also stops when control leaves the body early.
The actual stopwatch name is generated to avoid collisions; this expansion is
illustrative rather than an exact syntax-tree contract. Release builds report
`TIMER002` to make accidental instrumentation visible.

## `query!`

`query!` is a small LINQ-style token-tree DSL included in `Raven.Macros`. It
supports one `from` clause, an optional `where` clause, and one `select` clause:

```raven
import Raven.Macros.*

let projected = query! {
    from value in [1, 2, 3, 4]
    where value > 2
    select value * 10
}
```

The macro expands to ordinary `Where` and `Select` calls with Raven lambdas.
The source, predicate, and projection are parsed as ordinary Raven expression
fragments. The range variable is projected into those fragments for completion
and hover, so editor support does not depend on a query-specific syntax tree.

The current query macro is intentionally small. It demonstrates how a reusable
DSL can combine custom tokens, Raven expression fragments, introduced locals,
diagnostics, and ordinary generated code without adding query syntax to the
language grammar.

## Authoring model

The project under `src/Raven.Macros` is written in Raven. Each public macro has
its own source file, uses `macro`, and carries Markdown documentation
comments. `AssemblyInfo.rvn` marks the output as a compiler plugin:

```raven
[assembly: RavenCompilerPlugin]
```

Standard macro implementations move into this Raven project incrementally
instead of depending indefinitely on implementation helpers in the compiler.
Each port is deliberate language and API dogfooding: awkward
syntax construction, missing semantic operations, hidden compiler hooks, or a
required C# escape hatch should be treated as evidence of a Raven compiler or
macro-authoring problem to diagnose and improve. `timer` is implemented wholly
in `Raven.Macros`. `embedFileContent` also runs wholly from Raven and uses the
public dependency-tracked file-reading API. `sha256Digest` consumes the public
constant information already carried by its `MacroArgument`. The `Error` and
`ErrorMessage` pair also run wholly from Raven: they inspect attached syntax,
report diagnostics, rewrite a base list, and introduce generated properties.
The remaining older standard macros are migration work.

When a marked Raven library contains macro declarations, emission
lowers those declarations into reusable provider types and includes them in the
plugin assembly. This is the same general assembly-plugin mechanism available
to other Raven macro libraries; the standard macros do not require a built-in
macro category.

## Dependency and compatibility model

Macro implementations program against `Raven.CodeAnalysis`, so a macro library
has an ordinary dependency on a particular compiler-contract version. The
compiler loads the macro provider with its dependency closure and the consuming
compilation receives metadata references from that closure. Runtime
dependencies are copied only when the emitted application actually references
them.

Macro authors are responsible for building against a `Raven.CodeAnalysis`
version compatible with the compiler host that loads the plugin. The loader
must reject incompatible contracts with a diagnostic rather than allowing a
late type-load or invocation failure.

## Current implementation boundary

`Quote.rvn` owns expression parsing, splice recognition, source-mapped
diagnostics, and syntax-factory rendering through public macro and syntax APIs.
`Compile.rvn` reuses that Raven-authored quote operation before constructing
the runtime-compilation call. `Error.rvn`, `ErrorMessage.rvn`, and the other
standard macros are likewise implemented wholly in Raven. The compiler owns
the reusable contracts—position-preserving projected parsing, source-aware
diagnostics, and `RavenQuoter`—rather than the policy of individual standard
macros.

## Documentation publishing

The Raven.Macros project emits both XML documentation and Markdown documentation
artifacts. The documentation build publishes the standalone
[Raven.Macros API reference](https://marinasundstrom.github.io/raven/libraries/raven-macros/)
generated by RavenDoc alongside the
[Raven.Core API reference](https://marinasundstrom.github.io/raven/libraries/raven-core/).
These remain independent RavenDoc sites within the same published Pages
artifact as the language documentation and Playground.

Macro authors may cross the boundary into `Raven.CodeAnalysis`. Use the
[macro authoring guide](../macro-authoring.md) for the supported public
workflow. Compiler API implementation material remains in the repository's
development documentation.
