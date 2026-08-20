# Minimal macro DSL

This is the smallest project sample that parses a custom token DSL containing
an ordinary Raven expression. It is intended as the starting reference for
macro authors; the broader `macro-freestanding` query sample builds on the same
APIs with several clauses and introduced locals.

The application writes:

```raven
let shouldRetry = guard! {
    unless answer == 42
}
```

The outer grammar has one macro-local keyword, `unless`. Everything after that
keyword is an embedded Raven expression. The macro expands the invocation to
the ordinary Raven expression `!(answer == 42)`.

## The complete wiring

`macros/GuardMacro.rvn` demonstrates the four pieces required for this hybrid
DSL:

1. `IMacroKeywordProvider` gives `unless` a provider-owned token kind. The
   keyword exists only inside `guard!`; Raven's global grammar is unchanged.
2. `IMacroFragmentProvider` reports the body-relative condition span as
   `MacroFragmentKind.Expression`. The compiler maps it to the authored file,
   allowing normal expression highlighting, hover, completion, and future
   tooling inside the DSL without learning the guard grammar.
3. `stream.ParseExpression()` delegates to Raven's parser at the stream's
   current token, advances through the parsed expression, and returns both the
   recovered syntax and its body-relative span. Native diagnostics already
   point back to the invocation, so malformed conditions are reported where
   they were written. The sample then requires end-of-stream because its
   expression owns the rest of the body; a multi-clause DSL would continue
   reading its next keyword instead.
4. The parsed `condition.Syntax` is inserted into the generated prefix
   expression. Parsed executable fragments retain their authored origin for
   portable-PDB sequence points; the generated negation plumbing remains
   hidden while stepping.

The provider reports expected DSL mistakes with `CreateBodyDiagnostic` rather
than throwing. A larger DSL should normally parse its private grammar once and
share that result between `GetFragmentRegions` and `Expand`; the tiny repeated
keyword check here keeps the complete example readable in one file.

## Project shape

- `macros/MacroDslMacros.rvnproj` builds the compiler plugin.
- `macros/GuardMacro.rvn` exports the provider with
  `[assembly: RavenCompilerPlugin(...)]`.
- `app/MacroDslSample.rvnproj` references the macro project like an ordinary
  project dependency.
- `app/src/Main.rvn` invokes the DSL from application code.

Run it with:

```bash
dotnet run --project app/MacroDslSample.rvnproj --property WarningLevel=0
```

Expected output:

```text
42
False
```

## Planned macro-syntax parity

The explicit provider class is intentional for this reference because it
shows the contracts that currently carry keywords, fragments, diagnostics,
and expansion results. A later slice should let Raven's `macro` syntax
express this same example without requiring authors to default to the class
shape. That syntax must preserve all four capabilities above; reducing the
boilerplate must not remove source mapping or editor integration.

Compiler analyzers should eventually treat fragment-only references as normal
uses as well. Until that compiler-wide integration is complete, the sample
also prints `answer` outside the macro so the application remains warning-free.
