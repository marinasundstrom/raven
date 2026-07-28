# Macro Freestanding (`.rvnproj`)

This sample shows Raven-authored freestanding expression macros, including a
minimal token-tree DSL macro.

The sample shape is:

```raven
func Main() -> unit {
    val answer = #add(20, Right: 22)
    val shouldRetry = guard! {
        unless answer == 42
    }
    val verdict = choose! {
        test answer == 42
        then "correct"
        otherwise "wrong"
    }
    val queryResult = query! {
        from value in [1, 2, 3, 4]
        where value > 2
        select value * 10
    }
    val quoted = quote! {
        #(Raven.CodeAnalysis.Syntax.SyntaxFactory.IdentifierName("answer")) + 1
    }

    WriteLine(answer)
    WriteLine(shouldRetry)
    WriteLine(verdict)
    WriteLine(queryResult.Sum())
    WriteLine(quoted.ToString())
}
```

Current status:

- The macro definitions are written in Raven, not C#.
- The provider explicitly exports each macro definition with repeatable
  `[assembly: RavenCompilerPlugin(typeof(...))]` attributes, and the
  application consumes it through an ordinary `ProjectReference`. The SDK
  classifies the marked provider as a compiler plugin without a
  consumer-authored `RavenMacro` item or plugin container.
- `#add` uses the compiler-owned `quote!` intrinsic inside the Raven-authored
  macro implementation. Its two argument expressions are inserted with
  `#(...)` holes, producing `left + right` without manually assembling the
  infix syntax tree.
- The expansion reuses the original argument expression syntax and still
  returns an ordinary `ExpressionSyntax`.
- The sample uses a named argument to show the current freestanding macro argument shape.
- `guard! { unless ... }` is the token-tree MVP: Raven's standard macro token
  stream recognizes `unless` as a body-scoped macro keyword, the macro delegates
  the remaining span to Raven's expression parser, and expansion produces the
  ordinary Raven expression `!(...)`.
- The MVP deliberately lowers directly from the token stream. It does not build
  a custom DSL syntax tree.
- `choose!` extends that approach to three clauses. It uses `test`, `then`, and
  `otherwise` as macro-local reserved words, parses the text between them as
  three independent Raven expressions, and lowers directly to an ordinary
  Raven `if` expression.
- `query!` is the first LINQ-like MVP. It supports one `from` clause, an
  optional `where`, and one `select`. The authored range variable becomes the
  parameter of generated `Where` and `Select` lambdas, while the source,
  predicate, and projection remain independently parsed Raven expressions.
- The query uses `ParseExpressionResult` so recovered syntax and native Raven
  parser diagnostics stay together. The first invalid embedded fragment is
  forwarded with its authored source location.
- The query MVP generates no hidden temporary names and retains no custom DSL
  tree. Additional generators, repeated clauses, ordering, joins, and editor
  services remain future work.
- `quote!` is the compiler-owned expression-only quote MVP. It needs no plugin
  registration, preserves the quoted expression's tokens and trivia, and
  expands to ordinary fully qualified `SyntaxFactory` calls. Because the result
  is a runtime syntax object, the compiler adds the matching
  `Raven.CodeAnalysis` runtime dependency on demand. Its hole inserts an
  `ExpressionSyntax` constructed by an ordinary caller-bound Raven expression.

Files:

- `app/MacroFreestanding.rvnproj`: Raven application using `#add(...)`
- `app/src/Main.rvn`: executable entry point
- `macros/FreestandingMacros.rvnproj`: Raven macro plugin project
- `macros/FreestandingMacros.rvn`: related implementations of the category-specific macro interfaces

Analyze, build, or run the executable sample project. Its normal project
reference builds and activates the marked macro provider:

```bash
dotnet run --framework net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- app/MacroFreestanding.rvnproj --no-emit
```

```bash
dotnet run --project app/MacroFreestanding.rvnproj --property WarningLevel=0
```

Expected output:

```text
42
False
correct
70
answer + 1
```
