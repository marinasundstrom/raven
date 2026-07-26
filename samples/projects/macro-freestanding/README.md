# Macro Freestanding (`.rvnproj`)

This sample shows Raven-authored freestanding expression macros, including a
minimal token-tree DSL macro.

The sample shape is:

```raven
func Main() -> unit {
    val answer = #add(20, Right: 22)
    val shouldRetry = #guard {
        unless answer == 42
    }
    val verdict = #choose {
        test answer == 42
        then "correct"
        otherwise "wrong"
    }
    val queryResult = #query {
        from value in [1, 2, 3, 4]
        where value > 2
        select value * 10
    }

    WriteLine(answer)
    WriteLine(shouldRetry)
    WriteLine(verdict)
    WriteLine(queryResult.Sum())
}
```

Current status:

- The macro plugin is written in Raven, not C#.
- `#add(...)` is resolved from a `RavenMacro` project reference.
- The plugin expands structurally with the syntax API instead of parsing a generated expression string.
- The expansion reuses the original argument expression syntax when it builds the final `left + right` expression.
- The sample uses a named argument to show the current freestanding macro argument shape.
- `#guard { unless ... }` is the token-tree MVP: Raven's standard macro token
  stream recognizes `unless` as a body-scoped macro keyword, the macro delegates
  the remaining span to Raven's expression parser, and expansion produces the
  ordinary Raven expression `!(...)`.
- The MVP deliberately lowers directly from the token stream. It does not build
  a custom DSL syntax tree.
- `#choose` extends that approach to three clauses. It uses `test`, `then`, and
  `otherwise` as macro-local reserved words, parses the text between them as
  three independent Raven expressions, and lowers directly to an ordinary
  Raven `if` expression.
- `#query` is the first LINQ-like MVP. It supports one `from` clause, an
  optional `where`, and one `select`. The authored range variable becomes the
  parameter of generated `Where` and `Select` lambdas, while the source,
  predicate, and projection remain independently parsed Raven expressions.
- The query MVP generates no hidden temporary names and retains no custom DSL
  tree. Additional generators, repeated clauses, ordering, joins, and editor
  services remain future work.

Files:

- `app/MacroFreestanding.rvnproj`: Raven application using `#add(...)`
- `app/src/main.rvn`: executable sample
- `macros/FreestandingMacros.rvnproj`: Raven macro plugin project
- `macros/main.rvn`: plugin implementation of `IRavenMacroPlugin` / `IFreestandingExpressionMacro`

Build the macro plugin first:

```bash
dotnet build macros/FreestandingMacros.rvnproj --property WarningLevel=0
```

Then analyze, build, or run the executable sample project:

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
```
