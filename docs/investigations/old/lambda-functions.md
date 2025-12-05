# Lambda functions investigation

This note captures the current state of lambda expression support in the Raven
compiler and highlights the remaining work needed to make them executable.

## Syntax

* The syntax model exposes an abstract `LambdaExpressionSyntax` with concrete
  `SimpleLambdaExpressionSyntax` and `ParenthesizedLambdaExpressionSyntax`
  shapes in `Model.xml`. Both forms now omit a leading keyword; they share the
  `=>` arrow token and an expression body slot. The simple form stores a single
  `Parameter` node while the parenthesized form reuses the `ParameterList`
  structure used by function declarations.【F:src/Raven.CodeAnalysis/Syntax/Model.xml†L479-L504】【F:src/Raven.CodeAnalysis/Syntax/Model.xml†L632-L656】
* `ExpressionSyntaxParser.ParseFactorExpression` speculatively interprets either
  a parenthesized parameter list or a single identifier as the start of a
  lambda. The parser delegates parenthesized parameters to
  `StatementSyntaxParser.ParseParameterList`, parses the simple form inline, and
  consumes any optional return type before expecting the `=>` token and body.
  This flow now constructs both `SimpleLambdaExpressionSyntax` and
  `ParenthesizedLambdaExpressionSyntax` nodes depending on the source.
* Because blocks are first-class expressions, a lambda body can be either a
  single expression or a braced block expression, letting the binder reuse the
  regular expression binding pipeline.

## Binding and symbols

* `BlockBinder.BindLambdaExpression` performs the core semantic work. It expands
  the parameter list into `SourceParameterSymbol` instances, tracking `ref` and
  `out` modifiers, resolves any explicit return type, and creates a
  `SourceLambdaSymbol` placeholder before binding the body. The body is bound in
  a dedicated `LambdaBinder`, which inherits `BlockBinder` to reuse local-scope
  management.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L547-L606】
* Contextual delegate types now flow into the binder. `BlockBinder.GetTargetType`
  supplies the expected delegate when a lambda appears in an assignment,
  argument, or return position. Parameters without explicit annotations borrow
  their types (and ref kinds) from that delegate, and the body is converted to
  the delegate's return type when necessary. If no delegate context exists, the
  binder reports `RAV2200` so callers add explicit parameter annotations before
  inference proceeds.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L569-L702】【F:src/Raven.CodeAnalysis/CompilerDiagnostics.g.cs†L912-L940】
* Return types default to `Compilation.ErrorTypeSymbol` until the body finishes
  binding. Afterwards the binder uses `ReturnTypeCollector.Infer` to aggregate
  the types flowing out of the body (explicit `return`s plus the final
  expression) and reconciles the inference with any annotation. Mismatches emit
  a conversion diagnostic and the body is converted if needed before the symbol
  is updated.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L610-L637】【F:src/Raven.CodeAnalysis/Binder/ReturnTypeCollector.cs†L8-L73】
* `Compilation.CreateFunctionTypeSymbol` translates the final signature into an
  appropriate `System.Func<>` or `System.Action<>` delegate by selecting the
  overload whose arity matches the parameter count (plus a return type for
  `Func`). The chosen delegate is stored on the lambda symbol and copied into the
  `BoundLambdaExpression`.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L637-L654】【F:src/Raven.CodeAnalysis/Compilation.cs†L955-L1005】
* `SourceLambdaSymbol` implements `ILambdaSymbol` and keeps the computed return
  type, parameter list, and delegate type. The symbol is marked as a static
  method whose `MethodKind` is `LambdaMethod`, matching Roslyn’s shape for
  compiler-generated lambdas.【F:src/Raven.CodeAnalysis/Symbols/Source/SourceLambdaSymbol.cs†L6-L48】

## Captured variable analysis

* `LambdaBinder` records its parameters and locals so that lookups prioritize
  lambda-scoped declarations. After binding, `CapturedVariableWalker.Analyze`
  walks the bound body to find locals, parameters, and `self` references defined
  outside the lambda and reports them through the
  `BoundLambdaExpression.CapturedVariables` property. The walker’s findings are
  persisted on the `SourceLambdaSymbol`, allowing downstream stages to determine
  when closure materialization is required.【F:src/Raven.CodeAnalysis/Binder/LambdaBinder.cs†L1-L55】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceLambdaSymbol.cs†L6-L72】
* The captured-variable information now feeds code generation, guiding the
  creation of closure classes when non-local state is accessed.

## Code generation status

* `ExpressionGenerator.EmitExpression` now supports both capturing and
  non-capturing lambdas. Non-capturing lambdas continue to lower to static
  helper methods and `ldftn` delegate construction. When captures exist, the
  generator asks `TypeGenerator` to synthesize a nested closure type whose
  fields mirror the captured symbols. The outer method allocates the closure,
  copies the current values into the fields, and produces a delegate via
  `Delegate.CreateDelegate` so the closure instance becomes the bound receiver
  for the generated static helper.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L24-L223】【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L12-L195】
* Captured locals now live directly on the synthesized closure instance. The
  binder still reports captures on the lambda symbol, and the emitters translate
  each capture into a field on the closure class. Assignments and reads in either
  scope operate on those fields so mutations are immediately visible to all
  delegates.
* Nested lambdas reuse the closure instances created by their enclosing scopes.
  When a nested lambda captures a variable owned by a parent closure, the
  emitter now threads the existing closure field into the inner closure rather
  than materializing a new storage location, ensuring all delegates observe the
  same underlying state.

## Investigation: simple lambda syntax

The syntax model already exposes both `SimpleLambdaExpressionSyntax` and
`ParenthesizedLambdaExpressionSyntax`, and the parser now constructs either
shape depending on the tokens it encounters. When a lambda omits parentheses,
`ParseFactorExpression` parses a single `Parameter` node inline and produces a
`SimpleLambdaExpressionSyntax`. Parenthesized forms continue to route through
`StatementSyntaxParser.ParseParameterList`, so both shapes share the same
parameter and optional return-type parsing helpers.

The remainder of the pipeline handles both forms uniformly. `BlockBinder.BindLambdaExpression`
and the replay helpers enumerate the parameter syntax via pattern matching, so
either shape flows through binding and delegate rebinding without additional
changes.

## Recent commit review

Examining the latest ten commits on the `work` branch shows no functional
changes to the lambda pipeline:

* `9eed2740` (**Expand operations coverage (#771)**) broadens operation tests
  without touching lambda binding, lowering, or emission paths.
* `647bfce7` (**Fix data flow for global statements (#770)**) amends flow
  analysis in global statement handling only.
* `49eca8f1` (**Expand lambda investigation notes (#769)**) updates this
  document but does not alter compiler behaviour.
* `696b5e7c` (**Handle nullable type member lookups (#768)**) narrows symbol
  lookup for nullable member access.
* `0bcb1769` (**Allow fully qualified types in casts (#767)**) relaxes parsing
  for cast targets.
* `39e1afb6`, `a9f98e33`, `0a7e926e`, and `2ec3c4ee` adjust sample programs
  (`test.rav`, `reflection.rav`) without impacting lambda semantics.
* `8a84d91b` (**Support explicit reference casts (#766)**) adds reference-cast
  support only.

Because none of these commits modify the binder, overload resolver, or code
generation layers responsible for lambdas, the outstanding work described below
remains accurate.

## Investigation: validating the simple form

Once the parser constructs `SimpleLambdaExpressionSyntax`, the existing test
suite needs to be broadened so the new syntax is exercised end-to-end.

* Semantic tests such as `LambdaCapturedVariablesTests` and
  `LambdaInferenceTests` currently grab parenthesized lambdas directly from the
  syntax tree, e.g. by filtering for
  `SyntaxKind.ParenthesizedLambdaExpression`. After the parser change these
  scenarios should be duplicated (or converted) to assert that
  `SimpleLambdaExpressionSyntax` binds, infers delegate parameters, and reports
  captures correctly.【F:test/Raven.CodeAnalysis.Tests/Semantics/LambdaCapturedVariablesTests.cs†L25-L47】【F:test/Raven.CodeAnalysis.Tests/Semantics/LambdaInferenceTests.cs†L35-L100】
* The code-generation coverage in `LambdaCodeGenTests` also instantiates lambdas
  exclusively with parentheses. Adding equivalents that use the shorthand form
  will verify closure emission, delegate materialization, and nested lambda
  handling once the parser can produce the simple syntax.【F:test/Raven.CodeAnalysis.Tests/CodeGen/LambdaCodeGenTests.cs†L13-L120】【F:test/Raven.CodeAnalysis.Tests/CodeGen/LambdaCodeGenTests.cs†L189-L268】
* Parser-focused tests should assert that `value => value + 1` yields a
  `SimpleLambdaExpressionSyntax`. The grammar already models the node shape, so
  a targeted syntax test will guard against regressions that reintroduce the
  parenthesized-only behaviour.

With these additions in place the parser work will be observable across the
syntax, binding, and emission layers.

## Next steps

1. Harden the speculative parser by covering ambiguous parenthesized forms—such
   as casts versus lambdas—with targeted tests so the rollback logic stays
   resilient.
2. Expand semantic, syntax, and code-generation tests to include combinations of
   optional return types, attributes, and default values in the shorthand form
   to ensure all parameter features work without parentheses.
