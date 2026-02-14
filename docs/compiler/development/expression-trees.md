# Expression Tree Support (Status)

This page tracks the current implementation status of Raven's `.NET` expression tree support.

## Scope

Current support is **stage 1**:

- Target-typed lambda conversion to `System.Linq.Expressions.Expression<TDelegate>`.
- Lowering from the already-bound lambda/body into `System.Linq.Expressions.Expression.*` factory calls.
- Initial overlap with common EF Core predicate shapes.

## Supported today

### Conversion and binding

- Lambda to `Expression<TDelegate>` conversion is recognized.
- The inner delegate shape is replayed/inferred through normal lambda binding rules.
- Existing non-expression-tree lambda binding remains in place.

### Lowering shape

Expression-tree lambda lowering currently builds:

- `Expression.Parameter`
- `Expression.Constant`
- `Expression.Convert`
- `Expression.PropertyOrField`
- `Expression.Lambda<TDelegate>`

### Supported body nodes/operators

- Parameter access
- Local access (as constants)
- Literals
- Instance field/property access
- Conversions
- Unary:
  - logical not (`!`)
  - unary minus (`-`)
- Binary:
  - arithmetic: `+`, `-`, `*`, `/`
  - equality: `==`, `!=`
  - relational: `>`, `>=`, `<`, `<=`
  - logical: `&&`, `||`

## Not yet supported

- Static member access in expression-tree lambdas
- Method-call lowering inside expression-tree lambdas
- Broader control-flow/body constructs (blocks, loops, rich conditionals, etc.)
- Full EF Core translation-oriented coverage

## Emission invariant

For lambdas converted to `Expression<TDelegate>`:

- A lambda symbol is still created for semantic analysis/replay/diagnostics.
- The final emitted IL should use `Expression.*` construction and **not** rely on a generated delegate lambda method for that expression-tree lambda.

Current safeguards:

- Binder marks expression-tree-targeted lambdas (`SourceLambdaSymbol.MarkExpressionTreeLambda`).
- Codegen has a fail-fast guard if a marked expression-tree lambda reaches delegate-lambda emission.
- Codegen test coverage asserts expression-tree-only source does not emit `<lambda_...>` methods.

## Tests in place

- Semantic lowering coverage:
  - `/Users/robert/Projects/Raven/test/Raven.CodeAnalysis.Tests/Semantics/ExpressionTreeLoweringTests.cs`
- Codegen/runtime coverage:
  - `/Users/robert/Projects/Raven/test/Raven.CodeAnalysis.Tests/CodeGen/ExpressionTreeCodeGenTests.cs`

## Sample playground

- `/Users/robert/Projects/Raven/samples/expression-trees.rav`

Note: this sample currently includes both plain delegate lambdas and expression-tree lambdas; plain delegate lambdas are still expected to emit `<lambda_...>` methods.
