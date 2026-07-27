# Expression Tree Support (Status)

This page tracks the current implementation status of Raven's `.NET` expression tree support.

## Meaning

An expression tree is a typed operation graph represented as a runtime object.
It sits between an ordinary behavioral API and reflection:

- A delegate gives an API executable behavior but not a structure it can
  inspect.
- An expression tree gives an API a structured expression it can inspect,
  rewrite, translate, or compile.
- Reflection discovers already-emitted types, members, and metadata rather than
  preserving the operation supplied at a particular call site.

Expression trees are also different from macros. Macros transform program
syntax during compilation. Expression trees use a language-integrated
conversion: when a supported language construct is converted to an
expression-tree type, the compiler constructs the corresponding
`System.Linq.Expressions` operation objects. Raven currently supports this
conversion for target-typed lambdas. LINQ providers and EF Core use those
runtime objects to translate predicates and projections into query languages
such as SQL. A typed `Expression<TDelegate>` can also be compiled into a
`TDelegate` and executed by the program.

Expression trees are not Raven syntax trees and do not preserve syntax.
`Raven.CodeAnalysis` represents the language's authored and semantic structure,
including Raven syntax nodes, symbols, types, diagnostics, and operations.
`System.Linq.Expressions` provides a standardized .NET abstraction over
particular executable concepts such as parameters, constants, member access,
calls, operators, and lambdas. Conversion to an expression tree produces only
those operation objects. It carries no Raven syntax nodes, tokens, trivia, or
original source form.

The expression-tree API is intentionally not a complete model of Raven, C#,
Visual Basic, or any other .NET language. It is a shared operation vocabulary
covering common programming concepts plus .NET-specific concepts. Any language
construct without a valid representation in that vocabulary cannot be
converted directly to an expression tree.

Use `Raven.CodeAnalysis` when a tool needs to understand Raven source or
compiler semantics. Use an expression tree when a runtime API needs a portable,
inspectable representation of an operation.

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
- Codegen test coverage asserts expression-tree-only source does not emit delegate lambda methods (`<Method>b__...`).

## Tests in place

- Semantic lowering coverage:
  - `/Users/marina/Projects/Raven/test/Raven.CodeAnalysis.Tests/Semantics/ExpressionTreeLoweringTests.cs`
- Codegen/runtime coverage:
  - `/Users/marina/Projects/Raven/test/Raven.CodeAnalysis.Tests/CodeGen/ExpressionTreeCodeGenTests.cs`

## Sample playground

- `/Users/marina/Projects/Raven/samples/runtime/expression-trees-basic.rav`

Note: this sample currently includes both plain delegate lambdas and expression-tree lambdas; plain delegate lambdas are still expected to emit `<Method>b__...` methods.
