# Extension Method Consumption Status

## What already works

* **Method group formation.** `BlockBinder` merges instance methods with
  matching extensions when the receiver is eligible, ensuring the bound method
  group keeps track of the synthetic extension receiver for later stages.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1988-L2013】
* **Invocation representation and lowering.** `BoundInvocationExpression`
  captures both the syntactic receiver and the extension receiver placeholder so
  the lowerer can rewrite extension calls into static invocations by inserting
  the receiver as the first argument.【F:src/Raven.CodeAnalysis/BoundTree/BoundInvocationExpression.cs†L5-L30】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/Lowerer.Invocation.cs†L8-L29】
* **Metadata fixtures and semantic coverage.** The test reference bundle exposes
  a LINQ-inspired fixture, and semantic tests confirm that enumerable, array, and
  nullable receivers all select the metadata-backed extensions and surface them
  as extension invocations.【F:test/Raven.CodeAnalysis.Tests/TestMetadataReferences.cs†L10-L29】【F:test/Raven.CodeAnalysis.Tests/Semantics/MetadataExtensionMethodSemanticTests.cs†L11-L149】

## Active blockers

* **Lambda target retention.** When a lambda argument participates in an
  overloaded extension call, `GetTargetType` still gives up as soon as more than
  one candidate survives, so the lambda binder produces `RAV2200` instead of
  caching the delegate shapes for overload resolution to replay.【F:docs/compiler/design/extension-methods-baseline.md†L65-L121】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2141-L2204】
* **CLI reference gaps.** The command-line host keeps the `System.Linq`
  reference commented out, so consuming metadata extensions still requires
  manually supplying the LINQ assembly when running samples.【F:docs/compiler/design/extension-methods-baseline.md†L34-L71】
* **Metadata load context failure.** Even after binding succeeds, emitting
  lambdas that capture extension invocations will continue to fail until
  `ExpressionGenerator.EmitLambdaExpression` stops calling
  `Type.GetConstructor` directly and resolves delegate constructors through the
  compiler's metadata load context helpers.【F:docs/compiler/design/extension-methods-baseline.md†L17-L125】

## Next investigations

* Teach lambda binding to cache every viable delegate candidate, suppressing
  premature diagnostics while overload resolution decides which extension wins.
* Re-enable the LINQ reference in the CLI or flow it from the target framework
  resolver so metadata extensions are available without manual switches.
* Harden code generation by routing delegate construction through the
  metadata-aware helpers and adding execution tests that compile and invoke LINQ
  expressions end to end.
