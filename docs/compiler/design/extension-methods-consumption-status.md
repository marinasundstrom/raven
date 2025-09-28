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
* **Lambda delegate retention.** `GetTargetType` and `BindLambdaExpression`
  now cache every viable delegate candidate for lambda arguments, and overload
  resolution replays those lambdas so both metadata and fixture-backed `Where`
  overloads bind without diagnostics.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1072-L1175】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2322-L2468】【F:test/Raven.CodeAnalysis.Tests/Semantics/MetadataExtensionMethodSemanticTests.cs†L305-L463】
* **CLI references.** The command-line host unconditionally adds
  `System.Linq.dll` alongside the core runtime libraries, so LINQ extension
  methods are available without manually supplying reference switches.【F:src/Raven.Compiler/Program.cs†L172-L188】

## Active blockers

* **Metadata load context failure.** Even after binding succeeds, emitting
  lambdas that capture extension invocations will continue to fail until
  `ExpressionGenerator.EmitLambdaExpression` stops calling
  `Type.GetConstructor` directly and resolves delegate constructors through the
  compiler's metadata load context helpers.【F:docs/compiler/design/extension-methods-baseline.md†L17-L125】

## Next investigations

* Harden code generation by routing delegate construction through the
  metadata-aware helpers and adding execution tests that compile and invoke LINQ
  expressions end to end.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L403-L441】
* Add end-to-end coverage that exercises the CLI without extra `--refs`,
  proving metadata extensions continue to bind under command-line builds.【F:src/Raven.Compiler/Program.cs†L172-L188】
* Extend semantic tests to stress nested lambdas and query-like pipelines so the
  cached delegate logic keeps working across more complex extension chains.【F:test/Raven.CodeAnalysis.Tests/Semantics/MetadataExtensionMethodSemanticTests.cs†L305-L463】
