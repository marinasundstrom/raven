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

* **Real-world metadata gaps.** The infrastructure that recognises extension
  methods inside the metadata fixture still fails against the reference
  assemblies that ship with .NET. Running the CLI against
  `src/Raven.Compiler/samples/linq.rav` produces overload-resolution failures and
  misses `System.Linq.Enumerable` entirely, even though `Program.cs` adds
  `System.Linq.dll` by default.【F:src/Raven.Compiler/samples/linq.rav†L1-L18】【F:src/Raven.Compiler/Program.cs†L172-L188】 This
  suggests the metadata walker is either skipping the reference-assembly types
  or rejecting the generic receiver conversion when the declaring type comes
  from the BCL. Until the binder can surface those real assemblies, extension
  consumption only succeeds when callers reference the bespoke test fixture.

## Follow-up investigations

* Broaden semantic tests to cover overload shadowing and generic receivers for
  Raven-authored extensions so binder and lowering parity holds as syntax work
  resumes.【F:docs/compiler/design/extension-methods-plan.md†L6-L8】
* Continue documenting declaration support gaps so the consumption work stays in
  sync with upcoming syntax changes once extension modifiers become available.
