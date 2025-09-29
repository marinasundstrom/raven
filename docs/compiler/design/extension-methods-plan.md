# Extension Method Support Plan

This document sketches an incremental path for bringing Raven's extension method
story to parity with C# while avoiding the MetadataLoadContext issues currently
observed when compiling LINQ-heavy samples.

> **Next step.** Once the failure mode is addressed, cover extension method
> calls inside query comprehensions (`Select`, `Where`, `OrderBy`) to ensure
> LINQ works end-to-end.

## 1. Baseline assessment ✅

* Documented the current binder, lowering, and emitter behavior for metadata
  extensions, and captured the gaps for Raven-authored declarations in
  `extension-methods-baseline.md` for future reference.【F:docs/compiler/design/extension-methods-baseline.md†L1-L36】【F:docs/compiler/design/extension-methods-baseline.md†L38-L44】
* Recorded the minimal LINQ sample and the exact CLI invocation that surfaces
  the `ExpressionGenerator.EmitLambdaExpression` failure, along with the bound
  tree snapshot that will guide upcoming tests.【F:docs/compiler/design/extension-methods-baseline.md†L46-L64】

## 2. Metadata consumer support (active)

1. ✅ Built a canonical metadata fixture that mirrors LINQ's core surface area
   (`Select`, `Where`, `OrderBy`, aggregation helpers) and exposed it through a
   Raven test reference so the same binaries drive CLI experiments and unit
   coverage.【F:test/MetadataFixtures/ExtensionMethodsFixture/RavenEnumerableExtensions.cs†L1-L219】【F:test/Raven.CodeAnalysis.Tests/TestMetadataReferences.cs†L1-L30】
2. ✅ Authored semantic tests that import the fixture via `using` directives and
   prove that `BlockBinder.LookupExtensionMethods` produces the expected method
   groups for array, `IEnumerable<T>`, and nullable receivers using the shared
   metadata reference.【F:test/Raven.CodeAnalysis.Tests/Semantics/MetadataExtensionMethodSemanticTests.cs†L1-L125】
3. ✅ Traced the invocation pipeline—binding, overload resolution, and type
   inference—for representative calls and captured the results in the metadata
   pipeline trace, confirming no metadata-only gaps before we attempt
   Raven-authored declarations.【F:docs/compiler/design/extension-methods-metadata-pipeline.md†L1-L33】
4. ✅ Completed the delegate replay work for lambda arguments. `GetTargetType`
   caches every viable delegate type, `BindLambdaExpression` records the
   candidates on both bound and unbound lambdas, and overload resolution replays
   the lambda against each extension overload. New semantic tests covering
   metadata `Enumerable.Where` and the Raven fixture confirm the pipeline binds
   without diagnostics.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1072-L1175】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2322-L2468】【F:test/Raven.CodeAnalysis.Tests/Semantics/MetadataExtensionMethodSemanticTests.cs†L305-L463】
5. ✅ Routed delegate construction through the metadata-aware helpers so
   `EmitLambdaExpression` reuses the compilation's metadata types when locating
   constructors and CLI builds stop throwing for extension-backed lambdas.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L393-L446】
   1. ✅ Resolve delegate constructors via the compilation's metadata helpers
      instead of calling `delegateType.GetConstructors()` directly.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L422-L438】
   2. ✅ Cache resolved constructors per delegate shape to avoid redundant
      reflection while we migrate to metadata-aware APIs.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L395-L438】
   3. 🔍 Investigate whether additional emission paths (e.g. captured lambdas)
      need similar treatment once the primary constructor lookup is fixed.
6. ✅ Validated the CLI path by compiling the LINQ sample against the fixture
   library and capturing both the `dotnet run` invocation and resulting
   diagnostics in the baseline doc. The run still stops in overload resolution,
   so the `ExpressionGenerator` regression remains unexercised until the binder
   accepts the metadata extension.【F:docs/compiler/design/extension-methods-baseline.md†L74-L86】
7. Exit criteria: metadata extensions behave like their C# counterparts in both
   semantic analysis and emitted IL, and remaining interop gaps are documented
   with linked follow-up issues.

## 3. Symbol and syntax work (deferred)

1. Once consumer scenarios are solid, extend the syntax tree to recognize
   `extension` modifiers on static methods in static classes, mirroring C#'s
   rules. Ensure the language specification and syntax nodes mirror the new
   grammar.
2. Update symbol creation so that methods declared with the new modifier surface
   as `IsExtensionMethod = true`, and so that their first parameter is marked as
   the receiver. Verify that Raven-authored extension methods live in metadata
   tables compatible with existing lookup logic.
3. 🔍 **Investigation.** The declaration pipeline is still wired for
   attribute-based extension detection. Bringing the keyword online will require
   coordinated updates:
   * The syntax generator lacks an `extension` keyword entry, so NodeGenerator
     will continue to reject the modifier until `Tokens.xml` grows the token and
     regenerated facts teach the parser about it.【F:src/Raven.CodeAnalysis/Syntax/Tokens.xml†L1-L120】
   * `SourceMethodSymbol.ComputeIsExtensionMethod` only flips the extension flag
     after finding an `[Extension]` attribute, which means declarations using a
     keyword would never flow into lookup until the modifier is recognized at
     binding time.【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L197-L233】
   * `Binder.GetExtensionMethodsFromScope` filters candidates strictly through
     `IsExtensionMethod`, so the symbol change above must land before newly
     declared helpers appear in method groups or overload resolution.【F:src/Raven.CodeAnalysis/Binder/Binder.cs†L187-L291】
   * The language specification still documents extension **consumption** only;
     once declarations exist we need to describe the syntax, grammar, and
     scoping rules alongside the existing invocation text.【F:docs/lang/spec/language-specification.md†L640-L670】
   * Code generation continues to locate delegate constructors via raw
     reflection (`delegateType.GetConstructors()`), so enabling source-defined
     extensions that lower through lambdas will require routing this path through
     the metadata-load-context aware helpers from Stage&nbsp;6 to avoid the
     existing `Type.GetConstructor` failures.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L421-L441】

## 4. Binding and overload resolution (active)

1. ✅ Populated method groups with Raven-authored extensions. `Binder.GetExtensionMethodsFromScope`
   now filters candidates by accessibility and receiver compatibility before
   yielding them, and `NamespaceBinder.LookupExtensionMethods` threads declared
   source types through the lookup so namespaces in scope surface both metadata
   and Raven-authored helpers.【F:src/Raven.CodeAnalysis/Binder/Binder.cs†L187-L274】【F:src/Raven.CodeAnalysis/Binder/NamespaceBinder.cs†L33-L61】
2. ✅ Merged instance and extension candidates predictably. The existing
   `BindMemberAccessExpression` pipeline already stitches instance and metadata
   extensions together; with the lookup changes above we now reuse that logic
   for source declarations and rely on `IsExtensionReceiver` to gate extension
   lookups to non-static receivers.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1946-L2001】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3044-L3079】
3. ✅ Carried the receiver through overload resolution and conversions. No code
   changes were required, but new coverage verifies that
   `OverloadResolver.ResolveOverload` continues to prefer instance methods over
   competing extensions and that `ConvertInvocationArguments` synthesizes the
   receiver argument for Raven-authored extensions.【F:src/Raven.CodeAnalysis/OverloadResolver.cs†L12-L200】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3924-L3987】【F:test/Raven.CodeAnalysis.Tests/Semantics/ExtensionMethodSemanticTests.cs†L547-L608】
4. ✅ Strengthened diagnostics and coverage. Added semantic tests that exercise
   Raven extensions declared in separate namespaces, verify missing imports keep
   them out of scope, and confirm instance methods still win when extensions
   compete for the same receiver shape.【F:test/Raven.CodeAnalysis.Tests/Semantics/ExtensionMethodSemanticTests.cs†L515-L608】

## 5. Lowering adjustments

1. ✅ Extend `Lowerer.RewriteInvocationExpression` to treat Raven-authored
   extensions identically to metadata-backed ones by substituting the receiver as
   the first argument to the lowered static call. Coverage now confirms lowering
   rewrites source-defined extensions into static calls with the receiver as the
   leading argument.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/Lowerer.Invocation.cs†L8-L29】【F:test/Raven.CodeAnalysis.Tests/Semantics/ExtensionMethodSemanticTests.cs†L540-L609】
2. ✅ Confirmed that lowered extension invocations preserve value-type boxing and
   reject nullable receivers where C# would. New lowering coverage exercises the
   boxed first argument and verifies we emit the diagnostic for `int?` receivers
   targeting non-nullable parameters.【F:test/Raven.CodeAnalysis.Tests/Semantics/ExtensionMethodSemanticTests.cs†L585-L644】
3. Prototype a lowering pass trace (behind a compiler flag) that logs when a
   call is rewritten as an extension dispatch. This will aid in validating that
   overload resolution selected the expected candidate during manual review.

## 6. Code generation fixes

1. ✅ Fix `ExpressionGenerator.EmitLambdaExpression` so that it resolves
   delegate constructors using `Compilation`'s `MetadataLoadContext`-aware
   APIs. Avoid `Type.GetConstructor` calls that introduce foreign `Type`
   instances.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L409-L482】
2. ✅ Hardened the delegate construction path by caching resolved constructors
   per delegate signature so repeated lambda emission no longer incurs redundant
   metadata lookups and the metadata-aware helpers are reused when emitting
   extension lambdas.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L14-L33】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L419-L524】
3. ✅ Added execution coverage that compiles and runs the LINQ sample through the
   CLI with the metadata fixture, exercising `Where`/`Select` lambdas and
   asserting the emitted program produces the projected results.【F:test/Raven.CodeAnalysis.Samples.Tests/SampleProgramsTests.cs†L66-L117】
4. ✅ Exercised the LINQ sample through a full query-style pipeline. The
   `linq.rav` program filters, sorts, and projects via the metadata-backed
   `Where`, `OrderBy`, and `Select` helpers, and the CLI integration test still
   expects the ordered `2, 4, 6` projection so query comprehension lowering is
   covered end to end.【F:src/Raven.Compiler/samples/linq.rav†L7-L19】【F:test/Raven.CodeAnalysis.Samples.Tests/SampleProgramsTests.cs†L87-L140】

## 7. Validation and polish

1. Expand semantic tests under `test/Raven.CodeAnalysis.Tests` to cover Raven
   extensions calling into other Raven extensions, overload shadowing, and
   generic receivers.
2. Document the feature in the language specification and user-facing manuals,
   emphasizing parity with C#'s extension method behavior and any deliberate
   deviations.
3. Update the TODO/roadmap once the end-to-end support is in place.
