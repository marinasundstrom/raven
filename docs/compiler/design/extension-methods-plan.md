# Extension Method Support Plan

This document sketches an incremental path for bringing Raven's extension method
story to parity with C# while avoiding the MetadataLoadContext issues currently
observed when compiling LINQ-heavy samples.

> **Next step.** Unblock Stage&nbsp;2 by teaching lambda binding to keep every
> viable delegate candidate even when overload resolution is ambiguous, then
> replay those lambdas during overload resolution. Completing this work enables
> the LINQ samples in `src/Raven.Compiler/samples/linq.rav` to progress past the
> current `RAV1501` and `RAV2200` diagnostics and lets us continue validating
> metadata-backed extensions. The binder work is the remaining blocker before we
> can continue iterating on metadata-backed extension scenarios.

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
4. 🛑 Blocker: teach lambda binding to surface delegate shapes even when overload
   resolution has multiple candidates. Running
   `src/Raven.Compiler/samples/linq.rav` still reports `RAV1501` and `RAV2200`,
   so the delegate replay work has not landed in the CLI yet.【395e77†L1-L7】
   1. 🛠️ Extend `GetTargetType` so that lambda arguments keep every viable
      delegate candidate even when overload resolution hasn't picked a winner,
      allowing ambiguous extension groups to feed `BindLambdaExpression` the
      full delegate set.
   2. 🛠️ Update `BindLambdaExpression` so lambdas capture every candidate
      delegate, stash suppressed `RAV2200` diagnostics, and surface a
      `BoundUnboundLambda` payload that overload resolution can replay in the
      next step.
   3. 🛠️ Replay lambda binding across overload candidates.
      1. 🛠️ Adjust overload resolution and delegate conversions to rebind
         lambdas under each candidate delegate, mirroring Roslyn's
         `UnboundLambda` pipeline while preserving suppression behavior.
      2. 🛠️ Capture lambda replay perf counters so we can monitor cache hit
         rates, rebind attempts, and success ratios while iterating on
         multi-pass binding behavior.
   4. 🛠️ Capture semantic tests against both `System.Linq.Enumerable.Where` and
      the Raven LINQ fixture to prove implicit lambda parameters bind without
      diagnostics.
   5. 📐 Add a binder integration test that covers nested lambdas (e.g. `Select`
      with a trailing `Where`) to ensure delegate replay composes.
5. Validate end-to-end lowering/execution by compiling a LINQ-heavy sample with
   the fixture library and recording whether the `ExpressionGenerator` failure
   is still reachable when we stay within metadata-produced extensions. Capture
   the command-line invocation and emitted IL diff in the baseline document for
   traceability.
6. Exit criteria: metadata extensions behave like their C# counterparts in both
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
2. Confirm that the lowered invocation obeys value-type boxing semantics and
   nullability checks that C# enforces.
3. Prototype a lowering pass trace (behind a compiler flag) that logs when a
   call is rewritten as an extension dispatch. This will aid in validating that
   overload resolution selected the expected candidate during manual review.

## 6. Code generation fixes

1. Fix `ExpressionGenerator.EmitLambdaExpression` so that it resolves delegate
   constructors using `Compilation`'s `MetadataLoadContext`-aware APIs. Avoid
   `Type.GetConstructor` calls that introduce foreign `Type` instances.
2. Harden the delegate construction path by caching resolved constructors per
   `DelegateTypeSymbol` so repeated lambda emission does not incur redundant
   reflection or leak metadata handles.
3. Add targeted tests that compile and execute lambdas capturing extension
   invocations. Use `ilspycmd` to inspect the generated IL and ensure it mirrors
   the C# compiler's lowering.
4. Once the failure mode is addressed, cover extension method calls inside query
   comprehensions (`Select`, `Where`, `OrderBy`) to ensure LINQ works end-to-end.

## 7. Validation and polish

1. Expand semantic tests under `test/Raven.CodeAnalysis.Tests` to cover Raven
   extensions calling into other Raven extensions, overload shadowing, and
   generic receivers.
2. Document the feature in the language specification and user-facing manuals,
   emphasizing parity with C#'s extension method behavior and any deliberate
   deviations.
3. Update the TODO/roadmap once the end-to-end support is in place.
