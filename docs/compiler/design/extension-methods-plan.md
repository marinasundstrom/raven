# Extension Method Support Plan

This document sketches an incremental path for bringing Raven's extension method
story to parity with C# while avoiding the MetadataLoadContext issues currently
observed when compiling LINQ-heavy samples.

## 1. Baseline assessment ✅

* Documented the current binder, lowering, and emitter behavior for metadata
  extensions, and captured the gaps for Raven-authored declarations in
  `extension-methods-baseline.md` for future reference.【F:docs/compiler/design/extension-methods-baseline.md†L1-L36】【F:docs/compiler/design/extension-methods-baseline.md†L38-L44】
* Recorded the minimal LINQ sample and the exact CLI invocation that surfaces
  the `ExpressionGenerator.EmitLambdaExpression` failure, along with the bound
  tree snapshot that will guide upcoming tests.【F:docs/compiler/design/extension-methods-baseline.md†L46-L64】

## 2. Metadata consumer support (active)

1. Build a canonical metadata fixture that mirrors LINQ's core surface area
   (`Select`, `Where`, `OrderBy`, aggregation helpers) and expose it through a
   Raven test reference so the same binaries drive CLI experiments and unit
   coverage.
2. Author semantic tests that import the fixture via `using` directives and
   prove that `BlockBinder.LookupExtensionMethods` produces the expected method
   groups for common receivers (arrays, `IEnumerable<T>`, nullable structs).
3. Trace the invocation pipeline—binding, overload resolution, and type
   inference—for representative calls and capture any metadata-only gaps before
   we attempt Raven-authored declarations.
4. Validate end-to-end lowering/execution by compiling a LINQ-heavy sample with
   the fixture library and recording whether the `ExpressionGenerator` failure
   is still reachable when we stay within metadata-produced extensions.
5. Exit criteria: metadata extensions behave like their C# counterparts in both
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

## 4. Binding and overload resolution

1. Teach the binder to include Raven-authored extension methods in method groups
   once the new syntax is available. This requires scoping rules similar to C#:
   only methods in imported namespaces and static containers should be
   considered.
2. Update overload resolution so that the implicit receiver argument
   participates in type argument inference and accessibility checks in the same
   way as native instance methods.
3. Strengthen diagnostics around ambiguous extension lookups and missing `using`
   imports to match the behavior testers expect from C#.

## 5. Lowering adjustments

1. Extend `Lowerer.RewriteInvocationExpression` to treat Raven-authored
   extensions identically to metadata-backed ones by substituting the receiver as
   the first argument to the lowered static call.
2. Confirm that the lowered invocation obeys value-type boxing semantics and
   nullability checks that C# enforces.

## 6. Code generation fixes

1. Fix `ExpressionGenerator.EmitLambdaExpression` so that it resolves delegate
   constructors using `Compilation`'s `MetadataLoadContext`-aware APIs. Avoid
   `Type.GetConstructor` calls that introduce foreign `Type` instances.
2. Add targeted tests that compile and execute lambdas capturing extension
   invocations. Use `ilspycmd` to inspect the generated IL and ensure it mirrors
   the C# compiler's lowering.
3. Once the failure mode is addressed, cover extension method calls inside query
   comprehensions (`Select`, `Where`, `OrderBy`) to ensure LINQ works end-to-end.

## 7. Validation and polish

1. Expand semantic tests under `test/Raven.CodeAnalysis.Tests` to cover Raven
   extensions calling into other Raven extensions, overload shadowing, and
   generic receivers.
2. Document the feature in the language specification and user-facing manuals,
   emphasizing parity with C#'s extension method behavior and any deliberate
   deviations.
3. Update the TODO/roadmap once the end-to-end support is in place.
