# Extension Method Support Plan

This document sketches an incremental path for bringing Raven's extension method
story to parity with C# while avoiding the MetadataLoadContext issues currently
observed when compiling LINQ-heavy samples.

## 1. Baseline assessment

1. Audit the existing extension method pipeline.
   * Review `BlockBinder.LookupExtensionMethods` and overload resolution to
     understand how apparent instance calls are mapped to `BoundInvocationExpression`
     nodes with an `ExtensionReceiver`.
   * Map each stage (binding, lowering, emission) that assumes extension methods
     are imported from metadata only. Identify missing support for Raven-authored
     extension declarations.
2. Reproduce the failure thrown from `ExpressionGenerator.EmitLambdaExpression`
   when `MetadataLoadContext` attempts to resolve delegate constructors. Capture
   a minimal `.rav` repro and note the shape of the bound tree so future steps
   can add targeted tests.

## 2. Symbol and syntax work

1. Extend the syntax tree to recognize `extension` modifiers on static methods in
   static classes, mirroring C#'s rules. Ensure the language specification and
   syntax nodes mirror the new grammar.
2. Update symbol creation so that methods declared with the new modifier surface
   as `IsExtensionMethod = true`, and so that their first parameter is marked as
   the receiver. Verify that Raven-authored extension methods live in metadata
   tables compatible with existing lookup logic.

## 3. Binding and overload resolution

1. Teach the binder to include Raven-authored extension methods in method groups.
   This requires scoping rules similar to C#: only methods in imported namespaces
   and static containers should be considered.
2. Update overload resolution so that the implicit receiver argument participates
   in type argument inference and accessibility checks in the same way as native
   instance methods.
3. Strengthen diagnostics around ambiguous extension lookups and missing `using`
   imports to match the behavior testers expect from C#.

## 4. Lowering adjustments

1. Extend `Lowerer.RewriteInvocationExpression` to treat Raven-authored
   extensions identically to metadata-backed ones by substituting the receiver as
   the first argument to the lowered static call.
2. Confirm that the lowered invocation obeys value-type boxing semantics and
   nullability checks that C# enforces.

## 5. Code generation fixes

1. Fix `ExpressionGenerator.EmitLambdaExpression` so that it resolves delegate
   constructors using `Compilation`'s `MetadataLoadContext`-aware APIs. Avoid
   `Type.GetConstructor` calls that introduce foreign `Type` instances.
2. Add targeted tests that compile and execute lambdas capturing extension
   invocations. Use `ilspycmd` to inspect the generated IL and ensure it mirrors
   the C# compiler's lowering.
3. Once the failure mode is addressed, cover extension method calls inside query
   comprehensions (`Select`, `Where`, `OrderBy`) to ensure LINQ works end-to-end.

## 6. Validation and polish

1. Expand semantic tests under `test/Raven.CodeAnalysis.Tests` to cover Raven
   extensions calling into other Raven extensions, overload shadowing, and
   generic receivers.
2. Document the feature in the language specification and user-facing manuals,
   emphasizing parity with C#'s extension method behavior and any deliberate
   deviations.
3. Update the TODO/roadmap once the end-to-end support is in place.
