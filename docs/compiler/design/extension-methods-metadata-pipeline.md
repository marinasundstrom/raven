# Metadata Extension Method Invocation Trace

Stage 2 of the extension-methods plan calls for tracing how the compiler binds
and lowers calls into metadata-defined extension methods. The new semantic tests
exercise representative LINQ helpers and document how the binder forms method
groups, performs overload resolution, and infers type arguments when lambdas
appear as delegate parameters.

## Any(source, predicate)

* `MetadataExtensionMethodSemanticTests.Invocation_WithLambdaArgument_ResolvesPredicateOverload`
  confirms that `numbers.Any(func (value) => value > 0)` produces a method group
  with both metadata overloads, selects the two-parameter predicate overload,
  and converts the lambda into `Func<int, bool>` during argument processing.
  The bound invocation retains the original receiver through
  `BoundInvocationExpression.ExtensionReceiver`, mirroring the metadata
  pipeline.【F:test/Raven.CodeAnalysis.Tests/Semantics/MetadataExtensionMethodSemanticTests.cs†L133-L195】
* `MetadataExtensionMethodSemanticTests.Invocation_WithoutLambdaArgument_ResolvesParameterlessOverload`
  verifies the complementary case: when no lambda is supplied, overload
  resolution picks the parameterless `Any<TSource>()` extension while still
  providing the array-backed extension receiver and inferring `TSource = int`
  from the receiver type.【F:test/Raven.CodeAnalysis.Tests/Semantics/MetadataExtensionMethodSemanticTests.cs†L197-L232】

## Select(source, projector)

* `MetadataExtensionMethodSemanticTests.Invocation_WithProjectionLambda_InfersSourceAndResultTypes`
  shows that the compiler infers both `TSource = int` and `TResult = string`
  from `numbers.Select(func (value) => value.ToString())`. The projection lambda
  is converted to `Func<int, string>`, and the bound invocation records the
  extension receiver alongside the converted delegate argument for lowering.
  No metadata-only gaps surfaced during this trace.【F:test/Raven.CodeAnalysis.Tests/Semantics/MetadataExtensionMethodSemanticTests.cs†L234-L279】

## Gaps

The traced scenarios exercised method group formation, overload resolution, and
type inference for `Any` and `Select`. The binder produced the expected
`BoundInvocationExpression` graph in each case, so no metadata-consumer gaps are
currently blocked on Stage 2. Further coverage will follow once we expand into
end-to-end lowering tests for Stage 2 step 4.【F:test/Raven.CodeAnalysis.Tests/Semantics/MetadataExtensionMethodSemanticTests.cs†L133-L279】

