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

## Where(source, predicate)

* Replaying the LINQ sample from the baseline doc inside semantic tests keeps
  both `Enumerable.Where` overloads viable, so `GetTargetType` returns `null`
  for the lambda argument once overload resolution sees multiple matches. The
  binder immediately reports `RAV2200`, mirroring the CLI failure and preventing
  metadata-backed scenarios from compiling without explicit parameter
  annotations.【F:docs/compiler/design/extension-methods-baseline.md†L52-L81】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2094-L2167】
* To unblock Stage 2 we need to carry delegate candidate information through
  lambda binding instead of erroring early. Once the binder can surface the
  overload-specific delegate shapes to inference, we can record a passing trace
  that demonstrates parity with Roslyn for `Where`.

## Gaps

The traced scenarios exercised method group formation, overload resolution, and
type inference for `Any` and `Select`. Those paths still look healthy, but the
`Where` overload pair blocks Stage 2 until lambda inference can flow delegate
shapes through overload resolution. We'll return to lowering coverage once the
new binder plumbing lands so we can record a successful trace for `Where` as
well.【F:docs/compiler/design/extension-methods-baseline.md†L52-L104】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1056-L1109】

## Regression status

* 2025-09-28 — Running `dotnet test test/Raven.CodeAnalysis.Tests` keeps the
  metadata-backed `ExtensionMethodsFixture` green, confirming the semantic trace
  above still reflects the current implementation. The broader suite currently
  fails earlier in `GlobalForEach_AnalyzesAsImperativeLoop`, so the extension
  method plan remains blocked on the lambda inference work captured in Stage 2
  even though the dedicated fixture continues to pass.【a0a663†L1-L2】【acbd56†L13-L33】

