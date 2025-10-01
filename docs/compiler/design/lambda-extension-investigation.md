# Lambda expressions and extension method consumption

This investigation summarises the current lambda implementation in Raven, the open
issues we have identified, and how those gaps block real-world extension method
consumption.

## Lambda implementation snapshot

* **Binding pipeline.** `BlockBinder.BindLambdaExpression` constructs
  `SourceLambdaSymbol` instances, binds the body inside a dedicated
  `LambdaBinder`, and records every delegate type offered by overload resolution
  as potential targets for later inference and replays. The binder caches the
  delegate candidates through `_lambdaDelegateTargets` so subsequent passes can
  rehydrate the lambda with concrete signatures during overload resolution.
  【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L547-L706】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L300-L392】
* **Replay support.** When overload resolution needs to reconsider a lambda
  against a different delegate (for example, after picking an extension method),
  `ReplayLambda` either rebinds the syntax with the selected delegate or checks
  the previously bound lambda for compatibility. Successful replays are cached,
  letting repeated queries reuse the rebound lambda without rebinding syntax.
  【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L530-L590】
* **Compatibility checks.** `BoundLambdaExpression` retains the candidate
  delegates gathered during binding and compares them against a target delegate
  in `IsCompatibleWithDelegate`. The helper attempts to match the delegate
  signatures (including `ref` kinds and conversions) and maps any type parameters
  shared between the lambda parameters, return type, and the candidate delegate.
  【F:src/Raven.CodeAnalysis/BoundTree/BoundLambdaExpression.cs†L16-L143】
* **Emission.** The expression generator can lower both capturing and
  non-capturing lambdas. Captures trigger closure synthesis in `TypeGenerator`;
  the resulting helper method and closure instance are used to manufacture the
  delegate at runtime. Non-capturing lambdas become static helpers paired with
  `ldftn` delegate creation. 【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L159-L420】

## Known lambda pain points

* **Delegate candidates remain open.** `ExtractLambdaDelegates` records the
  delegate type from each viable overload, but the metadata-backed delegates
  still expose method type parameters (for example `Func<TSource, bool>` from
  LINQ). Those open delegates flow directly into
  `BoundLambdaExpression.CandidateDelegates` without specialising them to the
  receiver element type.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L318-L347】
* **Compatibility loses generic context.** Because the candidate delegates retain
  their open generic parameters, `IsCompatibleWithDelegate` compares the lambda
  against signatures that still mention the original type parameters. The helper
  attempts to substitute types, but without concrete arguments for those type
  parameters the compatibility check fails and the lambda is marked incompatible
  even when a matching delegate exists.【F:src/Raven.CodeAnalysis/BoundTree/BoundLambdaExpression.cs†L70-L143】

### Additional investigation findings

* **Parameter inference preserves type parameters instead of concrete types.**
  When a lambda has no explicit annotations, `BindLambdaExpression` pulls the
  candidate delegates and asks `TryInferLambdaParameterType` for each parameter.
  For metadata delegates this helper returns the delegate’s own type parameter
  (for example `TSource`) rather than the constructed element type, so the lambda
  parameters retain generic placeholders until a later replay can substitute
  them.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L40-L114】 The
  binder suppresses `RAV2200` at this point and stores the suspended diagnostics
  on `BoundUnboundLambda`, so any overload that fails to replay the lambda later
  will resurface the inference failure.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L78-L99】【F:src/Raven.CodeAnalysis/BoundTree/BoundUnboundLambda.cs†L8-L38】
* **Replay adds constructed delegates too late for initial compatibility.** The
  first time overload resolution revisits a lambda, `EnsureLambdaCompatible`
  checks `CandidateDelegates` before rebinding. Because the cache still only
  contains the open generic forms, `IsCompatibleWithDelegate` has to perform a
  full signature walk to prove compatibility every time. Only after
  `ReplayLambda` succeeds do we append the constructed delegate (for example
  `Func<Int32, bool>`) back into `_lambdaDelegateTargets`, so subsequent passes
  can short-circuit.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L318-L391】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L530-L699】【F:src/Raven.CodeAnalysis/BoundTree/BoundLambdaExpression.cs†L36-L143】
* **Method type inference ignores lambda arguments.**
  `OverloadResolver.ApplyTypeArgumentInference` intentionally skips
  `BoundLambdaExpression` arguments when unifying method type parameters.
  Generic extensions such as `Enumerable.Select<TSource, TResult>` therefore rely
  entirely on the receiver to infer `TSource`; `TResult` never acquires a
  substitution because the selector lambda is excluded from inference. In the
  metadata path this causes the constructed overload to be dropped before the
  replay step, leaving overload resolution with no viable candidates.【F:src/Raven.CodeAnalysis/OverloadResolver.cs†L12-L158】【F:src/Raven.CodeAnalysis/OverloadResolver.cs†L126-L139】

## Impact on extension methods

* **Overload resolution drops metadata-backed extensions.** Extension methods are
  replayed after overload resolution selects a candidate. When an extension has a
  lambda parameter (e.g. `Enumerable.Where<TSource>(this IEnumerable<TSource>,
  Func<TSource, bool>)`), the binder records the open `Func<TSource, bool>`
  delegate and later replays the lambda against the specialised delegate from the
  chosen overload. Because the cached delegate remains open, the compatibility
  check fails and overload resolution reports `RAV1501`, effectively discarding
  the metadata extension even though the CLI supplied `System.Linq.dll`.
  【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L530-L585】【F:src/Raven.CodeAnalysis/BoundTree/BoundLambdaExpression.cs†L36-L143】【F:src/Raven.Compiler/Program.cs†L180-L188】
* **Fixture extensions mask the failure.** Semantic tests currently pass thanks
  to bespoke fixtures that mirror LINQ signatures but operate entirely within the
  Raven compilation. Those fixtures avoid the metadata specialisation gap, so the
  remaining bug only surfaces when consuming the real BCL assemblies.
  【F:test/Raven.CodeAnalysis.Tests/Semantics/MetadataExtensionMethodSemanticTests.cs†L305-L463】

## Next steps

1. **Specialise delegates before caching.** Instantiate metadata delegates with
   the inferred method type arguments (or the receiver element type) before they
   are stored in `_lambdaDelegateTargets`. That way, compatibility checks see the
   concrete `Func<Int32, bool>` rather than the open generic form.
2. **Re-evaluate replay caching.** Once delegates are specialised, ensure the
   rebinding cache differentiates between constructed delegates so we do not
   reuse a lambda rebound against a different instantiation.
3. **Extend semantic coverage.** Add tests that consume LINQ directly from
   `System.Linq.dll` to prevent regressions once the specialisation work lands.
