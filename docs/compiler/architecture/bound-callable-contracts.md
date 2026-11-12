# Bound callable contracts

Method-group and delegate-creation nodes preserve reference identity. A method
group stores its candidates and selected method, and lazily caches its inferred
delegate type, including a null result. Delegate creation falls back to its
explicit delegate type when inference has no result. The factory is evaluated
at most once per method-group node.

A default or empty candidate array is rejected with `ArgumentException` for
`methods`. Candidate selection must check default-array state before reading its
length. Delegate creation rejects a null `methodGroup` with `ArgumentNullException`
before reading it in the base initializer. Both defects were exposed by shared
port tests; they previously escaped as `NullReferenceException` before the intended
argument validation.

Raven does not generally narrow storage after an ordinary null test. The port
therefore uses `if let factory: Func<TypeSymbol?> = delegateTypeFactory` before
constructing `Lazy<TypeSymbol?>`. C# uses the equivalent non-null binding pattern.
This follows the existing [static nullability rules](../../lang/nullability.md);
it is not a new constructor or nullable-flow feature. Preserve that deliberate
language difference during further translations.

Generated Raven updates import `System` so framework delegate types such as
`Func<T>` in constructor signatures resolve in the generated file's own scope.
The shared `BoundCallableContractTests` cover candidate validation and selection,
receiver traversal, lazy inference (including null), fallback type and updates.
These are compiler data contracts, not native neoCLR runtime execution evidence.

Invocation nodes preserve their selected method, argument sequence, optional
ordinary/extension receivers, receiver-address requirement, and conditional
omission flag. Their expression type is interpreted through `NullableMetadataFacts`
so an annotated value-type return does not become a runtime nullable value.

Traversal visits the ordinary receiver, then a distinct extension receiver, then
arguments in order. If both receiver properties refer to the same expression it
is visited once. Generated `Update` methods reuse unchanged nodes and preserve
the call flags when replacing operands. Both compiler implementations now carry
these contracts.

Diagnostic display supports methods without a containing type: an invocation with
no receiver or type owner prints `Method(...)` rather than throwing. A containing
type still supplies the usual `Type.Method(...)` prefix. This local correction is
covered by `BoundInvocationContractTests` alongside the data/traversal contracts.
