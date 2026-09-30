# Native Self (neoCLR experiment)

When a native Self Runtime Contract is configured, `Self` denotes the implementing
type in an interface signature. It does not introduce a generic parameter:

```raven
interface Number {
    static val Zero: Self { get; }
    static func +(left: Self, right: Self) -> Self
}

func Sum<T>(left: T, right: T) -> T where T: Number => left + right
```

In a concrete class or struct, Self denotes that declaring type. `self` remains
the instance value. A value erased to Number cannot invoke a Self-dependent
member without a known implementing type. Self is not enabled for ordinary CLR
targets; CLR generic math continues to require explicit generic contracts.

See [Runtime Contracts](../compiler/runtime-contracts.md#native-implementing-type-self-neoclr-experiment)
for configuration, metadata behavior and current integration limitations.

Self inheritance remains an isolated neoCLR experiment, separate from the compiler
boundary/multi-target refactor. Self is anchored to the class declaring conformance:
Derived inheriting Base : Clonable retains Base-returning Clone, but does not
satisfy a native Self bound as Derived. Redeclaring Clonable requires a matching
Derived result. An explicit `func Clonable.Clone() -> Self` implementation can
coexist with the inherited Base method; virtual overrides keep the Base signature.
Explicit and inferred generic calls validate this rule under the configured Self
Runtime Contract, with no additional target option. Metadata retains the declared
return types and explicit interface mapping. These are bounded feature changes;
broader neoCLR target integration follows the separate multi-target refactor.

## Shared-main rebase — 2026-09-30

The five native Self feature commits now build on merged main b40fe495d.
Existing Self configuration validation follows main's target-owned runtime
contract. Option copies retain both Self and main's nullable-value policy;
the metadata test covers copies in both directions. The opt-in feature behavior
and conformance rules are unchanged.

Compiler builds passed for .NET 10 and .NET 11; all 83 focused Self, nullable-type
and target-configuration tests passed on .NET 11. These validate compiler
semantics and CLI metadata transport, not native neoCLR execution. Whitespace
formatting and diff checks passed. The branch remains separate while target
mappings and unsupported-feature diagnostics are designed.


## Explicit platform API synchronization — 2026-09-30

Merged shared main c0a6344d0 into this feature branch without rewriting its published
history. Immutable option copies retain both TargetPlatform and RuntimeSelfTypeContract;
state-transfer compatibility checks include both. The pre-change Self baseline
passed 12 tests; 27 focused Self/platform/profile tests passed after reconciliation
on .NET 11. This synchronization does not yet enforce neoCLR-only Self or merge Self
into main. Function/structural-type isolation is being handled separately before
adding the target gate; the previous marker-only opt-in remains experimental here.

## Target-gated shared integration (2026-09-30)

Self now requires explicit `TargetPlatform.NeoCLR` selection as well as its marker
contract. Start with `CompilationOptions.NeoCLR.WithRuntimeSelfTypeContract(...)`;
projects set `RavenTargetPlatform=NeoCLR` and their Self assembly/type properties.
Configuring the marker on .NET reports RAVT003. The shared compiler supports this
opt-in target feature without enabling structural Function types. Historical
branch-only notes above describe earlier development checkpoints.
