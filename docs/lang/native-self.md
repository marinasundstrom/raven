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
