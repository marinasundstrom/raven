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
