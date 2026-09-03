# Model data with records, unions, and classes

Raven provides several type shapes because applications need to communicate
different meanings. Choose the shape from what the value represents, not from
which syntax is shortest.

## Use a record for a value

A record is a good default for data whose represented fields determine its
meaning, such as a message, DTO, or domain value:

```raven
record class Shipment(
    val Id: string,
    val WeightKg: decimal)
```

Use `record struct` when value-type storage fits. Use `record class` when
reference storage fits while structural value behavior remains useful.

## Use a nominal union for closed alternatives

A nominal union models a value that can be exactly one of a known set of states.
Each state carries only the data valid for that state:

```raven
union QuoteResult {
    case Quoted(amount: decimal)
    case Rejected(reason: string)
}
```

This is more expressive than an enum plus nullable detail fields: a quoted
result must have an amount, and a rejected result must have a reason.

Nominal unions use either parenthesized variant types or `case` declarations. If
the alternatives do not need a reusable domain name, use an ad-hoc union type
directly, such as `string | null`.

Use an enum when alternatives are only named constants. Use an interface or an
open class hierarchy when other code must add implementations.

## Use a class for identity and lifecycle

A class is appropriate when an object has identity, owns resources, or keeps
state that changes during its lifetime:

```raven
class RetryCounter(var Count: int) {
    func Increment() {
        Count = Count + 1
    }
}
```

Raven supports object-oriented design directly. What it avoids is requiring a
class merely to contain every function.

## Combine the shapes

Real programs normally use all three: records for messages and values, unions
for closed states, and classes for stateful services or resource owners.

Continue with [patterns](patterns.md) to interpret union cases, or read the
[domain-modeling guide](../domain-modeling.md) for larger design examples.
