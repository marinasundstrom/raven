# Model a shipment quote

This small quote sample shows Raven's preferred vocabulary for a domain
decision: a record for input data, a union for the possible outcomes, and a
plain function for the calculation.

```raven
record Shipment(Id: int, Weight: decimal)

union QuoteResult {
    case Quoted(amount: decimal)
    case Rejected(reason: string)
}

func Quote(shipment: Shipment) -> QuoteResult {
    if shipment.Weight <= 0 {
        return .Rejected("Weight must be positive")
    }

    return .Quoted(12.50m + shipment.Weight * 1.75m)
}
```

## What the sample shows

- `Shipment` is data with value meaning, so a record is a natural shape.
- `QuoteResult` is a closed decision: it is either quoted or rejected, and each
  outcome carries exactly the data it needs.
- `Quote` is an ownerless calculation, so it can remain a plain function rather
  than becoming a method on a utility class.
- `.Quoted(...)` and `.Rejected(...)` use the function's return type to infer
  the target union.

The important feature is not terseness. Invalid combinations—such as a
rejected quote with an amount but no reason—cannot be constructed through the
declared cases.

Continue with [records, unions, and classes](../lang/features/data-modeling.md)
for the individual type shapes, or [domain modeling](../lang/domain-modeling.md)
for larger application design choices.
