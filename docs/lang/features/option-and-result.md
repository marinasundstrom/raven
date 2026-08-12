# Handle absence and failure

Raven uses two common carrier types to make expected control flow visible:
`Option<T>` for meaningful absence and `Result<T, E>` for an operation that can
succeed or fail with useful information.

## Represent absence with `Option`

Return `Some(value)` when a value exists and `None` when absence is an expected
outcome:

```raven
func FindPlan(code: string) -> Option<RatePlan> {
    return plans.FirstOrNone(plan => plan.Code == code)
}

let message = match FindPlan("standard") {
    Some(let plan) => "Found ${plan.Name}"
    None => "Plan not found"
}
```

Use nullable types at .NET boundaries when the framework contract calls for
them. Convert to an option when absence has domain meaning inside the
application.

## Represent expected failure with `Result`

A result makes both the success value and error value part of the function's
contract:

```raven
func ValidateWeight(weightKg: decimal) -> Result<decimal, string> {
    if weightKg <= 0 {
        return Error("Weight must be positive")
    }

    return Ok(weightKg)
}
```

Use exceptions for faults and framework contracts that are naturally
exception-based. Use `Result` when callers are expected to inspect and handle
the failure.

## Propagate a failure with `?`

Inside a compatible function, `?` extracts the successful value or returns the
failure immediately:

```raven
func BuildQuote(weightKg: decimal) -> Result<decimal, string> {
    let weight = ValidateWeight(weightKg)?
    return Ok(12.50m + weight * 1.75m)
}
```

Use `match` instead when the current function should recover, translate the
error, or choose another path.

See [Raven.Core](../../compiler/raven-core-library.md) for the available
`Option` and `Result` operations.
