# Raven in 60 seconds

Raven is a typed, expression-oriented language for .NET. Small programs can use
top-level statements, while larger programs can combine functions, records,
unions, classes, and interfaces.

```raven
import System.Console.*

record Shipment(Id: int, Weight: decimal)

union QuoteResult {
    case Quoted(amount: decimal)
    case Rejected(reason: string)
}

func Quote(shipment: Shipment) -> QuoteResult {
    if shipment.Weight <= 0 {
        return .Rejected("Weight must be positive")
    }

    let amount = 12.50m + shipment.Weight * 1.75m
    return .Quoted(amount)
}

let shipment = Shipment(42, 3.5m)

let message = match Quote(shipment) {
    .Quoted(let amount) => "Quote: $amount"
    .Rejected(let reason) => "Cannot quote: $reason"
}

WriteLine(message)
```

This program shows the main shape of Raven:

- `import` exposes .NET namespaces, types, or static members.
- `record` defines a value-oriented data type with a primary constructor.
- `union` defines a closed set of cases, each of which can carry data.
- `func` declares a function without requiring a containing utility class.
- `let` introduces an immutable local binding; use `var` when reassignment is
  intentional.
- `if` and `match` are expressions, although they can also be used for effects.
- Pattern arms bind values explicitly, such as `let amount`.
- String interpolation uses `$name` inside a string.
- .NET APIs such as `System.Console.WriteLine` are available directly.

## Expected failure is data

Raven uses `Option<T>` for a value that may be absent and `Result<T, E>` for an
operation that may succeed or fail in an expected way.

```raven
func ParsePort(text: string) -> Result<int, string> {
    return try int.Parse(text) match {
        Ok(let port) when port > 0 => Ok(port)
        Ok(_) => Error("Port must be positive")
        Error(_) => Error("Port must be a number")
    }
}
```

`try int.Parse(text)` captures the call as a `Result`, and `match` handles every
outcome. The propagation operator `?` can return an error from the current
function when no local handling is needed.

## Objects remain available

Use a class when identity, mutable state, lifecycle, or open polymorphism is
part of the model. Use records and unions for value-shaped and closed-domain
data. Raven does not require choosing between functional and object-oriented
programming for an entire application.

## Continue

- [Install and run Raven](getting-started.md)
- [Choose your learning path](learn.md)
- [Read the full language introduction](introduction.md)
- [Model domains with functions, records, unions, and classes](lang/domain-modeling.md)
- [Look up precise language rules](lang/README.md)
