# Make decisions with patterns

Patterns let a decision test a value's shape and introduce the data proven by
that test. They are especially useful with unions, `Option`, and `Result`.

## Interpret every union case

Use `match` to give behavior to each state of a closed domain:

```raven
func Describe(result: QuoteResult) -> string {
    return match result {
        .Quoted(let amount) => "Quote: $amount"
        .Rejected(let reason) => "Rejected: $reason"
    }
}
```

The bindings exist only in the arm where the pattern succeeds. When the union
gains a case, exhaustiveness checking helps identify decisions that need a new
meaning.

## Match ordinary values

Patterns also work with literals, types, tuples, and fallback cases:

```raven
let category = match statusCode {
    200 => "success"
    404 => "not found"
    int code when code >= 500 => "server error"
    _ => "other"
}
```

Use `_` when the remaining values genuinely share one meaning. Avoid a broad
fallback when named cases of nominal unions should force future code changes.

## Use patterns to narrow a value

A successful pattern can establish that a nullable value is present or that a
value has a particular runtime type:

```raven
if let name: string = nullableName {
    Console.WriteLine(name.Length)
}
```

Inside the body, `name` is a non-null `string`.

Continue with [absence and failure](option-and-result.md) for the carrier types
most commonly interpreted with patterns.
