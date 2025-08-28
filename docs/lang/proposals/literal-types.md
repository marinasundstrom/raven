# Proposal: Literal-Value Types and Literal Union Types

> ⚠️ This proposal has NOT been implemented

## Summary
Allow literal values and constants to appear directly in type positions and participate in union types. Each literal-value type represents exactly one value, enabling precise exhaustiveness checks and clearer APIs. Type unions already exist in Raven and semantically hold type elements; this proposal extends them so they may also include literal-value types.

## Syntax
```raven
let flag: true | false = true
let token: 0 | 1 | "admin" = 1
open System.Math
let angle: Pi | () = Pi
```

A *literal-value type* is written with the literal itself. Literals may include numbers, strings, characters, booleans, or constant identifiers that resolve to compile‑time constants.

A *literal union* combines multiple literal-value types or mixes them with ordinary types using the existing `|` operator.

## Semantics
- Each literal-value type is a singleton subtype inhabited only by the literal itself.
- The compiler treats a union containing literals as a finite set of possible values.
- Type inference and pattern matching respect these singleton types.
- Existing type unions contain only type members; to include literals we treat each literal as its own type, using the literal's value when evaluating patterns.

## Metadata Representation
Literal unions are emitted using `TypeUnionAttribute`. Each union member becomes a constructor argument. To support literal values, `TypeUnionAttribute` must accept `object` arguments rather than only `Type` instances:

```csharp
[TypeUnion(typeof(int), "yes", 'c', 0.2, false)]
```

Constant identifiers are lowered to their literal values before emission. Consumers can reflect over `TypeUnionAttribute` to discover both type and literal members.

## Pattern Matching
Literal-value types participate in pattern matching. Any context that accepts a pattern—including the future `match` expression—may use literal patterns:

```raven
match token with
| 0 => "None"
| 1 => "One"
| "admin" => "Admin"
```

## Open Questions
- Should numeric literals respect implicit conversions when used as types?
- How are large or non-primitive constants encoded in metadata?

