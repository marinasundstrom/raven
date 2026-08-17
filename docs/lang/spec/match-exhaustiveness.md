# Match exhaustiveness

A `match` is exhaustive when its arms cover every possible value of the input.
The compiler uses exhaustiveness to find missing cases and unreachable arms.

```raven
union Status {
    case Ready
    case Running
    case Finished
}

func describe(status: Status) -> string {
    status match {
        .Ready => "ready"
        .Running => "running"
        .Finished => "finished"
    }
}
```

Because all three cases are covered, a final `_` arm would be redundant and is
reported as unreachable.

Only `match` performs exhaustiveness analysis. An `is` pattern or
deconstruction checks or extracts one shape and does not need to cover every
alternative.

## Closed value spaces

Raven can prove completeness without `_` for value spaces it knows are closed:

* discriminated unions
* enums
* `bool`
* `unit`
* nullable types whose non-null domain is closed
* finite tuples made from closed element domains
* sealed class or interface hierarchies

An open-ended type such as `object` or an extensible base class normally needs
`_` or a pattern that covers the open base type itself.

## Unions and enums

Every declared union case or enum member must be covered by an unguarded arm, or
by a broader unguarded pattern.

Union payloads are part of the coverage calculation. Several arms can
collectively cover one case when its payload is itself finite:

```raven
union Failure {
    case Offline
    case Timeout
}

union Response {
    case Success(value: string)
    case Error(reason: Failure)
}

let message = response match {
    .Success(let value) => value
    .Error(.Offline) => "offline"
    .Error(.Timeout) => "timed out"
}
```

With several finite payload positions, coverage uses their Cartesian product:
every combination must be handled. Merely mentioning each possible value in
each position is not sufficient. If a payload domain is too broad to enumerate
usefully, a total pattern for the enclosing case is required.

Parenthesized unions use the same reasoning over their declared variant types.
Missing-case diagnostics identify the uncovered case and payload when possible,
such as `Error(OverflowException)` or `.Error(.Offline)`.

## Tuples and structural patterns

Finite tuples are exhaustive when every combination of their finite elements is
covered. `not`, `and`, and `or` are evaluated over that finite value space, so
complementary rows can complete the match.

The analysis is deliberately bounded. A domain too large to enumerate requires
a total pattern.

A property or nominal deconstruction pattern can prove coverage when that one
pattern is total. Raven does not combine several constrained property or
deconstruction arms into a proof because user-defined getters and
`Deconstruct` methods may not be stable or free of side effects.

A sequence pattern containing only one unconstrained rest segment covers every
length of a compatible non-null sequence. Required elements and fixed-size
segments constrain the accepted lengths and are not total by themselves.

## Nullable values

A nullable `T?` adds `null` to the value space of `T`. A complete match must
cover `null` and the entire non-null domain:

```raven
let label = status match {
    null => "unknown"
    .Ready => "ready"
    .Running => "running"
    .Finished => "finished"
}
```

For a nullable union carrier `U?`, `null` is the nullable wrapper state, not a
union case. The rule is the same for struct and class union carriers.

`unit` has the single value `()`, and a null-only domain has the single value
`null`; matching that value covers the domain.

## Sealed hierarchies

A sealed hierarchy is complete when every permitted concrete leaf is covered.
An intermediate sealed branch can be covered through all of its concrete
descendants. An open intermediate branch must instead be matched by its own base
type because further runtime subtypes may exist.

A type parameter constrained to a sealed hierarchy uses the constraint as its
closed domain. Covering every permitted leaf is sufficient even though the
scrutinee's static type is a type parameter.

## Guards and combinators

An ordinary dynamic guard does not contribute coverage because it may be false.
A compile-time `true` guard preserves the pattern's coverage; a compile-time
`false` guard contributes none.

A nested pattern guard is intersected with the pattern it guards. For closed
domains, `not pattern` covers the complement, `and` covers the intersection, and
`or` covers the union of its operands. The complement of a partial structural
pattern remains partial.

## Struct-union default state

The inactive zero-initialized state of a struct union is not a declared source
case and does not add an arm to normal exhaustiveness checking. Raven instead
prevents a possibly inactive value from crossing ordinary call and return
boundaries and retains a defensive runtime fallback for exhaustive matches.

See [Case-construction forms](unions.md#case-construction-forms) for
the struct carrier's active-state rules.
