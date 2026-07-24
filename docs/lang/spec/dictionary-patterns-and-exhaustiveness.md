# Dictionary patterns and exhaustiveness

Dictionary patterns check for required keys and match their values at the same
time. They are useful when a branch only cares about a few known entries.

Dictionary patterns use bracket syntax with keyed entries:

```raven
if lookup is ["a": let first, "b": 2] {
    WriteLine(first)
}
```

Each dictionary-pattern entry evaluates its key expression and then matches the
corresponding value against the nested pattern. A dictionary pattern succeeds
only when:

* the scrutinee is dictionary-compatible,
* every requested key exists, and
* every nested value pattern succeeds.

Dictionary compatibility is defined in terms of the dictionary surfaces Raven
binds directly today: `IDictionary<TKey, TValue>`,
`IReadOnlyDictionary<TKey, TValue>`, and types that implement one of those
interfaces. Key expressions are converted to `TKey`; nested patterns are bound
against `TValue`.

Dictionary deconstruction uses the same keyed syntax in declaration or
assignment position:

```raven
val values: IReadOnlyDictionary<string, int> = ["a": 2, "b": 3]
let ["a": first, "b": second] = values
```

This is extraction-oriented rather than boolean. After validating that the
source is dictionary-compatible, each entry reads the requested key and assigns
the resulting value through the nested designation/pattern.

Only `match` participates in exhaustiveness checking.

## Exhaustiveness

A `match` expression is **exhaustive** when its set of arms covers every
possible runtime value of the input expression (the *scrutinee*).

When a `match` expression (and statement form) is exhaustive, a discard arm (`_ => ...`) is
unnecessary and is reported as unreachable.

Exhaustiveness is determined by analyzing the *runtime value space* implied by
the scrutinee’s static type. The compiler can prove exhaustiveness without a
discard arm when that value space is finite or otherwise fully covered by the
provided patterns.

Exhaustiveness analysis applies in particular to:

* **Discriminated unions** declared with the `union` keyword.
* **Enums**.
* **`bool`** (`true` and `false`).
* **Finite tuple products** whose element value spaces can all be enumerated,
  including nested tuples composed from `bool`, enums, and discriminated unions.
* **Sealed hierarchies** — a `sealed` class with a `permits` clause (or
  otherwise participating in a closed inheritance set). Closed branches
  contribute concrete leaves; open intermediate branches must be covered by
  matching the intermediate type.

For discriminated unions, exhaustiveness is computed from the union's declared
case set (equivalent to sealed-hierarchy reasoning over a closed subtype set).
Each declared case and its payload value space must be covered by unguarded
arms, or by an unguarded catch-all arm. Multiple arms may collectively cover a
case payload. For example, separate `Error(.A)` and `Error(.B)` arms cover the
entire `Error` case when the payload union declares only `A` and `B`. The same
rule applies to other finite payload domains such as `bool` and to the Cartesian
product of multiple finite payloads. Coverage must include every combination;
independently mentioning every value in each payload position is insufficient.
When a finite union payload is wholly or partly uncovered, the missing-case
diagnostic identifies each uncovered case-and-payload alternative, such as
`Error(OverflowException)` for a parenthesized union or
`Error(.ServiceUnavailable)` for a nested discriminated union. If the payload
domain cannot be usefully enumerated, the diagnostic conservatively reports the
enclosing case instead.

Positional tuple patterns use the same Cartesian-product analysis. Pattern
combinators (`not`, `and`, and `or`) are evaluated over each finite value, so
complementary tuple rows can establish exhaustiveness. The analysis is bounded;
domains that cannot be enumerated within the compiler's finite-product limit
remain conservative and require a total pattern. Property and nominal
deconstruction patterns can establish exhaustiveness when a single pattern is
itself total, but separate constrained property/deconstruction arms are not
combined: evaluating user-defined accessors or deconstruction methods across
arms is not assumed to be stable or side-effect free.

Boolean pattern combinators also participate directly in closed enum and
discriminated-union domains, closed type unions, sealed hierarchies, and
nullable domains. In particular, `not CasePattern` covers the complement of a
fully covered case pattern, `null` and `not null` partition a nullable domain,
and `and` covers the intersection of its operands. For closed type sets the
compiler distinguishes patterns that cover none, some, or all values of each
candidate type; complements of partial structural patterns remain partial.
Guards never contribute coverage unless they are compile-time guaranteed to
match.

A nested guarded pattern contributes coverage only when its expression guard
is compile-time `true`; a nested pattern guard is intersected with the guarded
pattern. Dynamic and compile-time-false guards contribute no coverage. For
sequence patterns, a single unconstrained rest segment (for example `[...]`)
covers every length and is a total pattern for a compatible non-null sequence.
Patterns containing required elements or fixed segments remain length
constrained.

The `unit` domain contains only `()`, and the null-only domain contains only
`null`; matching those respective values is exhaustive. A compile-time-true
match-arm guard preserves the arm pattern's coverage in every domain, while a
false or dynamic arm guard contributes no coverage.

For nullable discriminated union carriers (`U?`), exhaustiveness is computed
from the underlying union's declared case set plus the nullable wrapper's
`null` value. This rule is the same for `union struct` and `union class`; the
`null` arm covers the nullable wrapper state, not a union pseudo-case.

In addition, exhaustiveness may be proven through type analysis even when no
explicit finite-case construct is involved. For example:

* Covering all concrete types in a **closed inheritance chain**.
* Covering both the non-null and `null` cases of a nullable type.
* Using **type patterns** (for example, `string`, `MyBaseType`, or other
  concrete runtime types) such that all possible runtime types implied by the
  scrutinee are handled.

For sealed hierarchies with sub-hierarchies:

* If an intermediate subtype is `sealed`, exhaustiveness can be satisfied by
  covering its concrete descendants.
* If an intermediate subtype is not `sealed`, exhaustiveness for that branch
  requires a pattern that targets the intermediate type itself.

If every possible runtime value implied by the scrutinee type is handled by
explicit pattern arms, a discard arm is redundant.

When the scrutinee type is open-ended (for example, `object` or another
extensible base type), exhaustiveness generally cannot be proven without a
discard arm, or type pattern for type `object`.
