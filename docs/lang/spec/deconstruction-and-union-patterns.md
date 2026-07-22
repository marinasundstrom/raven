# Deconstruction, member, and union patterns

* `RecordType(pattern1, pattern2, …)` — **nominal deconstruction pattern**.
  Matches when the scrutinee can be treated as `RecordType` and each positional
  subpattern matches the corresponding value produced by `RecordType`’s
  `Deconstruct` shape.

  * Record patterns are valid for deconstructable nominal types, including
    `record` types and other nominal types that expose an accessible
    `Deconstruct` method.
  * Primary-constructor classes and structs with promoted public `val` / `var`
    parameters synthesize a `Deconstruct` method in declaration order, so the
    same pattern form works for them.
  * Each positional element is a pattern, so bindings still require `let`/`var`
    unless an outer construct such as `if let pattern = expr` or
    `while let pattern = expr` supplies the binding mode.
  * An element may optionally include a name before the colon
    (`Name: pattern`). Named elements bind by `Deconstruct` parameter name
    rather than source position and may appear in any order. Unnamed elements
    continue to fill the remaining unmatched parameters in declaration order.
  * A trailing whole-pattern designation may capture the successfully matched
    nominal value: `Person(1, name, _) person`.
  * The number of positional elements must match the selected `Deconstruct`
    parameters; mismatches are errors.
  * If a named element does not match any supported deconstruction
    member/parameter on the target type, the pattern is invalid and reports the
    member-not-found diagnostic.
  * When the scrutinee is a discriminated union, `CaseName(...)` in nominal-deconstruction-pattern
    syntax is interpreted as a discriminated-union case pattern and binds the case
    payload positionally.

## Member patterns

* `.Case` / `Type.Case` — **member pattern**. Matches a named member by name,
  including discriminated union cases, constants, or nested types.

  * The leading `.` resolves against the current scrutinee.
  * Member payloads may supply nested subpatterns matching the member’s
    parameter list, e.g. `.Identifier(text)` or `Result<int, string>.Error(let message)`.
  * Parentheses are optional for parameterless members.
  * Payload arity must match the declared parameters.
  * Each nested subpattern is typed to the corresponding member parameter.
  * A payload element introduces a new binding **only** when it uses
    `let`/`var` (`val` is also accepted). A bare identifier is a value pattern that matches an
    existing in-scope symbol.

    * Example: `.Case(let a, b)` binds `a` and matches the second payload against
      the runtime value of in-scope `b`.
  * A trailing whole-pattern designation may capture the matched member/case
    value: `.Case(...) caseValue`.
  * Use `_` to explicitly discard a payload.

## Union case patterns

Body-form unions synthesize named case types, and those cases may be matched
using three equivalent forms. The unqualified case form is the default pattern
syntax; the qualified and target-typed forms are alternatives for disambiguation
or stylistic clarity:

* `Case(...)` or `Case` — unqualified case pattern. This is the default form
  when the case name is unambiguous for the scrutinee's union case set.
* `Union.Case(...)` — explicitly qualified union-member form.
* `.Case(...)` — target-typed/member shorthand when the scrutinee determines the
  union.

For a case carrying a single `unit` payload, a bare `Case` pattern is sugar for
`Case(())`.

Parenthesized unions do not synthesize case names. They are matched using the
ordinary pattern for one of their declared member types:

```raven
union Payment(Cash | Card)

let description = match payment {
    Cash(let amount) => "cash $amount"
    Card(let reference) => "card $reference"
}
```

## Pattern combinators

* `pattern1 and pattern2` — **conjunction**. Succeeds only when both operands
  match. Bindings from either operand are available after the conjunction.

* `pattern1 or pattern2` — **alternative**. Matches when either operand matches.

* `not pattern` — **complement**. Succeeds when the operand fails. `not` never
  introduces bindings.

Precedence: `not` > `and` > `or`. `or` associates left-to-right. Parentheses
override precedence.
