# Types and unions

### Type annotations

Use type annotations where inference is insufficient or where a particular
target type is required. Locals commonly infer their type from the initializer;
function-expression parameters can infer from a delegate target; ordinary
function and method parameters still declare their parameter types explicitly:

```raven
val a = 2
val b: int = 2

func add(a: int, b: int) -> int { a + b }
```

### Tuple types

Tuple types use parentheses with comma-separated element types and map to
`System.ValueTuple`:

```raven
val pair: (int, string) = (42, "answer")
```

Elements may optionally be named with a `name: Type` pair. Names exist only for
developer clarity and do not participate in type identity or assignment:

```raven
val tuple2: (id: int, name: string) = (no: 42, identifier: "Bar")
```

When a tuple expression is assigned to an explicitly annotated tuple type, each
element is validated against the corresponding element type. Elements are
accessed positionally (e.g. `Item1`, `Item2`). Tuple types may nest or
participate in other type constructs such as unions or nullability.

### Standard union types

The type syntax `T1 | T2` is shorthand for `System.Union<T1, T2>`. The same
spelling supports three, four, and five alternatives. These standard unions are
provided by `Raven.Core` as a temporary bridge until .NET adopts a standard
union type.

### Function types

Function types describe callable delegates directly in a type annotation. The
syntax mirrors a lambda signature: a comma-separated
parameter list enclosed in parentheses followed by `->` and the return type.

```raven
val applyTwice: ((int -> int), int) -> int
val thunk: () -> unit
val comparer: (string, string) -> bool
```

In declaration-oriented lists, a newline may stand in for the expected explicit
separator token. The syntax tree keeps the separated-list shape and stores
`SyntaxKind.None` for those newline-delimited separator slots. If the separator
is omitted on the same line, recovery instead uses a missing separator token of
the expected kind and reports a diagnostic.

Single-parameter functions may omit the surrounding parentheses:

```raven
val increment: int -> int
```

The return portion may itself be any Raven type. Nested arrows associate to the right, so `int -> string -> bool` is
parsed as `int -> (string -> bool)`.

Function annotations are sugar over delegates. When the parameter and return
types match an existing declaration (including the built-in `Func`/`Action`
families), the compiler binds to that delegate. Otherwise it synthesizes an
internal delegate with the appropriate signature so interop with .NET remains
transparent. Parameter modifiers and names are not permitted inside a function
type; specify only the types that flow into and out of the delegate. A `unit`
return represents an action with no meaningful result.

Function-expression syntax, including explicit `func` expressions, lambda
shorthand, modifiers, and named recursive function expressions, is described in
`Function expressions`.

When a function expression is target-typed by a delegate requirement (for
example, assignment to `Action<int>` or passing to a delegate-typed parameter),
Raven projects the function value to a compatible delegate. Built-in
`Func`/`Action` delegate shapes are displayed as function signatures in Raven
type displays, while custom delegate types remain visibly named delegates.

### Nullability and `null`

Nullability is **explicit** in Raven. Reference types are non-nullable by
default, and `null` can only flow through nullable annotations (`T?`). The same
rules apply uniformly to reference and value types; the distinction only
affects runtime representation, not the surface type rules.

`null` is not a general type annotation spelling and is not a union member type.
Use nullable annotations such as `T?` to mark nullable active union contents.

#### Nullable suppression (`!`)

`!` treats the operand as non-null for a single expression. Use it only when
the programmer has stronger knowledge than the exposed type.

##### Concept

* For nullable references, `expr!` changes the static type from `T?` to `T`
  without inserting a runtime null check.
* For nullable value types, `expr!` unwraps `T?` to `T`.

##### Example

```raven
func ReadName(service: ExternalService) -> int {
    val name = service.TryGetName()!
    return name.Length
}
```

```raven
func Increment(value: int?) -> int {
    val required = value!
    return required + 1
}
```

##### Rules

* `!` affects only the annotated expression.
* `!` does not relax nullability rules for surrounding expressions.
* Using `!` reports warning `RAV0403` on the full `<expr>!` nullable
  suppression expression.

### Unions

Unions define nominal carrier types with a fixed set of cases. A union value is
always stored as the declared carrier type, and case values convert to that
carrier implicitly when required.

Plain `union` declarations synthesize struct carriers by default. Use
`union class` when a reference carrier is intended, such as for APIs that must
not expose the default struct-union state.

| Form | Syntax | Cases | Typical pattern form |
| --- | --- | --- | --- |
| Parenthesized | `union Payment(Cash \| Card)` | existing member types | `Cash(...)`, `Card(...)` |
| Body form | `union LookupResult { case Found(id: int) case Missing }` | synthesized case types | `Found(...)`, `Missing` |

#### Parenthesized unions

##### Concept

The parenthesized form declares a carrier over existing nominal or primitive
types. Raven does not synthesize additional case types for this form.

##### Example

```raven
record Cash(amount: decimal)
record Card(last4: string)

union Payment(Cash | Card)
union OptionalPayment(Cash | Card?)

val paidInCash: Payment = Cash(12.50m)
val paidByCard: Payment = Card("4242")
```

##### Rules

* Each listed member type is part of the carrier's closed case set.
* `null` is not a member type. Use nullable member annotations such as `T?` when
  a parenthesized union member may actively carry null.
* Pattern matching uses ordinary patterns over those member types. Nullable
  member patterns do not cover the `null` branch for exhaustiveness; include a
  `null` arm when the union contents may be null.
* Construction occurs by constructing a listed member type and then converting it
  to the carrier when needed.

#### Body-form unions

##### Concept

The body form declares a carrier with an ordinary member body. That member body
may contain `case` declarations alongside other members such as methods and
properties. Each `case` declaration synthesizes a named case type.
This form is also known as a tagged union because each value belongs to one
named case in the union's closed case set.

##### Example

```raven
union LookupResult {
    case Found(id: int)
    case Missing

    func Describe() -> string {
        match self {
            Found(val id) => "found $id"
            Missing => "missing"
        }
    }
}

val found: LookupResult = Found(42)
val missing: LookupResult = Missing
```

##### Rules

* Each `case` declaration declares one synthesized case type.
* Body-form unions may also declare ordinary members in the same body.
* `case` declarations are valid only inside `union` declarations.
* Case references may use `Union.Case`, `.Case`, or unqualified `Case` when
  resolution is unambiguous.
* A comma or semicolon after a case is optional; when present it terminates that
  case declaration.
* Generic unions are allowed in both forms, for example
  `union Result<T, E> { case Ok(value: T) case Error(error: E) }`.
* `union` declarations may be `partial`. Cases and ordinary members may be
  distributed across partial declarations of the same union.
* The carrier reserves the member names `Value` and `HasValue` for synthesized
  members.
* As with records, an authored `override ToString()` suppresses the synthesized
  union `ToString()`.
* Authored `Equals`, `GetHashCode`, and equality operators on unions are
  currently rejected.

Line-continuation details for leading-dot case forms are defined in
[Control flow: Line continuations](control-flow.md#line-continuations).

#### Case construction and extraction

##### Concept

Case construction creates a case value first. Conversion to the carrier happens
when the surrounding context requires the union type.

##### Example

```raven
val ok: Result<int, string> = Ok(99)
val err = Result<int, string>.Error("boom")

val outcome: Either<int, string> = 42
val left = (int)outcome
```

##### Rules

* `Case(...)` constructs the case value directly.
* `Union.Case(...)` resolves the case through the union surface and constructs
  the same case value.
* `.Case(...)` resolves the case from the target type's union case set.
* Unqualified `Case(...)` is valid only when case lookup is unambiguous. Normal
  lexical lookup wins before union-case lookup.
* Every union carrier exposes a conventional `Value` property whose runtime
  value is the currently stored member or case value.
* `Value` has a C#-compatible `object` or `object?` shape. This property shape
  is not the source of truth for nullable active contents; Raven derives that
  from the case construction surface.
* Every union carrier also exposes `HasValue: bool`, which follows the
  C# union access pattern and reports whether `Value` is not `null`.
* Public one-parameter constructors define the C#-compatible case set.
  `TryGetValue(out CaseType)` exposes carrier inspection for each case type but
  does not add extra cases when constructors already define the case set.
* An explicit cast from the carrier to a member or case type succeeds only when
  the carrier currently holds that case; otherwise it throws
  `InvalidCastException`.
* Pattern matching is preferred to explicit casts for ordinary extraction.

In pattern position:

* Body-form unions use `Case(...)` or `Case` by default when the case name is
  unambiguous.
* `Union.Case(...)` is available for explicit qualification.
* `.Case(...)` remains available as target-typed shorthand when the scrutinee
  already determines the union.
* Parenthesized unions use ordinary patterns over their declared member types.

### Canonical case-construction forms

Raven supports the following equivalent case-construction forms:

```raven
// Case type construction, when the case is imported
Ok(2)
Ok<int>(2)

// Union-member sugar
Result<int, MyError>.Ok(2)

// Target-typed member-binding sugar
val r: Result<int, MyError> = .Ok(2)
```

Binding model:

* `Case(...)` constructs the case type value directly when the case is in
  scope through a type wildcard import, direct case import, alias, or generated
  prelude import.
* Unqualified `Case(...)` is allowed when imported case resolution is
  unambiguous; otherwise a qualified form (`Union.Case(...)`) or target-typed
  member form (`.Case(...)`) is required.
* `Union.Case(...)` resolves `Case` from the union’s declared case set, then
  constructs the case value.
* `.Case(...)` resolves `Case` from the target type’s union case set.
* For an unqualified identifier in expression position, ordinary lexical lookup
  wins before imported union-case lookup: locals and parameters first, then
  visible instance/static members and imported symbols, then unqualified union
  cases made visible by imports.
* If a union value is required, case-to-union conversion applies implicitly by
  constructing the matching carrier value from the case value.

Union invariants:

* Plain `union` declares a struct carrier by default. `union struct` is explicit
  spelling for the same carrier category, and `union class` opts into a
  reference carrier.
* Case constructors are independent case-type constructors; they are not
  rebound as union constructors.
* `union struct` reserves its default state as an uninitialized carrier. For
  `default(U)`, `Value` is `null`, `HasValue` is `false`, and no case is active
  until a union constructor populates the carrier.
* The default `union struct` carrier state is not a formal union case. Pattern
  exhaustiveness checks the declared case set only. Lowering and emit must still
  preserve a defensive runtime fallback for source-exhaustive matches so
  metadata consumers or forced default carriers cannot fall through silently.
* Nullable union carriers (`U?`) add the nullable wrapper's `null` value to the
  source match domain. A match over `U?` must cover the declared union cases and
  `null`, or use a catch-all. This nullable `null` value is separate from the
  inactive/default carrier state of `union struct`.
* Function parameters and `self` of `union struct` type are active inside the
  callee because the call boundary rejects possibly inactive arguments before
  entry. Matching or forwarding them does not require an extra source catch-all.
* Fields and properties of `union struct` type are storage/interop boundaries
  that may still contain the inactive/default carrier unless narrowed by local
  flow. Passing or returning one of those values requires an active-state proof
  at the boundary rather than an extra source match arm.
* Local values initialized from a union case or assigned an active union value
  are known active. Matching such a local requires only the declared case set;
  a catch-all arm after all cases is redundant.
* Passing a `union struct` value to a `union struct` parameter requires the
  argument to be known active at the call site. A value that flow analysis knows
  may still be the inactive/default carrier is rejected before entering the
  callee. Omitting an optional argument whose default value is the carrier
  default is also rejected at the call site.
* Returning a `union struct` value from a function or property requires the
  value to be known active at the return boundary. A value that flow analysis
  knows may still be the inactive/default carrier is rejected before it leaves
  the declaring member.
* `union class` does not have that extra carrier state; a class carrier exists
  only after construction through one of its union cases or constructors.
* For ordinary class carriers with no nullable active member state, `null` is
  not a valid pseudo-case for `Value`.
* `HasValue` follows the public C# union access pattern and is equivalent to
  `Value != null`. Raven does not expose active-null contents as a separate
  public `HasValue` state.
* Union wrapping is represented by carrier construction from a case value.
* Compatibility is decided by case-to-union conversion rules (including
  payload subtype-to-supertype widening where valid).

Type argument behavior:

* Case type arguments may be explicit (`Ok<int>(2)`) or inferred from
  constructor arguments (`Ok(2)`).
* Union type arguments are taken from explicit receiver types
  (`Result<int, MyError>.Ok(2)`) or from target typing
  (`val r: Result<int, MyError> = .Ok(2)`).

For every case `Case`, assigning, returning, or passing a case value
automatically produces the union carrier through case-to-union conversion.
Member-qualified case construction still constructs the case first and then
converts to the carrier when the surrounding context requires the union value:

```raven
val ok: Result<int, string> = Ok(99)          // implicit case-to-union conversion
val err = Result<int, string>.Error("boom")
Console.WriteLine(ok)
```

Each case struct also exposes its payload via `get`-only properties and a
`Deconstruct(out ...)` method matching the payload order. These synthesized
members make deconstruction and positional patterns available in Raven and
improve interoperability with other .NET languages.

Pattern matching exhaustively checks every case; see
[Pattern matching](pattern-matching.md) for case-pattern forms (unqualified
`Case`, `Union.Case`, and `.Case`) inside `match` expressions.

### Closed-shape types

Raven has three primary ways to model a finite, closed set of alternatives:

1. **Unions** (`union`)
2. **Enums** (`enum`)
3. **Sealed hierarchies** (`sealed class` / `sealed record class`)

Each can participate in exhaustiveness analysis for `match`, and each represents
a known closed shape at compile time. The key difference is modeling style:

| Use this | When you need |
| --- | --- |
| `union` | Algebraic data modeling with explicit case payloads, carrier-based construction/extraction (`Ok(...)`, `.Ok(...)`, `TryGetValue`), and closed alternatives. |
| `enum` | Named constants over a single integral value domain, numeric interop, flags-style values, or compact status codes with no case payloads. |
| `sealed` hierarchy | Object-oriented subtype modeling with shared base behavior, virtual/interface-style design, and class hierarchy semantics. |

#### Choosing between them

Choose **unions** when:

* the alternatives are primarily data cases,
* payloads are part of the case definition,
* construction/pattern matching is the dominant interaction,
* parameterless alternatives are still semantic tagged cases rather than named
  numeric constants.

Choose **enums** when:

* every alternative is just a name for an integral constant,
* numeric representation, ordering, bitwise flags, or .NET enum interop matters,
* no alternative carries payload data or needs a distinct generated case type.

Choose **sealed hierarchies** when:

* you are modeling a class family,
* variants share behavior through a base type,
* subtype polymorphism is part of the design.

Both are "closed-shape" constructs; prefer the one that matches your domain
modeling style rather than forcing a single pattern for all cases.
