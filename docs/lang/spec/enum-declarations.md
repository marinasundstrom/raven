# Enum declarations

An `enum` declaration defines a set of **named constants** backed by an integral
underlying type. Use an enum for a small set of choices that do not carry
additional data.

```raven
enum Color {
    Red
    Green
    Blue
}
```

Enums represent named constants only. They do not support associated payloads
or per-member structure.

Raven enums are **closed by default**: the declaration's named members are the
complete source-level value set for exhaustiveness analysis. There is no
`closed` modifier. A `match` over an enum must cover every declared member or
include a catch-all arm.

```raven
func Describe(color: Color) -> string {
    return color match {
        .Red => "red"
        .Green => "green"
        .Blue => "blue"
    }
}
```

Adding a member to a published enum is therefore a source-breaking change for
consumers that exhaustively match it. This is intentional: Raven favors making
new domain alternatives visible at their decision points.

The CLR representation can still contain an unnamed underlying integer value,
particularly when it crosses an interop, serialization, or unsafe boundary.
Raven-generated matching code retains a runtime failure path for such invalid
values even when every declared member is covered. A catch-all arm may be used
when an application wants to recover explicitly at that boundary.

Raven does not currently provide an `open enum` declaration. Imported enum
metadata is matched against its declared members under the same rule. A future
.NET closed-enum metadata contract may let Raven distinguish closed and open
foreign enums without changing Raven-authored syntax.

### Underlying type

An enum may optionally specify an explicit underlying type using a base list after
the enum name:

```raven
enum Status : byte {
    Ok = 1
    Error = 2
}
```

If no underlying type is specified, the underlying type defaults to `int`.

Only a single underlying type may be specified; additional types report
`RAV0410`. The underlying type must be a
non-nullable integral primitive type (`byte`, `sbyte`, `short`, `ushort`, `int`,
`uint`, `long`, `ulong`, or `char`). Any other type reports `RAV0411`.

### Enum members

Each enum member introduces a public constant whose type is the enclosing enum.
Symbol signatures display that declaration with its underlying constant value,
for example `const field Rising: PinEventTypes = 1`.

Enum members carry no associated payload or structure beyond their constant
value. They cannot declare fields, parameters, or additional data.

Enum members may be referenced with their qualified name or with target-typed
member syntax when an expected enum type is available:

```raven
let target: AttributeTargets = AttributeTargets.Delegate
let shorthand: AttributeTargets = .Delegate

if target is AttributeTargets.Delegate {
}

if target is .Delegate {
}
```

Equality comparisons use qualified enum members:

```raven
if target == AttributeTargets.Delegate {
}
```

Type wildcard imports include enum members in unqualified value scope. For
example, `import System.AttributeTargets.*` makes `Delegate` available as a
simple name. Individual enum members can also be imported explicitly:

```raven
import System.AttributeTargets.Delegate
```

Explicit enum-member imports are deliberate and use the same precedence as other
specific imports. Qualified and target-typed enum access remain supported.

An enum member may optionally declare an explicit value using `=` followed by a
constant expression that is convertible to the enum’s underlying type:

```raven
enum ErrorCode : int {
    None = 0
    NotFound = 404
    Timeout = 405
}
```

If an enum member does not specify a value, its value is implicitly defined as one
greater than the previous member. The first member defaults to zero when no
explicit initializer is present.

Enum member initializers must be constant expressions and may reference
previously declared members. A non-constant initializer reports `RAV0412`; a
value that cannot be converted to the underlying type reports `RAV0413`.

### Conversions

An explicit conversion exists from an enum type to its underlying type:

```raven
let code: int = (int)ErrorCode.NotFound
```

The reverse conversion—from the underlying type to the enum type—requires an
explicit cast and is not validated for named membership at compile time.

### Enums vs. discriminated unions

Enums and discriminated unions both name a finite set of concepts, but they
model different things.

Use an enum when the value is fundamentally a named numeric value:

* the runtime representation must be a CLR enum;
* values may be cast to or from an underlying integer type;
* the type is used for flags, bit operations, or metadata/API interop; or
* the names are labels for stable numeric values rather than distinct data
  variants.

Use a discriminated union when modeling a *closed* set of alternatives where:

* individual cases need to carry associated data; or
* adding a new alternative should be visible through match exhaustiveness
  diagnostics.

Even when every union case is payload-free, a body-form union is still a tagged
union, not an enum. Each case is a distinct semantic case in the union's closed
case set and participates in case construction, case-to-carrier conversion, and
match exhaustiveness. The cases are not integer constants, do not have an
underlying numeric type, and are not interchangeable with enum members.

```raven
enum Direction {
    North
    South
}

union Command {
    case Start
    case Stop
    case Fail {
        Message: string
        Code: int? = null
    }
}
```

Union cases support three payload shapes: a bare case is unit-like, a
parenthesized case is tuple-like, and a braced case is struct-like with named
payload fields. Braced case fields without defaults are required during case
construction; fields with defaults are optional. The field block declares the
case payload shape and does not introduce mutable object-initializer semantics.

`Direction.North` is a named closed CLR enum value. `Command.Start` is a union case
value that can be converted to the `Command` carrier and matched as one of the
declared cases. Both declarations have closed alternatives and participate in
exhaustiveness; only the union expresses a tagged domain with distinct semantic
cases and optional payloads.

### Runtime representation

At runtime, enums are emitted as CLR enum types. The compiler emits a special
instance field named `value__` whose type is the enum’s underlying type. Each
enum member is emitted as a public static literal field whose constant value is
stored using the underlying type.
