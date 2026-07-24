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
or per-member structure. `match` expressions over enums are not required to be
exhaustive, and the compiler does not enforce completeness.

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

Only a single underlying type may be specified. The underlying type must be a
non-nullable integral primitive type (`byte`, `sbyte`, `short`, `ushort`, `int`,
`uint`, `long`, `ulong`, or `char`). Specifying any other type is a compile-time error.

### Enum members

Each enum member introduces a public constant whose type is the enclosing enum.

Enum members carry no associated payload or structure beyond their constant
value. They cannot declare fields, parameters, or additional data.

Enum members may be referenced with their qualified name or with target-typed
member syntax when an expected enum type is available:

```raven
val target: AttributeTargets = AttributeTargets.Delegate
val shorthand: AttributeTargets = .Delegate

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

Enum member initializers must be constant expressions. They may reference previously
declared enum members. References to non-constant values are invalid, and values
that cannot be represented in the underlying type are compile-time errors.

### Conversions

An explicit conversion exists from an enum type to its underlying type:

```raven
val code: int = (int)ErrorCode.NotFound
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

* every case must be handled exhaustively;
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

`Direction.North` is a named CLR enum value. `Command.Start` is a union case
value that can be converted to the `Command` carrier and matched as one of the
declared cases. Both declarations have two named alternatives, but only the
union expresses a closed tagged domain for exhaustiveness over semantic cases.

### Runtime representation

At runtime, enums are emitted as CLR enum types. The compiler emits a special
instance field named `value__` whose type is the enum’s underlying type. Each
enum member is emitted as a public static literal field whose constant value is
stored using the underlying type.
