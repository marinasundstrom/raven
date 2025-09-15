# Raven type system

Raven is a statically typed language whose types correspond directly to CLR types. The compiler uses .NET type symbols so that every Raven type has a concrete runtime representation.

## Primitive types

| Raven keyword | .NET type | Notes |
| --- | --- | --- |
| `int` | `System.Int32` | 32-bit signed integer |
| `string` | `System.String` | UTF-16 sequence of characters |
| `object` | `System.Object` | base type of all .NET reference types |
| `bool` | `System.Boolean` | logical true/false |
| `char` | `System.Char` | UTF-16 code unit |
| `unit` | `System.Unit` | single value `()` representing "no result" |
| `null` | *(null literal)* | inhabits any nullable reference type |

## Literal types

Numeric, string, character, and boolean literals may appear as their own types.
A literal type represents exactly that value and carries an underlying
primitive type—`1` has underlying type `int` while `"hi"` has underlying type
`string`. Literal types are considered subset types of their underlying primitive,
so every value of a literal type is also a value of that primitive type. Literal
expressions are given these singleton types. These singleton types act as
value-level constraints, most often used as branches in union
types or other constructs that restrict a value to specific constants.

```raven
let value: "yes" | "no" = "yes"

alias Switch = "yes" | "no"
let value: Switch = "yes"
```

Supported literal types are:

| Literal example        | Underlying type | Notes                     |
|------------------------|-----------------|---------------------------|
| `true`, `false`        | `bool`          | boolean constants         |
| `'a'`                  | `char`          | single UTF-16 code unit   |
| `"hi"`                | `string`        | sequence of characters    |
| `1`                    | `int`           | 32-bit signed integer     |
| `4_000_000_000`        | `long`          | promoted when `int` overflows |
| `3.14`                 | `double`        | default floating literal  |
| `3.14f`                | `float`         | `f` or `F` suffix selects `float` |

Literal types implicitly convert to their underlying type and then follow the
normal conversion rules of that type. This allows `1` to widen to `double` or
`"hi"` to be used wherever a `string` is expected.

When a literal is assigned to a target whose type is inferred—such as a
variable declaration without an explicit type annotation—the literal widens to
its underlying primitive type. When inference gathers multiple literal values
into a union (for example, via conditional branches), those literal members are
preserved so the inferred union reports the exact set of constants that may
flow to that location. This keeps single-literal inference in line with other
languages that treat `let x = 1` as `int` while still modeling literal unions.

```raven
let yes: "yes" = "yes"
let one: 1 = 1
let two: int = one      // implicit conversion to int
let d: double = one     // underlying int widens to double
let inferred = 1        // inferred int, literal type is widened
```

## Composite and derived types

### Arrays

`T[]` becomes `System.Array` with element type `T`.

### Tuples

`(T1, T2, ...)` map to `System.ValueTuple<T1, T2, ...>`.

### Nullable values

Appending `?` creates a nullable type. Value types are emitted as `System.Nullable<T>` while reference types use C#'s nullable metadata.

### Union types

`A | B` represents a value that may be either type. Each branch retains its own CLR representation and the union's base type is inferred from the operands. This common denominator is used whenever a single type is required, such as overload resolution.

Common use cases include mixing unrelated primitives, modeling optional values, or constraining a value to specific literals:

```raven
let a: int | string = "2"   // either an int or a string
let b: string | null = null // optional string (equivalent to string?)
let c: "yes" | "no" = "yes" // constrained to specific constants
```

When assigning to a union, the expression must convert to at least one branch. Literal branches are matched by value rather than by type:

```raven
let d: "true" | 1 = 1   // ok
let e: "true" | 1 = 2   // error: Cannot assign '2' to '"true" | 1'
let f: "true" | int = 1 // ok: 1 matches int
```

### Generics

Generic parameters compile directly to .NET generics:

```raven
func identity<T>(value: T) -> T { value }
```

## Interoperability

Because Raven reuses .NET types, existing libraries can be consumed seamlessly:

```raven
let ids: Guid[] = [Guid.NewGuid()]
Console.WriteLine(ids[0])
```

## Conversions

Values may convert to other types according to .NET rules. Implicit conversions
include identity, literal types to their underlying primitive type, `null` to any
nullable type, lifting value types to their nullable counterpart, widening numeric
conversions, reference conversions to base types or interfaces, boxing of value
types, and conversions to a matching branch of a union. Narrowing or otherwise
unsafe conversions require an explicit cast. See
[type compatibility](../proposals/type-compatibility.md) for a detailed list of
conversion forms.

### Explicit casts

Raven uses C#-style cast syntax for conversions that are not implicit, such as downcasting or numeric narrowing:

```raven
let d = (double)1
let n = (int)3.14
let s = obj as string
```

`(T)expr` performs a runtime-checked cast and throws an `InvalidCastException` if `expr` cannot convert to `T`.
`expr as T` attempts the conversion and yields `null` (or a nullable value type) when it fails.

## Overload resolution

When multiple function overloads are available, Raven selects the candidate whose
parameters require the best implicit conversions. Identity matches are preferred
over numeric widening, which outrank reference or boxing conversions.
User-defined conversions are considered last. An argument of a literal type is an
exact match for a parameter of the same literal type; otherwise the literal is
converted to its underlying primitive type before the ranking is applied. If no
candidate is strictly better, the call is reported as ambiguous.

Arguments with union types participate using the union's base type. The compiler
does not test each branch individually; instead, it ranks conversions from the
union's common denominator. This matches IL emission and ensures a union selects
the overload that best matches its shared base type:

```raven
func print(x: object) -> () {}
func print(x: int) -> () {}

let u: int | string = "hi"
print(u) // calls print(object)
```

