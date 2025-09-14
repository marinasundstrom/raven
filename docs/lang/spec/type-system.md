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

Numeric and string literals may appear as their own types. A literal type
represents exactly that value and carries an underlying primitive type—`1`
has underlying type `int` while `"hi"` has underlying type `string`. Literal
expressions are given these singleton types. These singleton types act as
value-level constraints, most often used as branches in union types or other
constructs that restrict a value to specific constants.

Literal types implicitly convert to their underlying type and then follow the
normal conversion rules of that type. This allows `1` to widen to `double` or
`"hi"` to be used wherever a `string` is expected.

When a literal is assigned to a target whose type is inferred—such as a
variable declaration without an explicit type annotation—the literal widens to
its underlying primitive type. This keeps type inference in line with other
languages that treat `let x = 1` as `int` rather than the literal type `1`.

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

`A | B` represents a value that may be either type. Each branch retains its own CLR representation and the union's base type is inferred from the operands.

When assigning to a union, the expression must convert to at least one branch. Literal branches are matched by value rather than by type.

```raven
let a: "true" | 1 = 1   // ok
let b: "true" | 1 = 2   // error: Cannot assign '2' to '"true" | 1'
let c: "true" | int = 1 // ok: 1 matches int
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

Raven uses C#-style cast syntax for conversions that are not implicit:

```raven
let n = (double)1
let s = obj as string
```

`(T)expr` performs a runtime-checked cast and throws if `expr` cannot convert to `T`.
`expr as T` attempts the conversion and yields `null` (or a nullable value type) when it fails.

## Overload resolution

When multiple function overloads are available, Raven selects the candidate whose
parameters require the best implicit conversions. Identity matches are preferred
over numeric widening, which outrank reference or boxing conversions.
User-defined conversions are considered last. An argument of a literal type is an
exact match for a parameter of the same literal type; otherwise the literal is
converted to its underlying primitive type before the ranking is applied. If no
candidate is strictly better, the call is reported as ambiguous.

