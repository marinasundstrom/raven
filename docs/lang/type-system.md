# Raven type system

Raven is a statically typed language whose types correspond directly to CLR types. The compiler uses .NET type symbols so that every Raven type has a concrete runtime representation.

## Primitive types

| Raven keyword | .NET type | Notes |
| --- | --- | --- |
| `int` | `System.Int32` | 32-bit signed integer |
| `string` | `System.String` | UTF-16 sequence of characters |
| `bool` | `System.Boolean` | logical true/false |
| `char` | `System.Char` | UTF-16 code unit |
| `unit` | `System.Unit` | single value `()` representing "no result" |
| `null` | *(null literal)* | inhabits any nullable reference type |

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

Values may convert to other types according to .NET rules. Implicit conversions include identity, `null` to any nullable type, lifting value types to their nullable counterpart, widening numeric conversions, reference conversions to base types or interfaces, boxing of value types, and conversions to a matching branch of a union. Narrowing or otherwise unsafe conversions require an explicit cast. See [type compatibility](proposals/type-compatibility.md) for a detailed list of conversion forms.

## Overload resolution

When multiple function overloads are available, Raven selects the candidate whose parameters require the best implicit conversions. Identity matches are preferred over numeric widening, which outrank reference or boxing conversions. User-defined conversions are considered last. Literal arguments convert to their underlying primitive type before the ranking is applied. If no candidate is strictly better, the call is reported as ambiguous.

