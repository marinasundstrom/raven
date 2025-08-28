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

