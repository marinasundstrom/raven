# .NET Implementation Notes

This document outlines how Raven constructs map to the .NET runtime and metadata. For language semantics, see [language-specification.md](language-specification.md).

## Unit type
When interacting with .NET, methods that return `void` are projected as returning `unit`, and Raven's `unit` emits as `void` unless the value is observed. After any call that returns metadata `void`, the compiler loads `Unit.Value` so the invocation still produces a `unit` result. In an expression statement that value is discarded, enabling nested `unit`-returning calls such as `Console.WriteLine(Console.WriteLine("foo"))`. The `unit` type is a value type (`struct`) and participates in generics, tuples, and unions like any other type.

## Return statements
A `return` without an expression in a method that returns `unit` emits IL with no value. If the underlying method returns `void`, `Unit.Value` is loaded to produce a `unit` result before the `ret` instruction.

## Union types
When emitted to .NET metadata, a union is projected as the narrowest common denominator of its members. If every member shares a base class, that base type becomes the metadata type; otherwise, `object` is used. Including `null` in the union marks the emitted type as nullable.

For example:

```raven
let pet = if flag { Dog() } else { Cat() } // Dog | Cat
```

Emits `Animal` because both `Dog` and `Cat` derive from it. In contrast:

```raven
let value = if flag { 0 } else { "hi" } // int | string | null
```

Emits `object?` since `int` and `string` share no base class other than `object`, and `null` is included.

This narrowing makes unions friendlier to inheritance-based languages such as C#, and it gives the runtime a smaller set of types to resolve. The `TypeUnionsAnalyzer` provides additional hints about possible targets so that consumers can work with the projected type more effectively.

To preserve the original union members, the compiler also attaches a `TypeUnionAttribute` to the parameter or return type in metadata. The attribute lists the CLR `Type` for each member in the union. The method signature itself uses the narrowed base type (or `object`) as described above.

For example:

```raven
func f(x: string | unit | null) -> unit { }
```

Emits a parameter of type `object?` with:

```csharp
[TypeUnionAttribute(typeof(string), typeof(Unit), typeof(Null))]
```

attached, indicating the full set of possible values.

Raven emits shim types so that every union member has a concrete `Type`:

* `Unit` represents the Raven `unit` value and is emitted into every assembly.
* `Null` represents the `null` literal and is emitted only when a union includes `null`.

