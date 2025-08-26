# Proposal: Unit type (MVP)

⚠️ This proposal has **NOT** been implemented

The `unit` type represents a value in absence of a value. It can be seen as `void` but the difference is that it is a real type with exactly one value.

The keyword `unit` maps to the type `System.Unit` (see definition below).

## Purpose

- Enable type flow where `void` cannot be used (generics, tuples, unions).
- Provide a concrete value for "empty", distinct from `null` or `void`.

## Syntax

```raven
let v = Foo()   // v : unit

func Foo() -> unit {
    unit // return unit
}
````

* `unit` is a valid type specifier.
* Functions without explicit return type default to `unit`.
* The literal `unit` refers to the single value of this type.

## Semantics

* `unit` is a builtin type with one value.
* Control-flow and block expressions return `unit` when no other type is inferred.
* `unit` may appear in unions and tuples.

```raven
func Foo(ok: bool) -> int | unit {
    if ok {
        return 42
    }
    return unit
}
```

## Interop

* **From Raven**: Methods that return `unit` emit as `void` by default, unless a real type is required (e.g. generics).
* **To Raven**: External `void` return types are projected as `unit`.

## `Unit` struct definition

```csharp
public readonly struct Unit : IEquatable<Unit>
{
    public static readonly Unit Value = default;
    public bool Equals(Unit other) => true;
    public override bool Equals(object? obj) => obj is Unit;
    public override int GetHashCode() => 0;
    public override string ToString() => "()";
}
```

The literal `unit` maps to `Unit.Value`.

> Initially this type will be embedded in Raven assemblies, later moved to a shared core library.