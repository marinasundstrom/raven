# Proposal: Unit type

> ⚠️ This proposal has **NOT** been implemented

This document outlines the `unit` type.

The `unit` type represents a value in absence of a value. It can be seen as `void` but the difference is that its a real type, and not just discarded.

The keyword `unit` maps to type `System.Unit` (see definition below)

## Purpose

Help types flow. Being implicit by handling a concrete value for empty, that is not `null` or `void`.

## Syntax

```raven
let v = Foo(); //v == unit

func Foo() -> unit {
    unit // return unit
}
```


### `unit` type

Unit can be specified as a type specifier.

```raven
let x : unit
func Foo() {} // defaults to -> unit
func Foo() -> unit {} // explicitly unit
func Foo(x: unit) {} // unit in an argument
```

The specifier can be aliased:

```raven
alias MyUnit = unit 
```

And is valid in any other type syntax, such as type unions:

```raven
func Foo(ok: bool) -> int | unit {
    if ok {. 
        return 42
    }
    return unit
}
```

Even tuples.

### `unit` literal expression

For cases when you want to be explicit with `unit`.

```raven
let x = unit
```

## Semantics

`unit` is to be regarded as a builtin type, with one value.

### Expressions

Control flow expressions (`if`, `while`, `for` etc) and block implicitly returns `unit` unless there is another type, or `unit` is explicit in any of the flows.

### Type unions

`unit` participates in type unions.

```raven
var ok = true
let result = if ok { return 42 } else { return unit } 

// the type of 'result' is a union between int and unit (int | unit).
```

## Interop

### From Raven

Return parameters with type `void` will be projected as `unit` inside of Raven. The developer will see `unit` and handle it as such.

### To C# and other .NET languages

By default, Raven will be the outwards type expose `unit` (`System.Unit`).

```raven
func Foo() { // returns unit
    
}
```

But if you want a method to return `void` you explicitly tell Raven to do so.

```raven
func Foo() -> void { // returns void
    
}
```

Raven will still handle `void` as if it was `unit`, but the generated IL will use `void`. This is an advantage when building class libraries shared by other languages.

## `Unit` struct definition

The `unit` keyword maps to the `System.Unit` struct.

The definition of `Unit` in C#:

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

The default instance of `Unit` is `Unit.Value`. And literal expression `unit` maps to `Unit.Value`.

> This type will in the future live in a shared assembly (such as `Raven.Core`). But will be initially generated and embedded in the output assembly.