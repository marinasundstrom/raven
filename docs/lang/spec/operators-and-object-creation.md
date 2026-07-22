# Operators and object creation

Prefixing an expression with `^` produces a `System.Index` value that counts
from the end of a sequence. The operand must be implicitly convertible to
`int`, and the result keeps its `Index` type even when not target-typed, so
`val offset = ^2` is valid without annotations. When indexing arrays, from-end
indices are computed using the array's length and are evaluated exactly once
alongside the receiver.

`^` is parsed as a tight prefix form: `^expr`. Trivia between `^` and the
operand is rejected, so `^1` is valid while `^ 1` is not.

## Range expressions

`..` produces an inclusive-notation `Range` form, and `..<` produces a half-open
form with an exclusive upper bound. Both can be stored, passed to APIs, or used for
slicing receivers that understand .NET ranges. Both endpoints are optional, and
either endpoint may be written as a **from-end** index by prefixing it with `^`.

```raven
val r = 3..^5
val halfOpen = 3..<10
val head = ..3
val tail = 3..
val all  = ..
```

Forming a range evaluates each supplied boundary exactly once, left-to-right.
Each boundary expression must be implicitly convertible to `int`; the `^` prefix
indicates the operand counts from the end of the receiver instead of from the
start. The resulting `Range` uses `Index.FromStart` for ordinary boundaries and
`Index.FromEnd` for prefixed ones, and omitting a boundary produces
`Range.StartAt`, `Range.EndAt`, or `Range.All` accordingly. A range expression
retains its `Range` type even when no target type is provided, enabling
declarations like `val r = 3..^5` without additional annotations.

Element access applies these types directly:
- One-dimensional arrays accept a single `Range` argument (`array[range]`) and
  produce a sliced array.
- Non-array receivers resolve `[]` using argument-type matching, so indexers
  that differ by `Index` vs `Range` can coexist.

## Bitwise operators

Raven supports unary bitwise-not `~` plus binary `&`, `|`, `^`, `<<`, and `>>`.

- `~` is defined for `int` and `long`.
- `&`, `|`, and `^` are defined for `int`, `long`, `bool`, and matching enum operands.
- `<<` and `>>` are defined for `int` and `long` left operands with an `int` shift count.

When both operands are `bool`, `&`, `|`, and `^` evaluate **without** short-circuiting and return `bool`, allowing direct use in non-conditional contexts or within compound assignments. Operands must share the same enum type when applied to enums; the result has that enum type.

Compound assignments `&=`, `|=`, and `^=` are available and apply the corresponding binary operator after evaluating the left-hand side once. These operators share left-to-right associativity with other Raven binary operators, and their precedence sits between the logical (`||`, `&&`) and equality operators.

Enum member accesses support **leading-dot** syntax when a target type is already known, including inside bitwise combinations and argument lists:

```raven
val flags: BindingFlags = .NonPublic | .Static

func WithBinding(flags: BindingFlags) { /* ... */ }

WithBinding(.Public | .Instance)
```

## Object creation

Objects are created by **calling the type name** directly, just like any
other method.

```raven
val sb = StringBuilder()
sb.AppendLine("Foo")
```

Generic types work the same way:

```raven
val list = List<int>()
list.Add(2)
```

When a generic type name is called without explicit type arguments, Raven may
infer the type arguments from constructor arguments. Constructor inference uses
the same argument-binding rules as method calls, including delegate target
typing for function expressions.

```raven
open class Endpoint {
    init(handler: Delegate) {}
}

class GET<T> : Endpoint {
    init(pattern: string, handler: T -> string) : base(handler) {}
}

val route = GET("/{id:int}", func (id: int) => id.ToString())
// route : GET<int>
```

If a non-generic type and a generic type share the same name, an applicable
non-generic constructor remains preferred. If the non-generic constructor is not
applicable and exactly one generic type can be constructed from the arguments,
that generic construction is selected. If multiple generic type candidates infer
successfully, the call is ambiguous and must be disambiguated with explicit type
arguments or a different type name.

> A standalone type name is not a constructor call in value position.
> Use `Foo()` instead of `Foo`.

The object-initializer `with` form may be used for parameterless construction:

```raven
val window = Window with {
    Title = "Main"
}
```

This form is equivalent to calling `Window()` and then applying the initializer entries in order.

## Tuple expressions and access

Tuples can be **named** or **positional**. Both projections are available.

```raven
val tuple = (a: 42, b: 2)
Console.WriteLine(tuple.a)      // named
Console.WriteLine(tuple.Item1)  // positional
```
