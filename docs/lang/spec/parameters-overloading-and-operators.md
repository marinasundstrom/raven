# Parameters, overloading, and operators

Parameters describe the values a function accepts. Defaults and overloads let
an API support common variations without requiring a different name for every
call, while operators let suitable domain types use familiar expression syntax.

Method, constructor, and accessor parameters are immutable by default. They
behave like `let` bindings: the compiler rejects assignments that attempt to
rebind the parameter name. Add the `var` modifier when a parameter must be
reassigned inside the body—for example, to reuse a scratch variable or to
satisfy an `out` contract.

```raven
func clamp(min: int, value: int, max: int) -> int {
    // value = ...    // error: parameters are immutable by default
    return Math.Max(min, Math.Min(value, max))
}

func TryParse(text: string, out result: int) -> bool {
    result = 0      // ok: the parameter explicitly opts into mutation
    /* ... */
}
```

Declaring a parameter with `ref`, `out`, or `in` passes the argument by
reference. The callee receives an alias to the caller's storage and callers
supply such arguments with the address-of operator `&expr`. Plain parameters are
readonly. `ref` parameters can be read and assigned, `in` parameters are
readonly aliases, and `out` parameters must be assigned before the method
returns. These modifiers already imply by-reference passing, so their declared
types stay plain: use `ref value: int`, not `ref value: &int`. Explicit
`&Type` parameters remain available when the by-reference type itself is the
intended type annotation.

```raven
func Increment(ref value: int) -> () {
    value = value + 1
}

var total = 41
Increment(ref total)
Console.WriteLine(total) // prints 42
```

## Method overloading

Use overloading when the same operation makes sense for different sets or types
of arguments.

Functions and methods may share a name as long as their parameter counts or
types differ. Overload resolution selects the best match based on argument
types, `out`/by-ref modifiers, and nullability. Ambiguous calls produce a
diagnostic.

```raven
class Printer {
    func Print(x: int) -> () => Console.WriteLine(x)
    func Print(x: string) -> () => Console.WriteLine(x)
}

Print(42)
Print("hi")
```

## Default parameter values

Use a default value when most callers should be able to omit an optional
argument.

Methods, constructors, and other function-like members may specify default
values for trailing parameters using `= expression`. Optional parameters follow
the same rules as top-level functions: once a parameter provides a default, all
subsequent parameters in the list must also supply defaults. The expression is
restricted to compile-time constants—literals (including `null`), parenthesized
literals, or unary `+`/`-` applied to numeric literals—and the resulting value
must convert to the parameter type using an implicit conversion. When the
expression fails these checks, the compiler reports an error and treats the
parameter as required.

## Operator declarations

Types can define operators when an operation has a clear meaning in ordinary
expression syntax, such as adding two vectors.

Classes and structs can declare overloadable operators using function-style
syntax where the operator token is the function name:
`static func <operator>(...) -> ...`.
Supported tokens are `+`, `-`, `*`, `/`, `%`, `^`, `&`, `&&`, `and`, `|`, `||`,
`or`, `<<`, `>>`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `!`, `~`, `++`, `--`.
Operators mirror methods: they take a parenthesized parameter list, optional
return-type arrow, and either a block body or expression body. The parameter
count must match the chosen operator (unary or binary). Operator declarations
are supported in classes, structs, and extensions.

```raven
class Vector {
    static func +(left: Vector, right: Vector) -> Vector => Add(left, right)
    static func -(value: Vector) -> Vector { /* ... */ }
}
```

Conversions follow the same style:
`static func implicit(value: SourceType) -> TargetType` and
`static func explicit(value: SourceType) -> TargetType`.
These conversion members are resolved using the same lookup rules as other
static members.

For null checks, prefer `is null` / `is not null` when you need strict
nullability narrowing. Raven's analyzer recommends these forms over
`== null`/`!= null` and provides a code fix. Pointer-like comparisons are
excluded from that recommendation.

## Invocation operator

An invocation operator makes an object callable. This is useful for objects
whose main job is to apply a configured operation.

Declaring a method named `self` makes instances of the type invocable with the
call operator `()`.

```raven
class Adder {
    func self(x: int, y: int) -> int => x + y
}

val add = Adder()
val sum = add(1, 2) // calls self(1, 2)
```

Invocation operators can themselves be overloaded by providing multiple `self`
methods with different parameter signatures.
