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

The `scoped` modifier restricts a parameter so that references obtained from it
cannot escape the call. It precedes the by-reference modifier:
`scoped value: Span<int>` classifies a scoped ref-like value, while
`scoped ref value: int` classifies a scoped reference. Raven follows the C#
`scoped` lifetime model and exposes the distinction as `ScopedValue` and
`ScopedRef` through the compiler symbol API.
Returning a scoped ref-like parameter, or a local alias of it, is rejected
because that would let the restricted value escape the function.
By-value `scoped` parameters must be ref-like; `scoped ref`, `scoped in`, and
`scoped out` parameters may refer to ordinary value types.
As in C#, `out` parameters and `ref` parameters whose type is ref-like are
implicitly scoped even when the keyword is omitted.

Local bindings use Raven's declaration order: `scoped val buffer: Span<int>`
declares a scoped ref-like value, and `scoped val reference = &value` declares
a scoped reference. The local symbol reports `ScopedValue` or `ScopedRef`
according to its resulting type.
Applying `scoped` to an ordinary value local is invalid; scoped value locals
must be ref-like, while scoped reference locals must have a by-reference type.
Scoped ref-like locals cannot be returned directly or through an ordinary local
alias, regardless of whether their initializer originally referred to
caller-owned storage.
The restriction follows scoped values stored inside ref-like fields, so
returning a containing ref struct does not bypass the local's scope.
Ref-like invocation results inherit the scope of receiver and argument values
that flow through unscoped parameters. Arguments supplied to scoped parameters
do not contribute to the result's escape scope.
Scoped parameters and locals cannot be captured by lambdas or local functions,
including scoped references whose element type is not itself ref-like.

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
