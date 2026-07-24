# Calls

Calls run functions and methods with a set of arguments. Raven uses the same
call syntax for function values, constructors, and types that define an
invocation operator.

```raven
Foo(1, 2)
Console.WriteLine("Test")
```

The `()` call operator invokes a function-valued expression. If the target
expression's type defines an invocation operator via a `self` method, that
member is invoked instead. Invocation operators can be declared on classes or
interfaces. Class declarations may mark them `virtual` or `abstract` to support
overrides.

When the target has optional parameters, omitted trailing arguments are filled
using the defaults declared on the parameter list. Supplied arguments are
matched positionally before defaults are considered.

Parameters may be declared as collectors using a trailing `...` after the
parameter type. A collector parameter must be the final parameter. The
convenience form `items: T ...` binds as `IList<T>`; use explicit `params`
syntax, such as `params items: int[]`, to control the collection type.

At call sites, extra positional arguments are packed into the collector.
`...expr` expands an existing sequence into it.

Arguments may be named with `name: expression`. Named arguments may appear in
any order, but positional arguments after a named argument must correspond to
parameters after the right-most named argument and must not duplicate an
already supplied parameter. Unknown and duplicate names reject the candidate.
This syntax applies to functions, object creation, constructor initializers,
and attributes; `name = expression` is not call-argument syntax.

```raven
func makePoint(x: int, y: int, label: string = "origin") -> string {
    return "$label: ($x, $y)"
}

func sum(items: int ...) -> int {
    return items.Length
}

val swapped = makePoint(y: 2, x: 1)
val mixed = makePoint(3, label: "axis", y: 0)
val invalid = makePoint(x: 1, 2) // error
val total = sum(1, 2, 3)
val values = [4, 5]
val expanded = sum(...values)
```

Function values are passed using ordinary function-expression syntax:

```raven
func use(action: () -> int) -> int {
    return action()
}

val result = use(() => 42)
```
