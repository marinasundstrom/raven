# Tuple expressions

Tuples group a small, fixed set of values without declaring a new type. They
are useful for local results and short-lived data shapes.

Tuples can be **named** or **positional**. Both projections are available.

```raven
val tuple = (a: 42, b: 2)
Console.WriteLine(tuple.a)      // named
Console.WriteLine(tuple.Item1)  // positional
```

Tuple types and their element-name rules are documented in [Types and
unions](types-and-unions.md#tuple-types).
