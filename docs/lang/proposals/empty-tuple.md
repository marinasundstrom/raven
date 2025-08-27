# Proposal: Empty tuple

> ⚠️ This proposal has **NOT** been implemented

Both the empty tuple and the unit value are represented by `()` in type annotations and expressions. This mirrors languages such as F# and Swift, where `()` denotes a value carrying no information.

## Tuples in .NET

In C#/.NET, tuples are typically created via `System.ValueTuple` and its `Create` methods. Each overload returns a specialized structure such as `ValueTuple<T1, T2>`. An overload `ValueTuple.Create()` yields the empty tuple. In practice, developers rarely create empty or singleton tuples.

## Details

* Mapping the empty tuple to `Unit` avoids introducing a separate runtime type.
* Tuples aren't meant to be exposed via public APIs, so this behavior remains internal to Raven code and has no impact on interoperability.

## Conclusion

An empty tuple is `Unit`.

Project `System.ValueTuple` as `System.Unit` and treat those types as interchangeable. Languages such as F# and Haskell also use `()` to denote a unit value, providing precedent for this design.

When `()` appears in code, the compiler produces a value of type `Unit`:

```raven
let complete : () -> Unit = () => ()
let pair : (Int, Unit) = (42, ())
```

