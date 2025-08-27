# Proposal: Empty tuple

> ⚠️ This proposal has **NOT** been implemented

Both empty tuple and and unit ar represented by `()` both as type specifier and expression.

## Tuples in .NET

The preferred way of creating tuples in C#/.NET is using `ValueTuple` using one of the `Create` methods. Each returning an instance of specialized `ValueType<T>`, `ValueType<T1, T2>` etc. 

There is an overload for empty tuple (`ValueTuple`): `ValueTuple.Create()`

You don't create empty tuples, or singleton tuples, in normal cases.

## Details

* Tuples aren't meant to be exposed via public APIs.
* The specific behavior is isolated to code written in Raven so it won't leak.

## Conclusion

An empty tuple is `Unit`.

Project `System.ValueTuple` as `System.Unit`. Treat those types as interchangeable.

When you defined `()` you will get `Unit`.