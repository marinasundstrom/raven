# Proposal: Static extension members

## Summary

```raven
extension FooExtensions for Foo {
    public static Test(value: int) -> int { ... }
}
```

```raven
Foo.Test(42)

class Foo {}
```

## Supported members

Static methods and static properties.

## Lookup

Static extension members participate in static member lookup. When binding
`Type.Member`, the compiler first resolves real static members on `Type`. If no
match is found, it searches in-scope extension containers whose receiver type is
compatible with `Type` (including implicit conversions and nullability).
Wildcard-importing the target type (`import Type.*`) also brings static
extension members into unqualified lookup.

## Interesting points

Allow for operator overloading via extensions: incl. operators, conversion, indexers, callable.

```raven
extension FooExtensions for Foo {
    public static operator == (a: Foo, b: Foo) -> bool { ... }

    public static implicit conversion(self: Foo) -> string { ... }
}
```

Operator overload declaration syntax
