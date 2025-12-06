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

## Interesting points

Allow for operator overloading via extensions: incl. operators, conversion, indexers, callable.

```raven
extension FooExtensions for Foo {
    public static operator == (a: Foo, b: Foo) -> bool { ... }

    public static implicit conversion(self: Foo) -> string { ... }
}
```

Operator overload declaration syntax