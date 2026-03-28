# Proposal: Class inheritance

> ⚠️ 🧩 This proposal has been partly implemented

This document outlines minimal support for class-based inheritance to enable testing scenarios that require a type hierarchy.

## Purpose

Allow classes to inherit from other classes while explicitly controlling whether a class may be extended.

## Syntax

### Declaring an inheritable class

Classes are sealed by default. Marking a class `open` allows derivation and keeps instantiable classes from becoming base types
unless their author consciously opts in:

```raven
open class Parent {}
class DerivedA : Parent {}
class DerivedB : Parent {}
```

Abstract classes are implicitly open and can be inherited without adding `open`:

```raven
abstract class Animal {}
class Dog : Animal {}
```

### Sealed hierarchies

To support algebraic-style modeling and exhaustive pattern matching, Raven follows the terminology used by Kotlin and modern
Java. Applying the `sealed` modifier to a class, record class, or interface creates a closed hierarchy whose direct subtypes are fixed at
compile time. A sealed class is implicitly abstract and cannot be instantiated directly. Without an explicit `permits` clause,
the permitted subtypes are all types in the same source file that directly inherit from, extend, or implement the sealed type. An optional `permits`
clause names the exact set of allowed direct subtypes. This differs from the C# meaning of `sealed` (which Raven already
provides by default). A sealed type remains extensible only by its known children, allowing tooling to reason about the entire
closed family. Nested declarations are part of this model, so sealed interfaces can host their case-like implementing types directly in the interface body.

### Constructors

If a derived class omits a constructor, the base class' default constructor is called automatically. A constructor that needs to forward arguments may declare an initializer clause between its parameter list and body:

```raven
open class Base { public init(value: int) {} }

class Derived : Base {
    public init(value: int): base(value) {
        // Body runs after the base constructor finishes.
    }
}
```

The initializer runs before the derived body executes. It is only available on ordinary instance constructors; static constructors report `RAV0312`.

For types with primary constructor parameters, the primary constructor body is written as a bare block and must be the first member:

```raven
class Customer(name: string) {
    {
        // primary constructor body
    }
}
```

`init { ... }` is sugar for `init() { ... }` (a parameterless constructor overload), not a primary-constructor body.

### Access modifiers

Classes and their members support the existing access modifiers (`public`, `internal`, `protected`, `private`). A `protected` member is accessible within the declaring class and its subclasses.

### Static classes

Static classes are non-instantiable containers for static members. Like C# and other .NET languages, Raven does not allow a
static type to appear in a storage-location type position. Fields, properties, indexers, parameters, and local declarations
must use non-static types, while non-storage contexts such as `typeof(System.IO.File)` remain valid.

## Progress

- ✅ `open` classes and base type syntax
- ✅ Default constructor chaining to the base default constructor
- ✅ Access modifiers on classes and members
- ✅ Explicit base constructor invocation
- ✅ Sealed hierarchies (`sealed class`, `sealed record class`, `sealed interface`, `permits` clause)
- ✅ Same-file closure and explicit permits enforcement
- ✅ Match exhaustiveness analysis for sealed hierarchies
- ✅ `[ClosedHierarchy]` attribute emission on sealed hierarchy base types
- ⚠️ Advanced inheritance features (for example multiple inheritance)

## Limitations

* Only single inheritance is supported.
* Base constructors must have no parameters for automatic chaining.
* Future work will explore Kotlin-style extensibility options and default constructors.
