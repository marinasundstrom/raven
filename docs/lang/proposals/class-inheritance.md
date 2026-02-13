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
Java. Applying the `sealed` modifier to a class keeps the hierarchy closed to subclasses defined alongside the base declaration
(either in the same file or as nested types). This differs from the C# meaning of `sealed` (which Raven already provides by
default). A sealed class remains inheritable by its known children, allowing tooling to reason about the entire closed family.

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

The initializer runs before the derived body executes. It is only available on ordinary instance constructors; static constructors report `RAV0312`, and named constructors continue to build user-defined factory patterns without chaining.

### Access modifiers

Classes and their members support the existing access modifiers (`public`, `internal`, `protected`, `private`). A `protected` member is accessible within the declaring class and its subclasses.

## Progress

- ✅ `open` classes and base type syntax
- ✅ Default constructor chaining to the base default constructor
- ✅ Access modifiers on classes and members
- ✅ Explicit base constructor invocation
- ⚠️ Advanced inheritance features (for example multiple inheritance)

## Limitations

* Only single inheritance is supported.
* Base constructors must have no parameters for automatic chaining.
* Future work will explore Kotlin-style extensibility options and default constructors.
