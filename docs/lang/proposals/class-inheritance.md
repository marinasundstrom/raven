# Proposal: Class inheritance

> ⚠️ This proposal has **NOT** been implemented

This document outlines minimal support for class-based inheritance to enable testing scenarios that require a type hierarchy.

## Purpose

Allow classes to inherit from other classes while explicitly controlling whether a class may be extended.

## Syntax

### Declaring an inheritable class

Classes are sealed by default. A class must be marked `open` to allow derivation:

```raven
open class Parent {}
class DerivedA : Parent {}
class DerivedB : Parent {}
```

### Constructors

If a derived class omits a constructor, the base class' default constructor is called automatically. Explicit constructors must chain to a base constructor; default constructors are planned but not yet available.

### Access modifiers

Classes and their members support the existing access modifiers (`public`, `internal`, `protected`, `private`). A `protected` member is accessible within the declaring class and its subclasses.

## Limitations

* Only single inheritance is supported.
* Base constructors must have no parameters for automatic chaining.
* Interfaces, abstract classes, and other inheritance features are out of scope for this MVP.
* Future work will explore Kotlin-style extensibility options and default constructors.

