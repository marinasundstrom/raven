# Proposal: Named constructors

> ⚠️ 🧩 This proposal has been partly implemented

This document outlines the feature "named constructors".

## Purpose

To provide a way to give semantics to object creation and to allow for behaviors not available on ordinary constructors.

## Syntax

The syntax for declaring a named constructor is similar to an ordinary constructor, but in addition to `init` there is an identifier holding the name of the construct:

```raven
public class Person {
    private let name

    public init WithName(name: string) {
        self.name = name
    }
}
```

Notice that `self` is available within the named constructor.

The named constructor is utilized like so:

```raven
let person = Person.WithName("John")
```

You could then additionally call methods on the resulting object in a way that makes code flow.

```raven
let person = Person
    .WithName("John")
    .WithAge(42)
```

### Implementation details

Implementation-wise, named constructors are static factory methods that are defined like this:

```csharp
public class Person 
{
    private string name;

    public string WithName(string name) 
    {
        var self = new Person();
        self.name = name;
        return self;
    }
}
```

And utilized like so from C# and other .NET languages:

```csharp
var person = Person.WithName("John");
```

## Points

* Gives semantics to object creation.
* Has access to `self`.
* Behave like methods.

## Notes

* Named constructors are like methods, because they are, so they can be `async`. But semantically, you might go for static methods when `self` is not appropriate.