# Object creation and copying

Create an object by calling its type with the values required by its
constructor. Raven deliberately uses ordinary call syntax instead of `new`:

```raven
let builder = StringBuilder()
let person = Person("Ada", 36)
let values = List<int>()
```

A standalone type name is not a value or a constructor call. Write `Person()`
when invoking a parameterless constructor.

Constructor arguments follow the same positional, named, optional, and
collector rules as other calls.

## Generic constructor inference

When a generic type is called without explicit type arguments, Raven can infer
them from the constructor arguments. Function expressions also receive target
types from the candidate constructor:

```raven
open class Endpoint {
    init(handler: Delegate) {}
}

class GET<T> : Endpoint {
    init(pattern: string, handler: T -> string) : base(handler) {}
}

let route = GET("/{id:int}", func (id: int) => id.ToString())
// route: GET<int>
```

Omitting the type-argument list intentionally leaves the terminal type name
open while constructor candidates are considered. This is a Raven-specific
source rule; the selected result is still an ordinary constructed CLR generic
type. By contrast, an authored type-argument list always requests its exact
arity, including on qualified and nested type names. `Container.Item<int>`
therefore cannot resolve to a non-generic `Item` or to `Item<T1, T2>`.

If generic and non-generic types have the same name, an applicable non-generic
constructor is preferred. Otherwise Raven selects the one generic type whose
arguments can be inferred. If several generic candidates succeed, the call is
ambiguous; write explicit type arguments or use a different qualified type
name.

## Object initializers

An object initializer sets named members or adds content as part of creating an
object:

```raven
let window = Window {
    Title = "Main"
    Width = 800
    Height = 600
}
```

A bare type followed by braces invokes its parameterless constructor. Braces
can also follow an explicit constructor call:

```raven
let person = Person("Ada") {
    Age = 36
}
```

Raven constructs the object, then evaluates initializer entries from left to
right in source order.

### Member entries

A member entry assigns a writable property or field. `init` accessors are
available because the object is still in its initialization phase:

```raven
class Settings {
    val Theme: string { init; }
    val FontSize: int { init; }
}

let settings = Settings {
    Theme = "Dark"
    FontSize = 14
}
```

`=` assigns a field or property. Compound assignments such as `+=` use ordinary
member-assignment behavior on the new instance. Events can be subscribed or
unsubscribed with `+=` and `-=`:

```raven
let button = Button {
    Clicked += () => Console.WriteLine("clicked")
}
```

The braces are initializer syntax, not constructor arguments. Pass a function
inside the ordinary argument list when the constructor expects one.

### Content entries

A standalone expression inside an initializer is a content entry. If the type
has a mutable `Content` property, the first content entry initializes it.
Otherwise Raven looks for a compatible instance `Add(T)` method and adds each
entry in order.

## Required members

A `required` field or property must be definitely assigned before construction
finishes:

```raven
class Person {
    required val Name: string { init; }
    required val Age: int { init; }
}

let complete = Person { Name = "Ada", Age = 36 }
let incomplete = Person { Name = "Ada" } // error: Age is required
```

A required field must be mutable. A required property must have an accessible
`init` or `set` accessor. `required` is not permitted on `const`, `static`, or
read-only members.

Required members declared by a base type remain required when constructing a
derived type.

### Constructors that satisfy requirements

A constructor marked with
`System.Diagnostics.CodeAnalysis.SetsRequiredMembersAttribute` promises to
initialize every required member. Calling it does not require an object
initializer.

A synthesized record primary constructor makes the same promise:

```raven
record Person(Name: string, Age: int)

let person = Person("Ada", 36)
```

When Raven checks a construction, it collects required members from the type and
its base types. A constructor that sets required members completes the check.
Otherwise, the object initializer must assign every required member. Missing
assignments are compile-time errors.

Primary-constructor promotion and record value shape are described under [Type
declarations and initialization](type-declarations-and-initialization.md).

## With expressions

A `with` expression creates a changed copy without mutating the original:

```raven
record Point(X: int, Y: int)

let origin = Point(0, 0)
let moved = origin with {
    X = 10
}
```

The receiver is evaluated once. Assignment values are evaluated from left to
right. Listing the same member more than once produces `RAV0241`.

Assignments must target writable instance fields or properties. An `init`
accessor is allowed because a `with` body is an initialization context. Required
members may likewise be assigned there.

For records, Raven clones the record and applies the assignments. Synthesized
record copying follows the record's value shape, which contains only public
promoted properties. Non-public promoted properties are not included in
synthesized copy semantics.

### Copy conventions for other types

Non-record types can opt into `with` through several conventions. Raven uses the
first applicable strategy:

1. Call an instance `Update(...)` method whose parameter names correspond to
   readable members. Written assignments provide replacement arguments; other
   parameters receive the current member values.
2. Call an instance `With(...)` method using the same parameter-name convention.
3. For each `X = value` entry, call a one-parameter `WithX(value)` method in
   source order.
4. Create a copy through a parameterless `Clone()` method or a copy constructor,
   then apply the member assignments as an initializer.

Record cloning precedes all of these conventions. If no strategy applies,
Raven reports `RAV0240` because the type does not support `with`.
