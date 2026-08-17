# Classes, structs, and interfaces

Classes and structs group data with the operations that work on it. Use a class
when identity, shared mutable state, inheritance, or a resource lifecycle is
part of the model. Use a struct for a compact value with copy semantics.

```raven
class Counter(private var count: int = 0) {
    val Count: int => count

    func Increment() {
        count = count + 1
    }
}

struct Point(val X: double, val Y: double)
```

An interface describes behavior that several otherwise unrelated types can
provide:

```raven
interface IPrintable {
    func Print()
}

class Report : IPrintable {
    func Print() {
        Console.WriteLine("Report")
    }
}
```

Classes, structs, and interfaces can contain methods, properties, indexers,
events, nested types, and other members appropriate to their kind. They support
generic parameters and constraints, access control, and partial declarations.

Type declarations at namespace scope default to `internal`; use `public` to
export them from an assembly. Members declared inside a type default to
`public`, although their effective visibility can never exceed that of the
containing type.

## Learn about types and members

* [Type declarations and initialization](type-declarations-and-initialization.md)
  covers primary constructors, fields, generic types, initializers, records,
  static classes, and ref structs.
* [Properties and events](properties-and-events.md) covers stored and computed
  properties, accessors, indexers, backing fields, and events.
* [Inheritance and partial types](inheritance-and-partial-types.md) covers base
  types, open and sealed hierarchies, overrides, and declarations spread across
  several source files.
* [Parameters, overloading, and operators](parameters-overloading-and-operators.md)
  covers parameter behavior, method selection, custom operators, and callable
  objects.
* [Interfaces](interfaces.md) covers interface inheritance and implementation.
