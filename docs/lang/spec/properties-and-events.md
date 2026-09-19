# Properties and events

Raven uses a property-first model. `val` and `var` define the public mutability
contract, while storage is an implementation detail.

## Property kinds

* `val`: publicly read-only after initialization.
* `var`: publicly mutable after initialization.
* Accessors may be omitted when the declaration contract fully defines the
  intended surface.

`val` may declare `set`/`init` accessors. A `set` accessor on `val` must
be less accessible than the getter; otherwise Raven reports `RAV0910`. A `var`
without any writable shape reports `RAV0911`. `init` remains compatible with
public object-initializer assignment.

## Storage properties

Storage properties are declarations without computed implementation:

```raven
val Name: string
var Count: int = 0
```

For storage properties, Raven can infer the property type from an initializer
when the annotation is omitted:

```raven
class Foo {
    val x = 2 // inferred as int
}
```

Without an initializer, a type annotation is required (`RAV0918`).

The compiler synthesizes backing storage. You can still provide accessors to
refine behavior:

```raven
val Status: OrderStatus { private set; }
var Score: int {
    get => field
    set => field = max(0, value)
}
```

Accessor defaults:

| Contract | Getter | Setter |
| -------- | ------ | ------ |
| `val`    | public | none   |
| `var`    | public | public |

When a storage property declares an accessor list that omits `get`, Raven still
provides the getter required by the `val` or `var` contract. For example,
`val Status: OrderStatus { private set; }` has a public getter and a private
setter. For `var`, writable access is part of the public contract: explicit
`set` and `init` accessors must match the property's accessibility.
Explicit accessor lists are required when the property surface differs from the
default `val`/`var` contract.

## Computed properties

Computed properties provide implementation directly and do not use synthesized
storage unless explicitly needed:

```raven
val FullName: string => first + " " + last
```

## The `init` accessor

`init` permits assignment only during initialization and preserves `val`
semantics:

```raven
val Name: string { init; }
```

Initialization includes inline initializers, constructors (`init(...)`),
initializer blocks, and object initializers.

## `field` and indexers

Inside storage-property accessors, `field` references the synthesized backing
field. Using `field` outside an accessor reports `RAV0912`; using it in a
property without backing storage reports `RAV0913`.

Indexers are a property form using `self[...]` and follow the same `val`/`var`,
`get`/`set`/`init`, and accessibility rules:

```raven
var self[index: int]: string {
    get => items[index]
    set { items[index] = value }
}
```

Indexers require element access (`items[index]`), including imported indexers whose
CLI property name is `Item` or another name. That metadata name is not an ordinary
member-access expression: `items.Item` is invalid. Dot completion omits indexers and
offers the element's members after `items[index].`. Ordinary parameterless properties
named `Item` remain accessible by name. Symbol enumeration still exposes indexer
metadata, with `IsIndexer` true and `CanBeReferencedByName` false.

This follows the distinction between property and indexer access in
[C#'s indexer model](https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/indexers/using-indexers).
The CLI property/accessor representation is unchanged.

## Events

Events let a type notify any registered handlers when something happens,
without knowing which objects are listening.

Events expose a delegate-like member that supports handler subscription via
`+=` and `-=`. An event declaration specifies the `event` keyword, a name, and
the event handler type:

```raven
class Button {
    event Clicked: System.Action;
}
```

### Custom event accessors

Custom events supply `add` and `remove` accessors, which receive the implicit
`value` parameter of the handler type:

```raven
class Button {
    event Clicked: System.Action {
        add { /* register value */ }
        remove { /* unregister value */ }
    }
}
```

### Auto-implemented events

When an event declaration ends with `;`, the compiler synthesizes a hidden
backing field and trivial `add`/`remove` accessors. Auto-events are the only
events that can be invoked directly, and invocation is only permitted inside
the declaring type:

```raven
class Button {
    event Clicked: System.Action? // Events can be null

    func Raise() -> unit {
        Clicked?();
    }
}
```


Interface indexer lookup includes inherited interfaces, with generic arguments
substituted. A more-derived declaration hides an inherited indexer with the same
signature before getter/setter availability is checked: redeclaring a read-only
indexer does not expose the hidden setter. Equally applicable indexers from
unrelated interfaces are ambiguous; select a specific interface explicitly.

### Expression-bodied indexer emission

A getter-only indexer can put its expression directly on the declaration:
`val self[index: int]: int => index + 40`. Class and struct indexers emit an
ordinary read-only CLI Item property and getter, using the same expression-body
lowering and sequence-point handling as properties. No Runtime Contract option
is needed. Regression validation executes both receiver kinds on modern .NET;
this is not a .NET Framework or NanoFramework execution claim.
