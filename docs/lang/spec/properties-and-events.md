# Properties and events

Raven uses a property-first model. `val` and `var` define the public mutability
contract, while storage is an implementation detail.

### Property kinds

* `val`: publicly read-only after initialization.
* `var`: publicly mutable after initialization.
* Accessors may be omitted when the declaration contract fully defines the
  intended surface.

`val` may declare `set`/`init` accessors. A `set` accessor on `val` must
be less accessible than the getter. `init` remains compatible with public
object-initializer assignment.

### Storage (auto) properties

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

Without an initializer, a type annotation is required.

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

When a storage property declares an accessor list that omits `get`, the compiler still synthesizes the getter to preserve the `val`/`var` contract. For example, `val Status: OrderStatus { private set; }` emits a public getter and a private setter.
For `var`, writable access is part of the public contract: explicit `set`/`init` accessors must match the property's accessibility.
Explicit accessor lists are required when the property surface differs from the
default `val`/`var` contract.

### Computed properties

Computed properties provide implementation directly and do not use synthesized
storage unless explicitly needed:

```raven
val FullName: string => first + " " + last
```

### `init` accessor and initialization phase

`init` permits assignment only during initialization and preserves `val`
semantics:

```raven
val Name: string { init; }
```

Initialization includes inline initializers, constructors (`init(...)`),
initializer blocks, and object initializers.

### `field` and indexers

Inside storage-property accessors, `field` references the synthesized backing
field.

Indexers are a property form using `self[...]` and follow the same `val`/`var`,
`get`/`set`/`init`, and accessibility rules:

```raven
var self[index: int]: string {
    get => items[index]
    set { items[index] = value }
}
```

### Private storage property lowering

Private storage properties may be lowered to field-only emission when observable
semantics are preserved.

```raven
private val count: int
private var score: int
```

This lowering affects emitted representation only. In semantic analysis and the
symbol model, the member remains a property and continues to participate in
property diagnostics/tooling.

Lowering is valid for private storage properties that do not require accessor
logic. Computed properties are not lowered this way.

When applied, reads/writes may lower to direct field access and accessor methods
may be omitted from emitted metadata. Source-level behavior remains
property-centric.

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

### Event accessors

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
