# Interfaces

`interface` declarations describe a contract that other types may implement. Interfaces are reference types; they emit as abstract CLR interfaces and cannot be instantiated directly.

```raven
interface ILogger {
    func Log(message: string) -> ()
}
```

Interfaces may be declared at the top level, inside namespaces, or nested inside other types. Like classes, they support the same set of member declarations (methods, properties, indexers, and nested types). Instance members are abstract requirements by default, but supplying a body for a method or accessor turns it into a default implementation emitted directly on the interface.

Static members, by contrast, must provide a body and emit as real static members on the interface type; implementing types never participate in their implementation or override process. When an interface member uses accessors, a bare `;` accessor denotes an unimplemented accessor requirement (`get;`/`set;`).

### Member accessibility

Interface members default to `public`, including members with default bodies.
Explicit `public`, `private`, `protected`, `internal`, `protected internal`, and
`private protected` access modifiers retain their ordinary meaning, following C#.
Private function members must have implementations and cannot be abstract or virtual.
Instance default implementations are implicitly virtual unless private or sealed.
These rules apply to methods and property, indexer, and event accessors.

Implicit interface implementation requires public members on both the interface
and the implementing type, including required property accessors. Non-public
requirements use explicit interface implementations. Private default helpers are
not implementation requirements and do not become class virtual slots.

### Base interfaces

An interface may inherit from other interfaces by listing them after a colon.
Every base must be an interface. The base list is resolved in the declared interface's
scope, including its own generic type parameters, also for nested declarations.
For example, `interface Child<T> : Parent<T> {}` passes Child's `T` to Parent.

```raven
interface IAsyncLogger : ILogger, IDisposable {}
```

Default interface bodies can refer to inherited accessible properties and methods
by unqualified name, including members inherited through constructed generic
interfaces. A shared ancestor in a diamond is one inherited contract. This does
not make default interface members available as unqualified class members.

### Implementing interfaces

Classes, structs, and union carriers implement interfaces by listing them in
their base list. The optional class base (if any) must appear first, followed by
one or more interfaces. Structs and unions accept interfaces only. Implementing
types must provide members whose signatures match every required interface
member—name, parameter count, parameter types (including by-reference
modifiers), and return type must align.

```raven
class FileLogger : ILogger, IDisposable {
    func Dispose() -> () { /* release resources */ }

    func Log(message: string) -> () {
        Console.WriteLine(message)
    }
}
```

For a union, the interface is implemented by the carrier rather than by each
generated case type:

```raven
union LogFailure: ILogger {
    case Unavailable

    func Log(message: string) -> () {
        Console.WriteLine(message)
    }
}
```

An **explicit interface implementation** qualifies the member name with the
interface type: `ILogger.Log`. Explicit members are always instance members,
ignore `virtual` and `override`, and are accessible only through the interface.

For an indexer, qualify `self`: `val IValues.self[index: int]: int => index`.
Writable implementations use `var IValues.self[index: int]: int` with `get`
and `set` accessor bodies. These explicit indexers are accessed through
the interface, following the same rules as other explicit members.

The qualifier must name an interface (`RAV0313`), the containing type must list
that interface (`RAV0314`), and the interface must contain a member with the
same name and signature (`RAV0315`).

```raven
class QuietLogger : ILogger {
    func ILogger.Log(message: string) {
        Console.WriteLine("[quiet] " + message)
    }
}
```

```raven
let logger = QuietLogger()
logger.Log("hi")              // error: member not found
(logger :> ILogger).Log("hi") // ok
```

### Reference-only variance conversions

Generic interface `out` and `in` annotations admit identity or reference conversions
between the corresponding type arguments. Boxing is not a variance conversion:
`Producer<int>` cannot become `Producer<object>`, and `Consumer<object>` cannot
become `Consumer<int>`. Identical value arguments remain valid. An unconstrained
type parameter does not establish the required reference conversion.

Nullable reference arguments obey the same variance direction while preserving
nullability. `Producer<string?>` can widen to `Producer<object?>`, and
`Consumer<object?>` can narrow to `Consumer<string?>`. Variance cannot remove a
possible null: `Producer<string?>` cannot implicitly become `Producer<object>`,
and `Consumer<object>` cannot become `Consumer<string?>`. Nullable value types do
not become reference arguments through this rule. Runtime generic arguments and
emitted interface variance annotations are unchanged.

## Target-specific contracts

**neoCLR only:** native `Self` contracts express the concrete conforming type
without adding an interface type parameter. See the grouped
[neoCLR-only features](neoclr-features.md#native-self-contracts) for semantics,
examples and current native metadata limitations. Ordinary interface declarations
remain part of the shared language reference.

## Interface tests and explicit reference conversions

An `open` or abstract class can be explicitly converted to an interface even when
the class does not declare that interface: a derived runtime object may implement
it. The reverse explicit reference conversion is also permitted. A closed class
must actually implement the interface. Unrelated interfaces can be explicitly
converted to each other because an object may implement both.

Type patterns use the same compatibility rule. A compatible pattern tests the
runtime object and can fail to match; compatibility does not establish that the
object implements the interface. These are shared language semantics for .NET and
neoCLR.
