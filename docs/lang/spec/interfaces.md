# Interfaces

`interface` declarations describe a contract that other types may implement. Interfaces are reference types; they emit as abstract CLR interfaces and cannot be instantiated directly.

```raven
interface ILogger {
    func Log(message: string) -> ()
}
```

Interfaces may be declared at the top level, inside namespaces, or nested inside other types. Like classes, they support the same set of member declarations (methods, properties, indexers, and nested types). Instance members are abstract requirements by default, but supplying a body for a method or accessor turns it into a default implementation emitted directly on the interface.

Static members, by contrast, must provide a body and emit as real static members on the interface type; implementing types never participate in their implementation or override process. When an interface member uses accessors, a bare `;` accessor denotes an unimplemented accessor requirement (`get;`/`set;`).

### Base interfaces

An interface may inherit from other interfaces by listing them after a colon.
Every base must be an interface.

```raven
interface IAsyncLogger : ILogger, IDisposable {}
```

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
