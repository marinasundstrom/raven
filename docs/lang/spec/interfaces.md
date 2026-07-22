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

An interface may inherit from other interfaces by listing them after a colon. The compiler resolves each entry to an interface type and records the relationship so `AllInterfaces` exposes the full transitive closure. Non-interface types are ignored.

```raven
interface IAsyncLogger : ILogger, IDisposable {}
```

### Implementing interfaces

Classes and structs implement interfaces by listing them in their base list. The optional class base (if any) must appear first, followed by one or more interfaces. Implementing types must provide members whose signatures match every required interface member—name, parameter count, parameter types (including by-reference modifiers), and return type must align. Raven records the matching methods as final overrides and emits the necessary `InterfaceImpl` metadata so the CLR recognises the implementation.

```raven
class FileLogger : ILogger, IDisposable {
    func Dispose() -> () { /* release resources */ }

    func Log(message: string) -> () {
        Console.WriteLine(message)
    }
}
```

An **explicit interface implementation** qualifies the member name with the interface type: `ILogger.Log`. Explicit members are always instance members, ignore `virtual`/`override` modifiers, and are not accessible through the implementing type by name; callers must reference the containing interface. The compiler emits these methods with metadata names like `Namespace.ILogger.Log` and wires them directly into the interface map.

```raven
class QuietLogger : ILogger {
    func ILogger.Log(message: string) -> string {
        return "[quiet]"
    }
}
```

```
val logger = QuietLogger()
logger.Log("hi")              // error: member not found
(logger :> ILogger).Log("hi") // ok
```

If a type lists only interfaces, the compiler still emits `System.Object` as the base type before attaching the interface implementations.
