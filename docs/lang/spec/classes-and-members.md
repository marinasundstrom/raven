> ⚠️ This is a living document that is subject to change.

# Classes, structs, and interfaces

This chapter describes Raven's object-oriented constructs: declaring classes and structs, defining members, and implementing interfaces.

## Members (classes/structs)

Raven supports classes and structs with fields, methods, constructors,
properties, and indexers. Modifiers are C#-like but validated by the
binder (e.g., `abstract` members require an `abstract` type; `override`
requires a virtual base member).

```raven
class Counter
{
    public let Name: string

    private var _value: int = 0

    init(name: string) { Name = name }

    public Value: int {
        get => _value
        private set => _value = value
    }

    // Indexer
    public this[i: int]: int {
        get => _value + i
    }

    public Increment() -> () => _value = _value + 1
}
```

**Notes**

* Fields use binding keywords (`let`, `var`, or `const`) and require `;` after declarators.
* Accessor-level access (e.g., `private set`) is supported.
* Methods/ctors/properties/indexers may use arrow bodies.
* Members can be marked `static` to associate them with the type rather than an instance.

`const` fields behave like their local counterparts: they must specify a
compile-time constant initializer, and the compiler records the folded value in
metadata. Raven treats these declarations as implicitly `static` even if the
modifier is omitted, and any attempt to reassign the field produces a diagnostic
just like rebinding an immutable local.

### Generic types

Classes and structs optionally declare type parameters immediately after the
type name. The parameters become part of the type's identity and are available
throughout the member list.

```raven
class Box<T>
{
    public Value: T { get; }

    init(value: T) { Value = value }
}

let ints = Box<int>(1)
let words = Box<string>("ok")
```

Instantiating a generic type supplies concrete type arguments between `<` and
`>` in the same order the parameters were declared. The compiler emits standard
CLR constructed types, so Raven generics interoperate seamlessly with existing
.NET APIs. When a type argument itself is generic, nest the constructions as
needed (`Dictionary<string, List<int>>`).

Type parameters support constraints using the `:` syntax. After the colon,
specify `class`, `struct`, and/or nominal types that the argument must derive
from or implement. Constraints are comma-separated and may appear in any order.

```raven
class Repository<TContext: class, IDisposable>
{
    init(context: TContext) { /* ... */ }
}
```

The compiler enforces the constraint set whenever the generic type is
constructed. Passing an argument that does not satisfy one of the constraints
reports an error and identifies the unmet requirement.

#### Accessibility

Types and members accept the standard access modifiers. Applying more than
one keyword produces the expected CLR combinations:

| Modifier syntax            | Meaning |
|----------------------------|---------|
| `public`                   | Visible from any assembly. |
| `internal`                 | Visible only within the current assembly. |
| `protected`                | Visible to the declaring type and to derived types. |
| `private`                  | Visible only inside the declaring type. |
| `protected internal`       | Visible to derived types or any code in the same assembly. |
| `private protected`        | Visible to derived types declared in the same assembly. |

Default accessibility depends on the declaration context:

* Top-level classes, structs, interfaces, and enums default to `internal` and
  must be marked `public` to be exposed from the assembly. Other accessibility
  keywords collapse to the same effective visibility when applied at the
  top level.
* Nested types default to `private` unless they are declared inside an
  interface, in which case they are implicitly `public`.
* Member declarations (fields, methods, properties, indexers, and constructors)
  default to `public`. Members of interfaces are always public, even when no
  modifier is written.

Constructors follow these rules as well. An explicitly declared parameterless
constructor may specify any of the modifiers above to control how instances
are created:

```raven
class Widget
{
    private init() { /* singleton helper */ }
    protected init(name: string) { /* subclass hook */ }
    protected internal init Clone(source: Widget) { /* reuse state */ }
}
```

### Primary constructors

Classes may declare a primary constructor by adding an argument list to the
type header. Each parameter is captured and stored in an implicit instance
field with the same name. The compiler synthesizes an instance constructor
whose signature matches the header parameters and assigns the arguments to
those fields before any other field initializers execute. Supplying a primary
constructor suppresses the implicit parameterless constructor; callers must
provide the declared arguments.

```raven
class Person(name: string, age: int)
{
    public GetName() -> string => name
    public GetAge() -> int => age
}

let person = Person("Ada", 42)
let years = person.GetAge()
```

### Properties

Property declarations expose a value through accessor methods rather than by
directly exposing a field. A property appears inside a class or struct and must
declare a name, type, and one or more accessors:

```raven
public Value: int {
    get {
        return _value
    }

    set {
        _value = value
    }
}
```

The compiler synthesizes accessor methods named `get_<PropertyName>` and
`set_<PropertyName>`. Setters receive an implicit parameter named `value` whose
type matches the property type. The `static` modifier may be applied to the
property declaration to associate both accessors with the containing type.

#### Accessor bodies

Accessors may use block bodies or expression bodies. A property declaration may
also collapse to a single expression-bodied member; it is shorthand for a `get`
accessor with the same expression body.

```raven
public Value: int => _value

public Value: int {
    get => _value
    private set => _value = value
}
```

#### Auto-implemented properties

When every accessor in a class or struct property omits both a block body and an
expression body, the property is treated as **auto-implemented**. The compiler
generates a hidden backing field of the same type and emits trivial accessor
bodies that read from and write to that field. Auto-properties respect the
property's modifiers: a `static` auto-property produces a static backing field,
and accessor-level accessibility (for example `private set`) controls exposure
without affecting code generation.

Any accessor may be omitted. A property with only `get` remains read-only and
exposes the default value of its backing field until assigned from within the
type (e.g., via a constructor calling another accessor). Auto-properties are not
available on interfaces, where accessors remain abstract.

### Class inheritance

Classes are sealed by default. Marking a class `open` allows it to be used as a base type. This explicit opt-in keeps instantiable
classes from being inherited accidentally—the class author must deliberately allow derivation before clients can extend it. The
`abstract` modifier also enables inheritance: abstract classes are implicitly open and may serve as base types without the `open`
keyword. Raven also supports **sealed hierarchies** in the style of Kotlin and modern Java. Applying the `sealed` modifier to a class keeps the hierarchy
closed to a known set of subclasses declared in the same source file (including nested types). The compiler treats those
subclasses as the exhaustive set for purposes such as pattern-matching analysis.

> **Note:** Raven's `sealed` keyword follows Kotlin and Java's terminology rather than C#'s. In C#, `sealed` means "cannot be
> inherited". In Raven that concept is already the default. The `sealed` modifier instead designates a hierarchy whose direct
> subclasses are known at compile time.

A base class is specified with a colon followed by the type name:

```raven
open class Parent {}
class Child : Parent {}
```

If the base list contains additional types after the base class, each of those entries must be an interface that the class implements. When no base class is provided, the compiler implicitly uses `object` and treats the first entry as an interface instead:

```raven
class Worker : IDisposable, ILogger
{
    Dispose() -> () { /* ... */ }
    Log(message: string) -> () { /* ... */ }
}
```

Implementations are matched by name, parameter count, and `ref`/`out` modifiers. Each successfully matched member is emitted as a
final override so the CLR records the implementation in the type's interface map. See [Interfaces](#interfaces) for interface declaration rules and inheritance.

If a derived class omits a constructor, the base class' parameterless constructor is invoked automatically. Access modifiers
(`public`, `internal`, `protected`, `private`) apply as usual; `protected` members are accessible to derived classes. An
instance constructor may chain to a specific base overload by adding a constructor initializer between its parameter list and
body:

```raven
open class Base { public init(value: int) {} }

class Derived : Base {
    public init(value: int): base(value) {
        // The base invocation runs before the derived body executes.
    }
}
```

The initializer is only available on ordinary instance constructors. Static constructors report `RAV0312`, and named
constructors continue to behave as user-defined factories without chaining.

> **Limitations:** Only single inheritance is supported.

### Partial classes

Applying the `partial` modifier to a class declaration allows the type to be defined across multiple declarations in the same
assembly. All declarations must use the `partial` modifier; omitting it on any declaration produces a diagnostic and prevents the
declarations from merging. When two declarations with the same name and containing scope appear without `partial`, the compiler
reports a duplicate-type diagnostic.

Partial declarations combine their members and share a single type identity. Modifiers, accessibility, and the base list are
interpreted as a whole—Raven currently resolves the base class, implemented interfaces, and type parameters from the first
declaration it processes, so later declarations should match or omit those clauses. Apart from these shared attributes, each
partial declaration may contribute additional members, and the aggregate behaves exactly like a class declared in one piece.

### Parameter semantics

Method, constructor, and accessor parameters are immutable by default. They
behave like `let` bindings: the compiler rejects assignments that attempt to
rebind the parameter name. Add the `var` modifier when a parameter must be
reassigned inside the body—for example, to reuse a scratch variable or to
satisfy an `out` contract.

```raven
func clamp(min: int, value: int, max: int) -> int
{
    // value = ...    // error: parameters are immutable by default
    return Math.Max(min, Math.Min(value, max))
}

func TryParse(text: string, out var result: &int) -> bool
{
    result = 0      // ok: the parameter explicitly opts into mutation
    /* ... */
}
```

Declaring a parameter with `&Type` passes the argument by reference. The callee
receives an alias to the caller's storage and can read or write through that
alias. Callers supply such arguments with the address-of operator `&expr`.
Placing `out` before the parameter name signals that the method must assign the
alias before returning; the caller is required to pass an assignable storage
location. Ordinary by-reference parameters omit `out` and behave like `ref`
arguments in other languages.

```raven
func Increment(var value: &int) -> ()
{
    value = value + 1
}

var total = 41
Increment(&total)
Console.WriteLine(total) // prints 42
```

### Method overloading

Functions and methods may share a name as long as their parameter counts or
types differ. Overload resolution selects the best match based on argument
types, `out`/by-ref modifiers, and nullability. Ambiguous calls produce a
diagnostic.

```raven
class Printer
{
    public Print(x: int) -> () => Console.WriteLine(x)
    public Print(x: string) -> () => Console.WriteLine(x)
}

Print(42)
Print("hi")
```

### Default parameter values

Methods, constructors, and other function-like members may specify default
values for trailing parameters using `= expression`. Optional parameters follow
the same rules as top-level functions: once a parameter provides a default, all
subsequent parameters in the list must also supply defaults. The expression is
restricted to compile-time constants—literals (including `null`), parenthesized
literals, or unary `+`/`-` applied to numeric literals—and the resulting value
must convert to the parameter type using an implicit conversion. When the
expression fails these checks, the compiler reports an error and treats the
parameter as required.

### Invocation operator

Declaring a method named `self` makes instances of the type invocable with the
call operator `()`.

```raven
class Adder
{
    public self(x: int, y: int) -> int => x + y
}

let add = Adder()
let sum = add(1, 2) // calls self(1, 2)
```

Invocation operators can themselves be overloaded by providing multiple `self`
methods with different parameter signatures.

## Interfaces

`interface` declarations describe a contract that other types may implement. Interfaces are reference types; they emit as abstract CLR interfaces and cannot be instantiated directly.

```raven
interface ILogger
{
    Log(message: string) -> ()
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
class FileLogger : ILogger, IDisposable
{
    public Dispose() -> () { /* release resources */ }

    public Log(message: string) -> ()
    {
        Console.WriteLine(message)
    }
}
```

An **explicit interface implementation** qualifies the member name with the interface type: `ILogger.Log`. Explicit members are always instance members, ignore `virtual`/`override` modifiers, and are not accessible through the implementing type by name; callers must reference the containing interface. The compiler emits these methods with metadata names like `Namespace.ILogger.Log` and wires them directly into the interface map.

```raven
class QuietLogger : ILogger
{
    string ILogger.Log(message: string) -> string
    {
        return "[quiet]"
    }
}
```

```
let logger = QuietLogger()
logger.Log("hi")              // error: member not found
(logger :> ILogger).Log("hi") // ok
```

If a type lists only interfaces, the compiler still emits `System.Object` as the base type before attaching the interface implementations.
