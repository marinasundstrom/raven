# Classes, structs, and interfaces

This chapter defines Raven's object-oriented type declarations and member forms.

## Members (classes/structs)

Raven supports classes and structs with properties, methods, constructors,
indexers, fields, and const members. The model is property-first: `val`/`var`
declarations in type bodies define properties by default. Explicit `field` and
`const` declarations provide explicit storage forms when needed.

Modifiers are C#-like but validated by the binder (e.g., `abstract` members
require an `abstract` type; `override` requires a virtual base member).

```raven
class Counter(name: string) {
    val Name: string => name
    private field _value: int = 0

    var Value: int {
        get => _value
        private set => _value = value
    }

    // Indexer
    this[i: int]: int {
        get => _value + i
    }

    func Increment() -> () => _value = _value + 1
}
```

**Rules**

* `val`/`var` type members declare properties.
* `field` declarations are explicit storage members.
* `const` declarations are compile-time constants and map to CLR literal fields.
* Accessor-level access (e.g., `private set`) is supported.
* Methods/ctors/properties/indexers may use arrow bodies.
* Members can be marked `static` to associate them with the type rather than an instance.
* Members that intentionally hide inherited members should use the `new` modifier; otherwise the compiler emits a warning.
* A member name cannot match its immediate containing type name.

### Delegate declarations

Delegate declarations introduce callable types by naming the `Invoke` signature. They can appear at the top level or nested inside other types.

```raven
delegate Transformer(value: int) -> string

class Pipeline {
    delegate Stage<T>(ref value: T) -> bool
}
```

The compiler synthesizes the standard delegate members: a constructor `.ctor(object, IntPtr)` and an `Invoke` method that matches the declared parameter list and return type. If the return type clause is omitted, the delegate returns `unit`.

Delegate parameters support the same `ref`/`out`/`in` modifiers as methods. Delegate declarations can be generic and accept type parameter constraints using the same `where` clause syntax as other type declarations.

### Static classes

Classes marked `static` are utility containers. They are implicitly `abstract`
and `sealed`, and all members must be `static`. Instance fields, methods,
constructors (including primary constructors), properties, events, or indexers
are not permitted inside a static class.

Use `final` alongside `override` to seal an override and prevent further
overrides in derived types (`final override` in Raven, equivalent to C#'s
`sealed override`). The compiler reports an error if `final` is applied without
`override`.

### Field declarations (low-level storage)

Fields are explicit CLR storage members. Use them when source code needs direct
control over storage/layout (for example interop with `StructLayout` and
`FieldOffset`, fixed buffers, or ABI-sensitive layouts).

```raven
class Counter {
    private field _count: int
    private readonly field _id: int = 42
}
```

`const` declarations are separate member declarations:

```raven
class MathConstants {
    const Pi: double = 3.141592653589793
}
```

They remain compile-time constants and are emitted as metadata constants
(implicitly static), similar to other .NET languages.

### Generic types

Classes and structs optionally declare type parameters immediately after the
type name. The parameters become part of the type's identity and are available
throughout the member list.

```raven
class Box<T> {
    val Value: T { get; }

    init(value: T) { Value = value }
}

val ints = Box<int>(1)
val words = Box<string>("ok")
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
class Repository<TContext: class, IDisposable> {
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
| `fileprivate`             | Visible only from the current source file. |
| `protected internal`       | Visible to derived types or any code in the same assembly. |
| `private protected`        | Visible to derived types declared in the same assembly. |

Default accessibility depends on the declaration context:

* Top-level classes, structs, interfaces, and enums default to `internal` and
  must be marked `public` to be exposed from the assembly. Other accessibility
  keywords collapse to the same effective visibility when applied at the
  top level.
* Nested types default to `private` unless they are declared inside an
  interface, in which case they are implicitly `public`.
* Member declarations (fields, methods, properties, indexers, constructors, and lifecycle blocks)
  default to `public` for classes/structs and interfaces.
  Narrower visibility requires an explicit modifier such as `private`,
  `internal`, or `protected`.

Constructors and lifecycle declarations follow these rules as well.

`fileprivate` is a source-level restriction for type-like declarations. The compiler enforces same-file visibility during binding, and mangles the emitted metadata name for the generated type so file-local helpers do not publish a stable CLR-facing type name.

### Initialization model

Raven supports type-header parameters plus `init` blocks as its object
initialization model:

* a type parameter list on the class/struct header (primary-constructor parameters), plus
* one or more `init { ... }` blocks for initialization logic.

`static init { ... }` provides type initialization, and `finally { ... }`
provides finalization logic.

```raven
class Widget(name: string) {
    static init {
        // type initialization
    }

    init {
        // primary initialization logic
        // primary parameters are in scope here
    }

    finally {
        // finalization
    }
}
```

Classes and structs may declare primary-constructor parameters by adding an
argument list to the type header. The compiler synthesizes an instance
constructor whose signature matches those parameters.

For `class` / `struct`, parameter promotion is explicit:

* `val` parameter: promoted to an instance `val` auto-property.
* `var` parameter: promoted to an instance `var` auto-property.
* promoted parameters may specify an access modifier (`public`, `internal`, `protected`, `private`) before `val`/`var` to control synthesized property accessibility (default is `public`).
* no binding keyword: captured in synthesized private instance storage for member access, but not promoted to a public property.
* constructor calls must use invocation syntax (`Foo()`); a standalone type name (`Foo`) is not a value expression.
* semantic model note: unqualified identifier access to captured/promoted primary-constructor members resolves to the originating parameter symbol.

```raven
class Person(val name: string, var age: int) {
    func GetName() -> string => name
    func GetAge() -> int => age
}

val person = Person("Ada", 42)
val years = person.GetAge()
```

### `init(...)` declarations

`init(...)` member declarations are constructor-shape declarations.

* When used alongside the primary model above, they are **secondary constructors**.
* When used without type-header parameters/`init {}` blocks, they are a valid
  alternative syntax for initializing the object.

```raven
class Widget {
    init(name: string) { /* constructor-shape init */ }
    init(name: string, age: int) { /* secondary overload */ }
}
```

### Record declarations

Records provide value semantics and support three declaration forms:

```raven
record Person(name: string, age: int);        // defaults to record class
record class Person(name: string, age: int);  // explicit record class
record struct Point(x: int, y: int);          // explicit record struct
```

Primary-constructor semantics differ between nominal types and records:

* `class` / `struct`: only `val`/`var` parameters are promoted to properties; parameters without a binding keyword are captured in synthesized private instance storage for member access. Access modifiers on primary-constructor parameters are valid only when the parameter is promoted.
* `record class` / `record struct`: positional parameters are promoted to public auto-properties by default (as `val` when no binding keyword is specified, or `var` when `var` is specified). The compiler synthesizes value-based members from the record's **public** promoted properties (`Equals`, `GetHashCode`, deconstruction, `ToString`, copy/with behavior, and record equality operators). Non-public promoted properties are not part of that value shape, and the compiler reports a warning when such a parameter is declared.

Outside primary-constructor promotion, parameters are ordinary value/by-ref
parameters and must not use `val`/`var` binding keywords. This applies to
functions, methods, operator declarations, and indexer parameter lists.

```raven
record class Person(name: string, age: int);

val a = Person("Ada", 42)
val b = Person("Ada", 42)

val same = a == b
```

### Properties

Raven uses a property-first model. `val` and `var` define the public mutability
contract, while storage is an implementation detail.

#### Property kinds

* `val`: publicly read-only after initialization.
* `var`: publicly mutable after initialization.
* Accessors may be omitted when the declaration contract fully defines the
  intended surface.

`val` may declare `set`/`init` accessors. A `set` accessor on `val` must
be less accessible than the getter. `init` remains compatible with public
object-initializer assignment.

#### Storage (auto) properties

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

#### Computed properties

Computed properties provide implementation directly and do not use synthesized
storage unless explicitly needed:

```raven
val FullName: string => first + " " + last
```

#### `init` accessor and initialization phase

`init` permits assignment only during initialization and preserves `val`
semantics:

```raven
val Name: string { init; }
```

Initialization includes inline initializers, constructors (`init(...)`),
initializer blocks, and object initializers.

#### `field` and indexers

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

#### Private storage property lowering

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

### Events

Events expose a delegate-like member that supports handler subscription via
`+=` and `-=`. An event declaration specifies the `event` keyword, a name, and
the event handler type:

```raven
class Button {
    event Clicked: System.Action;
}
```

#### Event accessors

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

#### Auto-implemented events

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

### Class inheritance

Classes are sealed by default. Marking a class `open` permits inheritance.
`abstract` classes are implicitly open and cannot be instantiated directly
(`RAV0611`).

Applying `sealed` to a class or record class creates a closed hierarchy whose
direct subtypes are fixed at compile time. A sealed class is implicitly
abstract and cannot be instantiated directly. Writing `sealed abstract` is
allowed, but `abstract` is redundant and produces warning `RAV0340`.

A base class is specified with a colon followed by the type name:

```raven
open class Parent {}
class Child : Parent {}
```

If the base list contains additional types after the base class, each of those
entries must be interfaces. When no base class is provided, the compiler uses
`object` and treats the first entry as an interface instead:

```raven
class Worker : IDisposable, ILogger {
    func Dispose() -> () { /* ... */ }
    func Log(message: string) -> () { /* ... */ }
}
```

Implementations are matched by name, parameter count, and `ref`/`out`
modifiers. Each successfully matched member is emitted as a final override so
the CLR records the implementation in the type's interface map. See
[Interfaces](#interfaces) for interface declaration rules and inheritance.

For struct-like declarations (`struct` and `record struct`), the base list is interface-only; the runtime base remains `System.ValueType`.

If a derived class omits a constructor, the base class's parameterless
constructor is invoked automatically. Access modifiers apply as usual. An
instance constructor may chain to a specific base overload by adding a
constructor initializer between its parameter list and body:

```raven
open class Base { init(value: int) {} }

class Derived : Base {
    init(value: int): base(value) {
        // The base invocation runs before the derived body executes.
    }
}
```

The initializer is only available on ordinary instance constructors. Static constructors report `RAV0312`, and named
constructors continue to behave as user-defined factories without chaining.

Only single inheritance is supported.

#### Sealed hierarchies and `permits`

A class, record class, or interface declared with the `sealed` modifier creates a **sealed hierarchy**. For classes and
record classes, the `sealed` modifier implies `abstract`: a sealed type cannot be instantiated directly. Its set of direct
subtypes is fixed at compile time and is
determined in one of two ways:

**Same-file closure (default).** When no `permits` clause is given, any type in the same source file that directly
inherits from the sealed type is automatically part of the hierarchy. Types in other files may not inherit from it:

```raven
// shapes.rvn
sealed class Shape {}
class Circle : Shape {}   // OK — same file
class Square : Shape {}   // OK — same file
```

**Explicit permits.** An optional `permits` clause names the exact set of allowed direct subtypes. Only those types may
directly inherit from or implement the sealed type, regardless of file location:

```raven
sealed class Expr permits Lit, Add {}

class Lit : Expr {}
class Add : Expr {}
```

Sealed interfaces follow the same closure rules, but their direct subtypes are any classes, records, structs, or interfaces
that list the sealed interface directly in their base list:

```raven
sealed interface HttpResponse permits Success, NotFound {}

record class Success(status: int, body: string) : HttpResponse {}
record class NotFound(message: string) : HttpResponse {}
```

Direct cases in a sealed hierarchy are still ordinary named types. They may be declared next to the sealed root:

```raven
sealed interface HttpResponse

record class Success(status: int, body: string) : HttpResponse {}
record class NotFound(message: string) : HttpResponse {}
```

Nested declarations are also allowed, so a sealed interface can host its direct cases inside the interface body when the
developer wants that organization:

```raven
sealed interface HttpResponse {
    record class Success(status: int, body: string) : HttpResponse {}
    record class NotFound(message: string) : HttpResponse {}
}
```

For generic sealed hierarchies, nested direct cases are treated as hierarchy cases rather than ordinary CLR-style nested
generic types. They do **not** implicitly capture the outer type parameters. Instead, each case must explicitly choose the
instantiation of the sealed root that it implements or inherits from:

```raven
sealed interface Expr<T> {
    record NumericalExpr(value: float) : Expr<float>
    record StringExpr(value: string) : Expr<string>
    record AddExpr(left: Expr<float>, right: Expr<float>) : Expr<float>
}
```

Outer type parameters are therefore not in scope inside such direct cases unless the case declares its own type parameters.

This affects only the nested case declaration form. The sealed root itself still follows the normal generic rules everywhere
it is used as a type:

```raven
func Evaluate<T>(expr: Expr<T>) -> T { ... }   // OK
func Broken(expr: Expr) { ... }                // Error: missing type arguments
```

Sealed-hierarchy constituents are still ordinary Raven types, so they use the same generic parameter and `where`
constraint rules as any other type declaration. Member methods over sealed hierarchies likewise honor their own declared
constraints during body binding. This allows constrained generic evaluators to stay fully native to Raven and .NET-style
generic interfaces:

```raven
import System.Numerics.*

sealed interface Expr<T>
    where T: INumber<T> {
    record Literal<T>(Value: T) : Expr<T>
        where T: INumber<T>

    record Add<T>(Left: Expr<T>, Right: Expr<T>) : Expr<T>
        where T: INumber<T>
}

func Evaluate<T>(expr: Expr<T>) -> T
    where T: INumber<T> {
    return expr match {
        .Literal(val value) => value
        .Add(val left, val right) => Evaluate(left) + Evaluate(right)
    }
}
```

The sealed hierarchy contributes closed-family reasoning and nested-case lookup. Operator validity still comes from the
ordinary generic constraint system rather than a sealed-hierarchy-specific rule.

When nested cases are used, the containing sealed root acts as a logical qualifier for construction:

```raven
val value = Expr.NumericalExpr(40)
```

In pattern position, the same nested cases can be referenced through target-typed case patterns when the scrutinee already
determines the sealed root:

```raven
match expr {
    .NumericalExpr(val value) => value
    .AddExpr(val left, val right) => Evaluate(left) + Evaluate(right)
}
```

This target-typed `.Case(...)` form is ergonomic sugar for nested sealed cases. It is not required when the case types are
declared outside the sealed root.

When a `permits` clause is present, any attempt to derive from the sealed type outside the permitted list produces
`RAV0335`. If a type in the permits list does not actually inherit from the sealed base, the compiler reports `RAV0338`.

**Sealed record classes** follow the same rules. Applying `sealed` to a record class creates a sealed hierarchy of
records:

```raven
sealed record class Node {}
record class Leaf : Node {}
record class Branch : Node {}
```

**Match exhaustiveness.** The compiler uses the sealed hierarchy's permitted
subtypes for exhaustiveness analysis of `match` expressions. Closed branches
(sealed intermediates) are checked by concrete leaf type. Open intermediates
(non-sealed sub-hierarchies) must be covered by matching the intermediate type
itself.

When all required coverage types are handled, the match is considered exhaustive:

```raven
val result = expr match {
    Lit lit => lit.value
    Add add => eval(add.left) + eval(add.right)
}
```

Example with an open intermediate:

```raven
sealed record Expr
record Lit(Value: int) : Expr
abstract record BinaryExpr(Left: Expr, Right: Expr) : Expr
record Add(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
record Sub(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)

val result = expr match {
    Lit(val value) => value
    BinaryExpr(val left, val right) => eval(left) + eval(right)
}
```

**IL emission.** Sealed hierarchy base types are emitted as `abstract` (not IL
`sealed`) so that the CLR allows subclassing by the permitted types. A
`[ClosedHierarchy]` attribute is emitted on the base type carrying the
permitted `Type[]` array for runtime reflection.

For guidance on choosing sealed hierarchies versus discriminated unions, see
[Closed-shape types](language-specification.md#closed-shape-types).

### Partial types and members

Applying the `partial` modifier to a class, struct, record, or interface declaration allows the type to be defined across
multiple declarations in the same assembly. All declarations must use the `partial` modifier; omitting it on any declaration
produces a diagnostic and prevents the declarations from merging. When two declarations with the same name and containing scope
appear without `partial`, the compiler reports a duplicate-type diagnostic.

Partial type declarations combine their members and share a single type identity. Accessibility, `fileprivate` usage, and type parameters must match
across all parts. Class/struct/record parts also share the same nominal identity, so later parts should agree with the base type
shape established by the earlier declarations. File-scoped partial types must keep all parts in the same file. Interface bases contributed by different parts are merged.

Raven also supports partial methods, partial properties, and partial events inside partial nominal types.

A partial method must be split into:

- a declaration part with no body
- an implementation part with a block or expression body

```raven
partial class Logger {
    partial func WriteCore(message: string) -> unit;
}

partial class Logger {
    partial func WriteCore(message: string) -> unit {
        Console.WriteLine(message)
    }
}
```

A partial property must likewise have a declaration part and an implementation part. The declaration part uses accessor
signatures without bodies, while the implementation part must provide at least one accessor body.

```raven
partial class Person {
    partial var Name: string {
        get;
        set;
    }
}

partial class Person {
    partial var Name: string {
        get => field;
        set => field = value;
    }
}
```

Partial events follow the same pattern. Field-like `partial event` declarations act as the declaration part, and the
implementation part provides `add`/`remove` bodies.

```raven
partial class Notifier {
    partial event Changed: System.Action;
}

partial class Notifier {
    partial event Changed: System.Action {
        add { }
        remove { }
    }
}
```

All partial member parts must use the `partial` modifier, live inside a partial type, and have the same signature. A declaration
without an implementation, or an implementation without a declaration, produces a diagnostic. Partial properties and partial
events do not allow an auto/field-like implementation part; the implementing declaration must provide real accessor bodies.

### Parameter semantics

Method, constructor, and accessor parameters are immutable by default. They
behave like `let` bindings: the compiler rejects assignments that attempt to
rebind the parameter name. Add the `var` modifier when a parameter must be
reassigned inside the body—for example, to reuse a scratch variable or to
satisfy an `out` contract.

```raven
func clamp(min: int, value: int, max: int) -> int {
    // value = ...    // error: parameters are immutable by default
    return Math.Max(min, Math.Min(value, max))
}

func TryParse(text: string, out result: int) -> bool {
    result = 0      // ok: the parameter explicitly opts into mutation
    /* ... */
}
```

Declaring a parameter with `ref`, `out`, or `in` passes the argument by
reference. The callee receives an alias to the caller's storage and callers
supply such arguments with the address-of operator `&expr`. Plain parameters are
readonly. `ref` parameters can be read and assigned, `in` parameters are
readonly aliases, and `out` parameters must be assigned before the method
returns. These modifiers already imply by-reference passing, so their declared
types stay plain: use `ref value: int`, not `ref value: &int`. Explicit
`&Type` parameters remain available when the by-reference type itself is the
intended type annotation.

```raven
func Increment(ref value: int) -> () {
    value = value + 1
}

var total = 41
Increment(ref total)
Console.WriteLine(total) // prints 42
```

### Method overloading

Functions and methods may share a name as long as their parameter counts or
types differ. Overload resolution selects the best match based on argument
types, `out`/by-ref modifiers, and nullability. Ambiguous calls produce a
diagnostic.

```raven
class Printer {
    func Print(x: int) -> () => Console.WriteLine(x)
    func Print(x: string) -> () => Console.WriteLine(x)
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

### Operator declarations

Classes and structs can declare overloadable operators using function-style
syntax where the operator token is the function name:
`static func <operator>(...) -> ...`.
Supported tokens are `+`, `-`, `*`, `/`, `%`, `^`, `&`, `&&`, `and`, `|`, `||`,
`or`, `<<`, `>>`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `!`, `~`, `++`, `--`.
Operators mirror methods: they take a parenthesized parameter list, optional
return-type arrow, and either a block body or expression body. The parameter
count must match the chosen operator (unary or binary). Operator declarations
are supported in classes, structs, and extensions.

```raven
class Vector {
    static func +(left: Vector, right: Vector) -> Vector => Add(left, right)
    static func -(value: Vector) -> Vector { /* ... */ }
}
```

Conversions follow the same style:
`static func implicit(value: SourceType) -> TargetType` and
`static func explicit(value: SourceType) -> TargetType`.
These conversion members are resolved using the same lookup rules as other
static members.

For null checks, prefer `is null` / `is not null` when you need strict
nullability narrowing. Raven's analyzer recommends these forms over
`== null`/`!= null` and provides a code fix. Pointer-like comparisons are
excluded from that recommendation.

### Invocation operator

Declaring a method named `self` makes instances of the type invocable with the
call operator `()`.

```raven
class Adder {
    func self(x: int, y: int) -> int => x + y
}

val add = Adder()
val sum = add(1, 2) // calls self(1, 2)
```

Invocation operators can themselves be overloaded by providing multiple `self`
methods with different parameter signatures.

## Interfaces

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
