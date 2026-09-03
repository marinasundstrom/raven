# Domain modeling in Raven

Raven supports functional and object-oriented domain modeling as first-class
parts of one language. Objects are not just an interoperability concession, and
functions are not restricted to methods. The useful question is not which camp
a program belongs to, but which shape makes each domain concept honest and easy
to compose.

The usual starting point is:

- records and structs for values
- unions for closed sets of states or outcomes
- functions for rules, transformations, and workflows
- function parameters for small behavioral dependencies
- classes for identity, lifecycle, encapsulated mutation, and stateful resources
- interfaces for open-ended polymorphism across multiple implementations

These are guidelines rather than architectural restrictions. A Raven application
can use all of them and still have a coherent model.

This guide deliberately uses established domain-modeling and design-pattern
names. Raven should help developers recognize a recurring problem, learn the
name used to discuss it, see the Raven shape, and run a corresponding sample.
Patterns are vocabulary and starting points rather than mandatory architecture.
Substantial additions to this guide should normally include a focused Playground
example that demonstrates the pattern in working code.

Plain functions can be declared directly; they do not require a static utility
class. Likewise, an application can use top-level statements or a plain `Main`
function without introducing a `Program` class.

For developers coming from C#, this can require some unlearning. What Raven asks
you to leave behind is not object-oriented design, but the reflex that all code
must first be assigned to a class. Model the domain concept first; choose its
language construct second.

## Give domain values their own identity

Values such as `string`, `int`, or `Guid` often carry domain meaning only
through variable names and conventions. For example, a function that accepts
two `Guid` values cannot prevent callers from swapping a customer ID and an
order ID. This modeling problem is often called **primitive obsession**.

Raven can replace the primitive with a strongly typed ID, a validated value
object, or an opaque object. Choose the smallest shape that expresses the
domain boundary.

### Prevent identifiers from being mixed up

Give each kind of identifier its own small type so identifiers from different
domains cannot be interchanged accidentally. This is commonly called a
**strongly typed ID**.

```raven
record struct CustomerId private (Value: Guid) {
    static func From(value: Guid) -> CustomerId => CustomerId(value)
}

record struct OrderId private (Value: Guid) {
    static func From(value: Guid) -> OrderId => OrderId(value)
}

func LoadOrder(customerId: CustomerId, orderId: OrderId) -> Option<Order> {
    // The two arguments cannot be swapped even though both IDs store a Guid.
    return None
}
```

The restricted constructor makes creation policy explicit. A domain may offer
`New`, `Parse`, or persistence-oriented factories instead of a general `From`
function. The public `Value` property is reasonable when infrastructure needs
the storage value and revealing it does not violate an invariant.

### Validate a value at its boundary

A type alias gives another name to an existing type; it does not create a new
domain identity. When an integer means a year, quantity, account number, or
temperature, wrap it in a record or record struct and validate it at creation.

```raven
union YearError {
    case OutOfRange(value: int)
}

record struct Year private (Value: int) {
    static func Create(value: int) -> Result<Year, YearError> {
        if value < 1 {
            return Error(.OutOfRange(value))
        }

        return Ok(Year(value))
    }
}
```

`Year` is now distinct from `int`. Its private primary constructor ensures that
callers use `Create`, while the result type makes validation failure part of the
API. This is an ordinary record and accessibility pattern, not a special opaque
type feature.

As a record, `Year` has value equality: two valid instances containing the same
year represent the same domain value. This kind of model is commonly called a
**value object**. Use a record class when reference storage fits better, or a
record struct when compact value semantics fit the domain.

### Keep the representation from escaping

Sometimes even a read-only `Value` property exposes too much. Callers should be
able to construct the value through approved factories and ask domain
questions, but not extract or mutate its underlying representation. This API
shape is often described as an **opaque object**.

```raven
union AccessTokenError {
    case Empty
}

class AccessToken private (private val value: string) {
    static func Create(value: string) -> Result<AccessToken, AccessTokenError> {
        if String.IsNullOrWhiteSpace(value) {
            return Error(.Empty)
        }

        return Ok(AccessToken(value))
    }

    func Matches(candidate: string) -> bool => value == candidate

    override func ToString() -> string? => "<access-token>"
}
```

Unlike the strongly typed ID example, `AccessToken` does not expose a `Value`
property. Its useful surface is behavioral. This is appropriate for secrets,
capabilities, validated handles, or values whose raw representation should not
spread through application code.

Opacity is an API/accessibility pattern here, not a new Raven type category.
Serialization and persistence adapters may need a deliberately narrow internal
operation rather than making the representation public.

Use a class when instances have identity independent of their current values.
Use a record when structural value equality is part of the concept.

## Make valid states explicit

Use a union when the domain has a closed set of meaningful alternatives,
especially when different alternatives carry different data. This is Raven's
direct form of an **algebraic data type (ADT)**: the union is a sum of its cases,
and each case payload is a product of its fields.

```raven
union GreenhouseStatus {
    case Healthy
    case TooCold(actual: decimal, minimum: decimal)
    case TooHot(actual: decimal, maximum: decimal)
    case SensorUnavailable(message: string)
}

func Describe(status: GreenhouseStatus) -> string {
    return status match {
        .Healthy => "Greenhouse conditions are healthy"
        .TooCold(let actual, let minimum) => "$actual is below $minimum"
        .TooHot(let actual, let maximum) => "$actual is above $maximum"
        .SensorUnavailable(let message) => "Sensor unavailable: $message"
    }
}
```

This keeps impossible combinations out of a single record full of flags and
optional fields. Pattern matching then makes the cases visible where decisions
are made.

Use an enum when cases are simple named constants without case-specific data.
Use an open class or interface hierarchy when third parties must add new cases.
This approach is also described as **making illegal states unrepresentable**:
the model has no case for combinations the domain does not permit.

When the alternatives need their own implementations, inheritance, or
interface contracts, use a sealed class or interface hierarchy. Raven refers to
that object-oriented closed-family form as a **generalized data type (GDT)**.
Like a union ADT, a GDT has a compiler-known set of direct alternatives and can
be matched exhaustively; unlike a union, its alternatives are ordinary named
.NET types with hierarchy semantics.

```raven
sealed interface DeliveryMethod

record class Pickup(Store: string) : DeliveryMethod
record class Courier(Driver: string, Window: string) : DeliveryMethod

func Describe(method: DeliveryMethod) -> string {
    return method match {
        Pickup(let store) => "Collect at $store"
        Courier(let driver, let window) => "$driver arrives $window"
    }
}
```

Choose an ADT union when explicit cases and payload-oriented matching are the
center of the model. Choose a GDT sealed hierarchy when each alternative
benefits from its own type identity, implementation, or inherited contract.

### Expose operations only in valid states

When an object's valid operations depend on its lifecycle, the **typestate
pattern** can encode that state in its type. Empty state types such as `Closed`
and `Open` are commonly called **marker types**. The state parameter is a
**phantom type parameter** when it contributes to the connection's type but is
not stored as instance data.

A non-generic base provides a state-erased view for operations that work with
every connection. Generic derived connections retain the state where it
matters:

```raven
import ClosedConnection.*
import OpenConnection.*

class Closed
class Open

open class Connection(private val address: string) {
    internal val Address: string => address
}

class Connection<State> : Connection {
    internal init(address: string) : base(address) {}
}

func Connect(address: string) -> Connection<Closed> {
    return Connection<Closed>(address)
}

func Describe(connection: Connection) -> string {
    return "Connection to ${connection.Address}"
}

extension ClosedConnection for Connection<Closed> {
    func Open() -> Connection<Open> {
        Console.WriteLine("Opening ${self.Address}")
        return Connection<Open>(self.Address)
    }
}

extension OpenConnection for Connection<Open> {
    func Send(message: string) {
        Console.WriteLine("Sending '$message' to ${self.Address}")
    }

    func Close() -> Connection<Closed> {
        Console.WriteLine("Closing ${self.Address}")
        return Connection<Closed>(self.Address)
    }
}

let closed = Connect("example.test:443")
Console.WriteLine(Describe(closed))

let opened: Connection<Open> = closed.Open()
opened.Send("hello")
let closedAgain: Connection<Closed> = opened.Close()
Console.WriteLine(Describe(closedAgain))
```

`Describe` accepts the non-generic `Connection` because it does not depend on
the state. `Send` is available on `Connection<Open>` but not on
`Connection<Closed>`, while only a closed connection offers `Open`. The
extensions provide state-specific public vocabulary, and the class hierarchy
exposes only the narrow internal constructor and address surface needed to
perform transitions.

This pattern is useful in Raven: it improves API discovery, prevents many
invalid calls, and separates state-independent from state-dependent operations.
It resembles the pattern used with Rust marker types and specialized `impl`
blocks, but the two versions do not have identical power. Raven extensions are
not privileged partial definitions; they do not reopen a class or gain access
to its private representation. Members needed by an extension must therefore
be exposed at least within the assembly, as in this example. A future
trait-oriented design may offer a more direct way to associate capabilities
with a type and state.

The .NET object model also does not give a transition Rust-style move semantics.
Calling `Open` returns a new typed handle but does not make every alias of the
old `Connection<Closed>` value unusable. Typestate still provides meaningful
compile-time guidance and sequencing, but a real socket or other stateful
resource must enforce its lifecycle at runtime when aliases or concurrent
access are possible.

### Separate construction from validity

Several patterns can appear together in a staged construction API, but each
solves a different problem.

#### Fluent interface

A **fluent interface** makes an operation return an object that offers the next
useful operation. This allows calls to read as a single chain. For example, an
invoice API might let a caller describe the recipient, add lines, and then
issue the invoice:

```raven
let invoice = InvoiceDraft.Start()
    .BillTo(customer)
    .AddLine("Keyboard", 99)
    .DueInDays(30)
    .Issue()
```

Fluency describes how the API is used. It does not by itself guarantee that
the calls occur in a valid order; every operation could still return the same
type.

#### Builder pattern

The **builder pattern** separates construction from the resulting object. It is
useful when creating an object requires several inputs, optional settings, or
validation that would make one constructor difficult to understand. A report
builder, for example, can collect a date range, filters, grouping, and output
format before it creates an immutable `Report`.

A builder does not have to be fluent or staged. Its configuration methods may
return nothing, and `Build` may validate all required data only at the end.

#### Typestate pattern

The **typestate pattern** represents each valid stage with a distinct
compile-time type. The methods available on a value therefore depend on its
current state. A payment workflow might expose `Authorize` only for a pending
payment and `Capture` only for an authorized payment:

```text
PendingPayment → AuthorizedPayment → CapturedPayment
```

The same idea can enforce staged construction:

```text
NeedsName → NeedsPrice → ReadyToBuild
```

If `Build` exists only on `ReadyToBuild`, callers cannot skip the required name
or price while remaining in the typed API. Typestate supplies the ordering and
safety that a fluent interface or ordinary builder does not provide alone.

#### F-bounded polymorphism

**F-bounded polymorphism**, also called a **recursive generic constraint**,
lets a reusable base abstraction refer to the concrete type that implements it:

```raven
abstract class Builder<TSelf>
    where TSelf: Builder<TSelf> {
}
```

For example, purchase-order and shipment builders might inherit common
operations such as `WithAuditInfo`. The recursive constraint allows that shared
operation to preserve the appropriate concrete builder family instead of
falling back to a general `Builder` type in the middle of a fluent chain. This
technique is closely related to the curiously recurring template pattern
(CRTP) in C++.

The constraint preserves the concrete family; it does not enforce the order of
construction steps. Distinct typestate types are responsible for that.

#### Phantom types and marker types

A **phantom type** appears in another type's signature without contributing
instance data at runtime. For example, `OrderBuilder<NeedsShippingAddress>` and
`OrderBuilder<ReadyToSubmit>` can have the same stored fields while the type
argument controls which operations are available. Empty state types can name
those stages:

```raven
class NeedsShippingAddress
class ReadyToSubmit
```

A **marker type** similarly carries no domain data and exists to classify a
type, often so that a generic constraint can require a capability or category.
The terms overlap when an empty builder-state type is used only through a type
parameter or constraint. Calling it a phantom type emphasizes that it has no
runtime role; calling it a marker type emphasizes its use in classification.

When these patterns are combined, the result can be described concisely as a
**staged fluent builder implemented using the typestate pattern, with F-bounded
polymorphism to preserve the concrete builder type and phantom or marker types
to encode its construction state**. The builder pattern explains the purpose,
typestate explains the safety and ordering, and the recursive generic
constraint explains how the concrete builder remains constrained through the
chain.

## Keep rules as functions

Domain rules often need only their inputs. Keeping them as plain functions makes
their dependencies visible and lets the same rule work in workflows, methods,
tests, and collection operations.

```raven
record struct ClimateRange(Minimum: decimal, Maximum: decimal)
record struct ClimateReading(Temperature: decimal)

func Evaluate(range: ClimateRange, reading: ClimateReading) -> GreenhouseStatus {
    if reading.Temperature < range.Minimum {
        return .TooCold(reading.Temperature, range.Minimum)
    }

    if reading.Temperature > range.Maximum {
        return .TooHot(reading.Temperature, range.Maximum)
    }

    return .Healthy
}
```

There is no class wrapper because the rule has no identity or lifecycle. If the
rule naturally belongs to a type's public vocabulary, it can instead be a method;
the choice does not change the rest of Raven's type system.

## Inject behavior with function parameters

For a dependency that is one operation, accept the operation itself. Raven's
function types make the required capability precise.

```raven
func ObserveClimate(
    read: () -> Result<ClimateReading, string>,
    evaluate: (ClimateReading) -> GreenhouseStatus,
    publish: (GreenhouseStatus) -> ()) -> Result<GreenhouseStatus, string> {
    let reading = read()?
    let status = evaluate(reading)
    publish(status)
    return Ok(status)
}
```

The function does not need to know whether `read` calls hardware, reads a file,
or returns simulated data. Tests can inject fixed functions without mocks or
service classes.

Prefer a class or interface when the dependency is a cohesive protocol with
several related operations, shared state, disposal, or a lifecycle contract.
Function injection should remove accidental ceremony, not hide a real object.
For a single capability, this is sometimes called **function-based dependency
injection**.

## Compose functional rules with stateful objects

Objects and functions can meet at a narrow boundary. A class can own a device
connection while domain decisions remain independent functions.

```raven
class GreenhouseDevice private (val DeviceId: string) {
    static func Connect(deviceId: string) -> Result<GreenhouseDevice, string> {
        return Ok(GreenhouseDevice(deviceId))
    }

    func ReadClimate() -> Result<ClimateReading, string> {
        // Read from the connected device.
        return Ok(ClimateReading(21.5))
    }
}

func CheckDevice(device: GreenhouseDevice, range: ClimateRange) -> Result<GreenhouseStatus, string> {
    return ObserveClimate(
        () => device.ReadClimate(),
        reading => Evaluate(range, reading),
        status => LogStatus(device.DeviceId, status))
}
```

`GreenhouseDevice` represents identity and a connection lifecycle. `Evaluate`
remains a pure domain rule. `ObserveClimate` coordinates capabilities through
function parameters. Each construct is used for what it describes best.
Separating pure decisions from stateful integration in this way is commonly
called a **functional core with an imperative shell**.

## Model expected outcomes as data

Use `Option<T>` when a value may legitimately be absent and `Result<T, E>` when
an operation has an expected failure mode. Define a domain error union when
callers need to make decisions based on the failure.

```raven
union RegistrationError {
    case InvalidYear(YearError)
    case DuplicateName(name: string)
}

func Register(name: string, yearValue: int) -> Result<Registration, RegistrationError> {
    let year = Year.Create(yearValue).MapError(error => .InvalidYear(error))?
    return Ok(Registration(name, year))
}

record class Registration(val Name: string, val Year: Year)
```

Exceptions remain appropriate for unexpected failures and are unavoidable at
some .NET boundaries. The domain-facing API should still distinguish expected
outcomes from exceptional faults.
Composing workflows from `Result`-returning operations is sometimes called
**railway-oriented programming**.

## Keep identity stable as data changes

Some domain objects must remain the same conceptual thing while their data
changes. Give that identity its own type, and use a class or record class for
the object depending on the desired equality contract. In domain-driven design,
this kind of model is called an **entity**.

```raven
record struct CustomerId private (Value: Guid) {
    static func From(value: Guid) -> CustomerId => CustomerId(value)
}

class Customer(
    val Id: CustomerId,
    var DisplayName: string) {

    func Rename(displayName: string) {
        DisplayName = displayName
    }
}
```

Changing `DisplayName` does not create a different customer. The `CustomerId`
establishes continuity, while the class encapsulates the entity's lifecycle and
allowed mutation.

## Choosing a modeling shape

| Domain characteristic | Good starting shape |
| --- | --- |
| Immutable value with structural equality | `record struct` or `record class` |
| Validated primitive with its own identity | Record with a restricted constructor and factory function |
| Closed set of states with case-specific data | `union` |
| Simple closed set of named constants | `enum` |
| Closed object-oriented family with distinct implementations | Sealed class or interface hierarchy |
| Stateless rule or transformation | Plain function |
| One-operation dependency | Function parameter |
| Entity with stable identity | `class` or `record class` |
| Encapsulated mutation or resource lifecycle | `class` |
| Open family of interchangeable implementations | `interface` plus classes or structs |

Start with the least machinery that expresses the domain accurately. Add an
object boundary when the domain has an object boundary; keep behavior as
functions when it is simply behavior. Raven is designed so that this decision
can be made concept by concept rather than once for the entire application.
