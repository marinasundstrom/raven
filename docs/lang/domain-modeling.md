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

    override func ToString() -> string => "<access-token>"
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
especially when different alternatives carry different data.

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
        .TooCold(val actual, val minimum) => "$actual is below $minimum"
        .TooHot(val actual, val maximum) => "$actual is above $maximum"
        .SensorUnavailable(val message) => "Sensor unavailable: $message"
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
    val reading = read()?
    val status = evaluate(reading)
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
    val year = Year.Create(yearValue).MapError(error => .InvalidYear(error))?
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
| Stateless rule or transformation | Plain function |
| One-operation dependency | Function parameter |
| Entity with stable identity | `class` or `record class` |
| Encapsulated mutation or resource lifecycle | `class` |
| Open family of interchangeable implementations | `interface` plus classes or structs |

Start with the least machinery that expresses the domain accurately. Add an
object boundary when the domain has an object boundary; keep behavior as
functions when it is simply behavior. Raven is designed so that this decision
can be made concept by concept rather than once for the entire application.
