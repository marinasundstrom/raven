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

A type alias gives another name to an existing type; it does not create a new
domain identity. When an integer means a year, quantity, account number, or
temperature, wrap it in a record or record struct.

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

Use a class instead when instances have identity independent of their current
values. Use a struct when value semantics and compact representation fit the
domain.

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
