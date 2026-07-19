# Raven for C# developers

Raven runs on .NET and uses the .NET type system and libraries, but it does not
require C#'s usual source structure. Learning Raven is partly a process of
separating useful object-oriented modeling from habits that exist because C#
traditionally requires code to live inside types.

The Raven question is not “how do I rewrite this C# syntax?” It is “what does
this code represent, and what is the most direct Raven shape for it?”

This guide presents starting points, not mechanical rewrite rules. Raven fully
supports classes, interfaces, methods, properties, and mutable objects when they
fit the problem.

## A quick translation table

| Common C# starting point | Raven starting point |
| --- | --- |
| `Program.Main` | Top-level statements or a plain `Main` function |
| Static utility class | Top-level or namespace-level functions |
| One-method service interface | Function parameter |
| DTO class | Record class or record struct |
| Primitive used for a domain concept | Record wrapper with validation |
| `null` for domain absence | `Option<T>` |
| Exception for an expected outcome | `Result<T, E>` |
| Enum plus associated nullable fields | Union with case payloads |
| `switch` plus type and null checks | Structural `match` |
| Mutable local by default | `val`, with `var` when mutation is intentional |
| Object hierarchy for a closed set of variants | Union |
| Class with identity or resource lifecycle | Class |
| Open implementation boundary | Interface, class, or struct implementation |

## Entry points do not require a `Program` class

A traditional C# application puts its entry point on a type:

```csharp
public static class Program
{
    public static void Main()
    {
        Console.WriteLine("Hello");
    }
}
```

A small Raven application can consist of top-level statements:

```raven
import System.Console.*

WriteLine("Hello")
```

When a named entry point is useful, declare a plain function:

```raven
import System.Console.*

func Main() -> () {
    WriteLine("Hello")
}
```

Create an application class only if the application itself has meaningful state
or behavior to encapsulate—not because the runtime entry point needs a home.

## Utility classes become plain functions

C# frequently uses static classes as namespaces for behavior:

```csharp
public static class CarrierNames
{
    public static string Normalize(string name) =>
        name.Trim().ToUpperInvariant();
}
```

The Raven version names the operation directly:

```raven
func NormalizeCarrier(name: string) -> string {
    return name.Trim().ToUpperInvariant()
}
```

Parsing, validation, formatting, calculations, and workflow orchestration are
good candidates for plain functions. Put behavior on a class or record when it
belongs to that type's vocabulary or needs its encapsulated state.

## Inject one operation as one function

A C# dependency is often represented by an interface even when it contains one
operation:

```csharp
public interface ITemperatureReader
{
    Task<decimal> ReadAsync();
}

public sealed class Monitor(ITemperatureReader reader)
{
    public async Task<bool> IsTooHot(decimal limit) =>
        await reader.ReadAsync() > limit;
}
```

In Raven, a function type can describe that capability directly:

```raven
import System.Threading.Tasks.*

async func IsTooHot(read: () -> Task<decimal>, limit: decimal) -> Task<bool> {
    val temperature = await read()
    return temperature > limit
}
```

Production code can pass a device-reading function and tests can pass a
deterministic function. Use an interface when the dependency is genuinely an
open protocol with several related operations or implementations. Use a class
when it owns state, disposal, or a resource lifecycle.

## DTOs become explicit data shapes

A record expresses immutable domain data without a handwritten property and
constructor shell:

```csharp
public sealed record ShipmentRequest(
    string Id,
    string Carrier,
    int WeightKg);
```

```raven
record class ShipmentRequest(
    val Id: string,
    val Carrier: string,
    val WeightKg: int)
```

Choose a record struct for value semantics and a record class for reference
semantics. Choose an ordinary class when identity or encapsulated mutable state
matters more than structural value behavior.

## Domain primitives become domain types

C# applications often pass primitive values whose meaning exists only in names
and conventions:

```csharp
static Result Register(int year) { /* ... */ }
```

Raven can give the value its own identity and keep validation at its boundary:

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

This is an ordinary record with a restricted constructor, not special compiler
support for opaque aliases. A `Year` cannot be confused with every other `int`.

## Absence becomes `Option`

Nullable references commonly make absence implicit in C#:

```csharp
Customer? FindCustomer(string id);
```

Raven domain APIs prefer to state absence in the return type:

```raven
func FindCustomer(id: string) -> Option<Customer> {
    // ...
    return None
}
```

Handle the alternatives with a match:

```raven
val message = FindCustomer("C-100") match {
    Some(val customer) => "Found ${customer.Name}"
    None => "Customer not found"
}
```

Raven still supports nullable values for .NET interop. `Option<T>` is the
preferred domain shape when absence is expected and meaningful.

## Expected failure becomes `Result`

Exceptions are useful for unexpected faults. They are less useful when callers
are expected to branch on validation or lookup outcomes.

```csharp
static Quote BuildQuote(string id)
{
    var request = FindRequest(id)
        ?? throw new RequestNotFoundException(id);
    return CalculateQuote(request);
}
```

A Raven API can expose the expected failure:

```raven
union QuoteError {
    case RequestNotFound(id: string)
    case InvalidWeight(weight: int)
}

func BuildQuote(id: string) -> Result<Quote, QuoteError> {
    val request = FindRequest(id)
        .IsOkOr(() => .RequestNotFound(id))?

    if request.WeightKg < 1 {
        return Error(.InvalidWeight(request.WeightKg))
    }

    return Ok(CalculateQuote(request))
}
```

The `?` expression keeps the successful path linear while preserving the typed
failure in the function signature. Use `match` when recovery deserves to be
shown explicitly.

## State plus payload becomes a union

C# models sometimes combine an enum with fields that are valid only for some
states:

```csharp
public enum DeliveryStatus { Pending, Delivered, Failed }

public sealed record Delivery(
    DeliveryStatus Status,
    DateTime? DeliveredAt,
    string? FailureReason);
```

A Raven union puts the data on the case where it is valid:

```raven
import System.*

union DeliveryStatus {
    case Pending
    case Delivered(at: DateTime)
    case Failed(reason: string)
}
```

```raven
func Describe(status: DeliveryStatus) -> string {
    return status match {
        .Pending => "Pending"
        .Delivered(val at) => "Delivered at $at"
        .Failed(val reason) => "Failed: $reason"
    }
}
```

Use an enum when cases are only named constants. Use a union for a closed family
of cases with different payloads. Use an interface or class hierarchy when the
family must remain open to new third-party implementations.

## Immutability is the visible default

C# locals are mutable unless the programmer arranges otherwise:

```csharp
var total = subtotal + tax;
total -= discount;
```

Raven distinguishes the intent at the declaration:

```raven
val subtotal = 1000
val tax = 250
var total = subtotal + tax
total = total - discount
```

Use `val` for a binding that does not change and `var` when mutation is part of
the algorithm. Mutable objects and fields remain available when stateful
modeling is appropriate.

## Pattern matching replaces scattered inspection

Raven patterns work across unions, options, results, records, tuples, and other
structural shapes. Code that would distribute null, type, and status checks
across several C# branches can often become one visible decision:

```raven
union CustomerError {
    case NotFound(id: string)
    case Unavailable
}

func Message(result: Result<Customer, CustomerError>) -> string {
    return result match {
        Ok(val customer) => "Welcome ${customer.Name}"
        Error(.NotFound(val id)) => "No customer named $id"
        Error(.Unavailable) => "Customer service unavailable"
    }
}
```

The point is not merely a shorter `switch`. The data model and the decision use
the same case vocabulary, so invalid combinations are harder to represent.

## Classes are still the Raven way when the domain has objects

Do not translate every C# class into a collection of functions. A stateful
connection, aggregate with identity, actor, cache, UI component, or resource
owner can still be most honestly represented by a class:

```raven
class GreenhouseDevice private (val DeviceId: string) {
    static func Connect(deviceId: string) -> Result<GreenhouseDevice, string> {
        return Ok(GreenhouseDevice(deviceId))
    }

    func ReadTemperature() -> Result<decimal, string> {
        // Read from the connected device.
        return Ok(21.5)
    }
}
```

The Raven difference is that the class has a reason to exist: it represents a
device with identity and a connection boundary. Domain calculations around its
readings can remain plain functions, and those functions can be injected back
into object-oriented components when useful.

## A practical decision sequence

When translating a design from C#, ask:

1. Is this just an operation? Start with a function.
2. Is this one required capability? Consider a function parameter.
3. Is this immutable data or a domain value? Consider a record.
4. Is this a closed set of meaningful alternatives? Consider a union.
5. Is absence or failure expected? Use `Option` or `Result`.
6. Does this concept have identity, state, lifecycle, or encapsulation? Use a
   class.
7. Must unrelated implementations participate in an open contract? Use an
   interface.

This is the main adjustment when moving from C#: classes remain available, but
they stop being the mandatory starting point.

For more examples, continue with [Domain modeling in
Raven](lang/domain-modeling.md) and the [language
introduction](introduction.md).
