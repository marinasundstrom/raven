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

## Gradual adoption

Familiar C# structures—classes, interfaces, methods, loops, mutable objects,
nullable values, exceptions, and ordinary .NET APIs—can be expressed directly
in Raven. You can start with a close translation and introduce Raven features
as you learn them.

The comparisons below therefore use a gradual lens: the common C# approach,
the same approach in Raven when useful, and a Raven feature to consider when it
expresses the problem better. No third step is mandatory. For domain code,
Raven generally prefers `Option<T>` for expected absence and `Result<T, E>` for
expected failure; nullable values remain useful at .NET boundaries, and
exceptions remain appropriate for unexpected faults.

## A quick translation table

| Common C# starting point | Raven starting point |
| --- | --- |
| `Program.Main` | Top-level statements or a plain `Main` function |
| Static utility class | Top-level or namespace-level functions |
| One-method service interface | Function parameter |
| DTO class | Record class or record struct |
| Primitive used for a domain concept | Record wrapper with validation |
| `null` as expected domain absence | `Option<T>` |
| Exception for an expected outcome | `Result<T, E>` |
| Enum plus associated nullable fields | Union with case payloads |
| `switch` plus type and null checks | Structural `match` |
| Mutable local by default | `let`, with `var` when mutation is intentional |
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

## Utility classes can become plain functions

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
    let temperature = await read()
    return temperature > limit
}
```

Production code can pass a device-reading function and tests can pass a
deterministic function. Use an interface when the dependency is genuinely an
open protocol with several related operations or implementations. Use a class
when it owns state, disposal, or a resource lifecycle.

## DTOs can become explicit data shapes

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

## Domain primitives can become domain types

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

## Prefer `Option` for expected absence

Nullable references commonly make absence implicit in C#:

```csharp
Customer? FindCustomer(string id);
```

The same nullable shape is valid Raven:

```raven
func FindCustomer(id: string) -> Customer? {
    // ...
    return null
}
```

When absence is an expected domain outcome, prefer to state it in the return
type:

```raven
func FindCustomer(id: string) -> Option<Customer> {
    // ...
    return None
}
```

Handle the alternatives with a match:

```raven
let message = FindCustomer("C-100") match {
    Some(let customer) => "Found ${customer.Name}"
    None => "Customer not found"
}
```

Raven still supports nullable values for .NET interop. `Option<T>` is the
preferred domain shape when absence is expected and meaningful. Raven also
projects selected framework APIs such as `TryParse` and `TryGetValue` into
`Option` or `Result`; projects can disable those projections when they need the
ordinary CLR signatures.

## Prefer `Result` for expected failure

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

The same exception-based approach is valid Raven:

```raven
func BuildQuote(id: string) -> Quote {
    let request = FindRequestOrNull(id)
        ?? throw RequestNotFoundException(id)
    return CalculateQuote(request)
}
```

When callers are expected to handle the failure, prefer to expose it in the
return type:

```raven
union QuoteError {
    case RequestNotFound(id: string)
    case InvalidWeight(weight: int)
}

func BuildQuote(id: string) -> Result<Quote, QuoteError> {
    let request = FindRequest(id)
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

## State plus payload can become a union

C# models sometimes combine an enum with fields that are valid only for some
states:

```csharp
public enum DeliveryStatus { Pending, Delivered, Failed }

public sealed record Delivery(
    DeliveryStatus Status,
    DateTime? DeliveredAt,
    string? FailureReason);
```

Raven can use the same enum-plus-record model:

```raven
import System.*

enum DeliveryStatus {
    Pending
    Delivered
    Failed
}

record class Delivery(
    val Status: DeliveryStatus,
    val DeliveredAt: DateTime?,
    val FailureReason: string?)
```

That model is valid Raven and may be the clearest choice, especially when it
matches an existing .NET contract. When each state has different valid data, a
custom union can keep the payload on the case where it belongs:

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
        .Delivered(let at) => "Delivered at $at"
        .Failed(let reason) => "Failed: $reason"
    }
}
```

An enum fits cases that are only named constants. A union fits a closed family
whose cases carry different payloads. An interface or class hierarchy fits a
family that must remain open to new third-party implementations.

## Immutability is the visible default

C# locals are mutable unless the programmer arranges otherwise:

```csharp
var total = subtotal + tax;
total -= discount;
```

Raven distinguishes the intent at the declaration:

```raven
let subtotal = 1000
let tax = 250
var total = subtotal + tax
total = total - discount
```

Use `let` for a lexical binding that does not change and `var` when mutation is part of
the algorithm. Mutable objects and fields remain available when stateful
modeling is appropriate.

## Pattern matching can replace scattered inspection

Raven patterns work across unions, options, results, records, tuples,
sequences, and other structural values. One gradual starting point is to carry
familiar boolean conditions across directly. C# commonly classifies an object
like this:

```csharp
static string Describe(Shipment shipment)
{
    if (shipment.IsPriority && shipment.WeightKg > 20)
        return "Heavy priority shipment";
    if (shipment.IsPriority)
        return "Priority shipment";
    return "Standard shipment";
}
```

The same conditions and early returns are valid Raven:

```raven
func Describe(shipment: Shipment) -> string {
    if shipment.IsPriority && shipment.WeightKg > 20 {
        return "Heavy priority shipment"
    }
    if shipment.IsPriority {
        return "Priority shipment"
    }
    return "Standard shipment"
}
```

When the problem is best understood as recognizing structural shapes, property
patterns can express those shapes directly and compose with `match`:

```raven
func Describe(shipment: Shipment) -> string {
    return shipment match {
        { IsPriority: true, WeightKg: > 20 } => "Heavy priority shipment"
        { IsPriority: true } => "Priority shipment"
        _ => "Standard shipment"
    }
}
```

Neither Raven version is inherently better. Use conditions when the branching
process is clearest; use property patterns when the shapes being recognized are
the important part of the problem.

## Classes are still the Raven way when the domain has objects

Not every C# class needs to become a collection of functions. A stateful
connection, aggregate with identity, actor, cache, UI component, or resource
owner may still be most honestly represented by a class:

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

1. Is this just an operation? Consider a function.
2. Is this one required capability? Consider a function parameter.
3. Is this immutable data or a domain value? Consider a record.
4. Is this a closed set of meaningful alternatives? Consider a union.
5. Is absence or failure expected? Consider `Option` or `Result`.
6. Does this concept have identity, state, lifecycle, or encapsulation? Consider
   a class.
7. Must unrelated implementations participate in an open contract? Consider an
   interface.

This is the main adjustment when moving from C#: classes remain available, but
they stop being the mandatory starting point.

For more examples, continue with [Domain modeling in
Raven](lang/domain-modeling.md) and the [language
introduction](introduction.md).
