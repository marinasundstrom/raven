# The meaning of Raven features

Raven features are modeling tools, not merely alternative spellings for C#
constructs. Read Raven code by asking what distinction a construct communicates
about the program. Write Raven code by identifying that distinction before
choosing syntax.

This guide is intentionally compact enough to use as context for people and
coding agents. It describes the semantic intent of common Raven features, the
choice each feature makes visible, and the boundary where an ordinary .NET
shape may still be more appropriate.

The central rule is:

> Model the meaning first. Select the Raven construct that represents that
> meaning second.

For larger examples and established pattern names, continue with [Domain
modeling in Raven](domain-modeling.md). The [Raven for C#
developers](../raven-for-csharp-developers.md) guide compares common starting
shapes.

## Start with the distinction the program needs

Before designing a type or function, identify which of these questions the
program must answer:

| Question | Meaning to represent | Good Raven starting point |
| --- | --- | --- |
| Is this one value, or may it be absent? | Meaningful presence or absence | `Option<T>` |
| Did an expected operation succeed or fail? | A typed success or failure channel | `Result<T, E>` |
| Can a value be one of several known alternatives? | A closed semantic domain | `union` |
| Are the alternatives only names without data? | A closed set of constants | `enum` |
| Does data have structural value meaning? | Equality by represented value | `record struct` or `record class` |
| Does something have identity, state, or a lifecycle? | An object that persists through change | `class` |
| Is this behavior just a transformation? | An operation without an owning object | Plain function |
| Is a dependency one operation? | A single required capability | Function parameter |
| Must outside code add implementations? | An open behavioral contract | `interface` |
| Must every known alternative be handled? | A complete interpretation | Exhaustive `match` |

These are starting points, not mechanical rules. A real application normally
uses several shapes together.

## Bindings communicate whether a name changes

`let` means that a lexical binding will not be reassigned. `var` means that
reassignment is part of the algorithm.

```raven
let requestId = Guid.NewGuid()
var attempts = 0
attempts = attempts + 1
```

This is a statement about the **binding**, not deep immutability. A `let`
binding may refer to a mutable object, and that object may still change.

Use `let` by default because a stable name gives readers and tools a stronger
local fact. Use `var` when changing the binding itself expresses the intended
state transition.

Type inference has a related meaning: an omitted annotation says that the
initializer already determines the type without losing useful intent. Raven
remains statically typed; inference does not make a value dynamic.

## Records represent values

A record says that the represented data is the important identity of the
value. Records are good starting points for immutable data, messages, DTOs, and
domain value objects.

```raven
record struct Money(Amount: decimal, Currency: Currency)

record class OrderSummary(
    val Id: Guid,
    val Total: Money)
```

Choose a record struct when value-type storage and semantics fit. Choose a
record class when reference storage fits while structural value behavior
remains meaningful.

A record wrapper can also give a primitive a domain identity:

```raven
record struct CustomerId private (Value: Guid) {
    static func From(value: Guid) -> CustomerId => CustomerId(value)
}
```

`CustomerId` means more than `Guid`, even though it uses a `Guid` as its .NET
representation. It prevents unrelated identifiers from being exchanged merely
because their storage types happen to match.

Do not use a record for an object whose identity must remain stable while its
state changes independently of structural equality. That meaning belongs to a
class.

## Classes represent identity, ownership, and lifecycle

A class says that an object boundary matters. It may own mutable state, a
resource, an invariant, or an identity that persists as its data changes.

```raven
class Customer(
    val Id: CustomerId,
    var DisplayName: string) {

    func Rename(displayName: string) {
        DisplayName = displayName
    }
}
```

Raven does not treat classes as an interoperability fallback. They remain the
right model for entities, stateful services, connections, caches, UI
components, and resource owners.

What Raven removes is the requirement to place every operation inside a class.
Create an object when the problem contains an object boundary, not merely to
provide a home for a function.

## Unions represent closed alternatives

A union says that a value belongs to one of a known set of alternatives. Each
case may carry exactly the data that is valid for that alternative.

```raven
union DeliveryStatus {
    case Pending
    case Delivered(at: DateTimeOffset)
    case Failed(reason: string)
}
```

This is not just a shorter enum or class hierarchy. It encodes a closed
semantic domain:

- `Pending` has no delivery time or failure reason.
- `Delivered` must have a delivery time.
- `Failed` must have a reason.
- no value can accidentally combine incompatible fields.

Use an enum when the cases are only named constants. Use an interface or open
class hierarchy when external code must add new alternatives. Use a sealed
hierarchy when the alternatives form a closed family but benefit from separate
.NET type identities and implementations.

Target-typed case construction such as `.Delivered(now)` does not change that
meaning. The leading dot says that the surrounding expected type supplies the
union whose case is being constructed:

```raven
let status: DeliveryStatus = .Delivered(DateTimeOffset.UtcNow)
```

It is contextual construction, not a globally scoped case name.

## `match` represents a complete interpretation

A `match` gives meaning to the alternatives of a value. With a closed domain,
exhaustiveness checking connects the decision directly to the model.

```raven
func Describe(status: DeliveryStatus) -> string {
    return status match {
        .Pending => "Pending"
        .Delivered(let at) => "Delivered at $at"
        .Failed(let reason) => "Failed: $reason"
    }
}
```

This is more than concise branching. It states that `Describe` interprets the
whole `DeliveryStatus` domain. If another status is added, the compiler can
identify decisions that have not assigned it meaning.

Patterns also establish facts. A successful pattern may prove a case, extract
its payload, narrow a type, or prove a value non-null. Prefer a pattern when
subsequent code depends on one of those facts rather than repeating casts,
property checks, or null suppression.

## `Option<T>` represents meaningful absence

`Option<T>` says that producing no value is an ordinary, expected result:

```raven
func FindCustomer(id: CustomerId) -> Option<Customer> {
    // A customer may legitimately be absent.
    return None
}
```

`Some(customer)` and `None` make presence and absence explicit alternatives.
The caller must decide what absence means in its context.

Use `T?` when nullability is part of a .NET API, storage representation, or
object model. Use `Option<T>` in domain-facing APIs when absence is a meaningful
outcome that callers should handle explicitly. Raven supports conversions at
appropriate nullable interoperability boundaries; the two forms should not be
treated as conceptually identical.

`unit` has a different meaning again: it represents no meaningful return value,
not an absent value.

## `Result<T, E>` represents expected failure

`Result<T, E>` says that failure is an expected outcome of a correctly used
operation and that callers may need to make a decision based on it.

```raven
union RegistrationError {
    case DuplicateEmail(email: string)
    case InvalidName
}

func Register(request: RegistrationRequest)
    -> Result<Customer, RegistrationError> {
    // ...
    return Error(.InvalidName)
}
```

Use an error union when different failures carry different meaning. An
exception remains appropriate for unexpected faults, broken invariants, and
.NET APIs whose contract is exceptional.

The `?` propagation operator means: if this result is successful, continue with
its value; if it contains the compatible failure channel, return that failure
from the current function.

```raven
func CreateCustomer(request: RegistrationRequest)
    -> Result<Customer, RegistrationError> {
    let validated = Validate(request)?
    return Save(validated)
}
```

The operator keeps the success path linear, but it does not erase the failure.
The function signature continues to expose it. Use an explicit `match` when
recovery, logging, translation, or branching gives the failure new meaning.

## Pattern declarations establish invariants

A pattern declaration with `else` means that the remainder of the current
scope may rely on the pattern having succeeded. The `else` branch must leave
that flow.

In an ASP.NET Core endpoint, a nullable database result can become a proven
non-null local:

```raven
let vehicle: VehicleEntity =
    await context.Vehicles.SingleOrDefaultAsync(
        candidate => candidate.Id == id,
        cancellationToken) else {
    return Results.NotFound()
}

return Results.Ok(MapVehicle(vehicle))
```

The important meaning is not terseness. The declaration establishes the
invariant “from here onward, `vehicle` exists,” while the failure path remains
at the boundary where it is understood.

## Expressions let decisions produce values

Raven control flow is expression-oriented. An `if` or `match` can produce the
value assigned or returned by its surrounding context:

```raven
let label =
    if total == 0 {
        "empty"
    } else {
        "ready"
    }
```

This means the branches are alternative ways to compute one value. Prefer that
shape when the branches answer one question. Prefer statement-oriented control
flow when the branches primarily perform effects or require early exits.

## Functions represent operations without artificial ownership

A plain function says that an operation does not need object identity or owned
state:

```raven
func CalculateTotal(lines: OrderLine[]) -> Money {
    // ...
}
```

A function parameter says that a caller must provide one capability:

```raven
async func LoadSummary(
    load: (Guid) -> Task<Option<Order>>,
    id: Guid) -> Task<Option<OrderSummary>> {
    // ...
}
```

Use a method when the behavior belongs to a type's vocabulary or requires its
encapsulated state. Use an interface when the dependency is an open protocol
with several related operations. Function-based dependency injection should
remove accidental ceremony, not conceal a meaningful service or lifecycle.

## Extensions adapt vocabulary

An extension makes an operation readable as part of an existing type's
vocabulary without changing that type:

```raven
extension HttpClientOperations for HttpClient {
    async func Fetch(url: string) -> Task<Result<string, FetchError>> {
        // ...
    }
}
```

The meaning is adaptation and composition. An extension does not reopen the
original type, grant access to its private state, or create new stored state.
Use it for coherent operations that read naturally from the receiver. Keep a
plain function when receiver syntax would imply ownership that the operation
does not have.

## `async` represents asynchronous execution, not failure

`Task<T>` and `async` describe when and how an operation completes. `Option<T>`
and `Result<T, E>` describe what its completion means.

For example:

```raven
async func FindCustomer(id: CustomerId)
    -> Task<Option<Customer>> {
    // Asynchronous operation with ordinary absence.
}

async func Register(request: RegistrationRequest)
    -> Task<Result<Customer, RegistrationError>> {
    // Asynchronous operation with expected failure.
}
```

Do not use `null` or exceptions merely because an operation is asynchronous.
Execution and outcome are separate modeling dimensions.

Likewise, `use` communicates ownership of a disposable lifetime. In an async
context it prefers asynchronous disposal when available. It means that the
current scope is responsible for ending the resource's lifetime.

## Preserve ordinary .NET boundaries

Raven code should remain native to the .NET ecosystem. Do not replace a
framework's public vocabulary solely to make every boundary look
Raven-specific.

A useful architecture is:

1. Accept the shapes expected by ASP.NET Core, Entity Framework Core, and other
   .NET libraries.
2. Translate boundary representations into honest domain types.
3. Use Raven records, unions, options, results, patterns, and functions inside
   the domain and application workflow.
4. Exhaustively translate domain outcomes back to framework results.

For example, an ASP.NET endpoint may accept a mutable request model required by
model binding, create validated record values, call a function returning a
domain `Result`, and translate its error union into `Results.BadRequest`,
`Results.NotFound`, or `Results.Conflict`.

An EF entity may remain a conventional mutable class while its domain status is
a Raven union. Persistence then uses an explicit conversion when the provider
cannot store that union directly. This preserves both the framework contract
and the domain meaning.

## Decision rules for coding agents

When generating or changing Raven code:

1. Describe the domain distinction before selecting a construct.
2. Identify the .NET or storage boundaries before changing their types.
3. Use `Option` for meaningful absence and `Result` for expected failure; do
   not treat them as stylistic replacements for every nullable value or
   exception.
4. Use a union only when the alternative set is intentionally closed.
5. Use exhaustive `match` to interpret closed alternatives. Avoid a catch-all
   when named cases should force future code changes.
6. Prefer records for values and classes for identity, lifecycle, or
   encapsulated mutation.
7. Prefer plain functions for ownerless operations and function parameters for
   single capabilities.
8. Keep framework-facing signatures recognizable to .NET unless a documented
   Raven projection or adapter applies.
9. Do not infer Raven support from similar C#, F#, Rust, or Swift syntax.
   Consult the language reference and compile the code.
10. Treat compiler diagnostics as semantic feedback. Fix the modeled mismatch
    instead of hiding it with casts, null suppression, or a broad catch-all.

The desired result is not code that uses the largest number of Raven features.
It is code whose types and control flow make the problem's meaning visible.

## Working examples

The following repository samples demonstrate these meanings in ordinary .NET
workloads:

- [ASP.NET Core Minimal
  API](https://github.com/marinasundstrom/raven/tree/main/samples/projects/aspnet-minimal-api)
  uses unions as request and response shapes with OpenAPI generation.
- [EF Core vehicle
  costs](https://github.com/marinasundstrom/raven/tree/main/samples/projects/efcore-vehicle-costs)
  combines ASP.NET Core, EF Core entities, domain value records, union state,
  pattern declarations, and explicit persistence adapters.
- [Raven Core](../compiler/raven-core-library.md) documents the precise
  `Option<T>` and `Result<T, E>` contracts and framework API projections.
- [Language reference](spec/language-specification.md) is authoritative for
  supported syntax and behavior.
