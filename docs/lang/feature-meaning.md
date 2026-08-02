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

## Expression-oriented does not mean expression-only

Raven makes expressions the usual way to compose computations. Blocks and
control flow such as `if` and `match` can produce values, so a decision can
appear directly in an initializer, argument, or return value.

The language still distinguishes expressions from statements in both syntax
and semantics. Similar-looking `if` or `match` constructs may be parsed as
different expression and statement nodes depending on their position. Loops
are statements in the current language. A lexical binding is also necessarily
a statement:

```raven
let x = 2 + a
let description = if count == 0 { "empty" } else { "populated" }
```

In the first line, `2 + a` is an expression, but `let x = 2 + a` is a
declaration statement with no equivalent binding-expression form. In the
second line, the whole line is likewise a lexical-binding statement; its
initializer is an `if` expression, and each branch produces the value bound to
`description`.

Expression-oriented programming reduces the need for temporary mutation and
statement-only control flow; it does not erase declarations, effects, jumps,
or other inherently statement-shaped operations.

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

### Nullability is an interoperability corridor

Raven does not expect nullable flow to organize an application. At a .NET
boundary, accept the platform signature faithfully, then eliminate the nullable
state as close to that boundary as practical:

```raven
func FindRequiredCustomer(id: CustomerId) -> Result<Customer, LookupError> {
    let customer = DotNetStore.Find(id)
    match customer {
        null => .Err(.MissingCustomer(id))
        let value => .Ok(value)
    }
}
```

After that conversion, domain code should normally use exhaustive patterns over
`Option`, `Result`, unions, or other explicit states. This keeps absence and
failure visible in the model instead of carrying a nullable reference through
unrelated code.

Null-flow analysis complements that style. It must remain sound for code that
cannot immediately leave the nullable corridor: platform callbacks, mutable
object models, generated APIs, reflection, and gradual migrations. Its job is
to publish the state established by patterns, branches, assignments, and .NET
flow attributes through diagnostics and `TypeInfo`; it is not a reason to
prefer null over Raven's explicit alternatives.

Raven treats flow and nullability as five separate responsibilities:

1. **Reachability analysis is core control-flow semantics.** It determines
   whether a block, branch, arm, or statement can execute and which paths can
   complete normally. Other analyses may consume that graph, but they should
   not independently invent reachability rules.
2. **Assignment analysis is core variable semantics.** It determines whether a
   variable is definitely assigned before use and whether an immutable binding
   is assigned again. Disabling null-flow analysis must not affect either rule.
3. **Explicit nullness, dereference checks, and .NET metadata are core type
   semantics.** At every source
   position where null is intentionally admitted—including locals and
   parameters—the semantic type for `T?` remains a nullable wrapper around `T`.
   The compiler preserves that unified representation, validates conversions
   and constraints, rejects unsafe dereferences known directly from the
   declared state, and imports and emits the corresponding platform annotations.
4. **Branch and pattern refinement is core semantics.** A successful non-null
   test or pattern establishes a safe fact within that arm or branch. This is
   required for Raven's preferred exhaustive pattern style; it is not an
   optional warning pass.
5. **Null-flow analysis is tooling policy.** Following a
   nullable value through distant assignments, loops, exceptions, and metadata
   postconditions is most useful for .NET interop and gradual migration. The
   analysis is enabled by default, but can be disabled without weakening the
   language's declared-nullness rules or pattern semantics.

For an MSBuild Raven project, disable null-flow analysis with:

```xml
<PropertyGroup>
  <EnableNullFlowAnalysis>false</EnableNullFlowAnalysis>
</PropertyGroup>
```

Hosts that construct compilations directly use
`CompilationOptions.WithEnableNullFlowAnalysis(false)`. This currently
suppresses the flow-derived possible-null-reference diagnostic (`RAV0402`). It
does not change `T` versus `T?`, nullable conversions, imported or emitted .NET
metadata, syntax-directed pattern refinement, or the nullability information
published by `TypeInfo`.

The purpose of null-flow analysis is defect discovery. It should identify
likely null-reference bugs in existing .NET-shaped code and make nullable state
visible while a codebase is being migrated. The recommended response is not to
build more domain logic around null, but to progressively shorten the nullable
corridor: handle a boundary value with a pattern and convert meaningful absence
or failure into an explicit Raven type.

A practical adoption sequence is:

1. preserve and enforce the existing .NET nullable annotations;
2. enable flow diagnostics to find unsafe dereferences and mutation paths;
3. introduce local patterns that handle every nullable outcome;
4. project recurring domain outcomes into `Option`, `Result`, or a union;
5. leave null-flow tracking concentrated near the remaining interop edges.

Turning off the fifth layer must not change reachability, assignment checks,
declared types, pattern-arm types,
overload resolution, emitted metadata, or runtime behavior.

`unit` has a different meaning again: it represents no meaningful return value,
not an absent value.

At a callable boundary, a `unit`-returning function still discards a
non-`unit` tail value. The optional `RAV9034` analyzer diagnostic warns by
default when that value looks like a result. Assign the expression to `_` when
discarding it is deliberate:

```raven
func Refresh(cache: Cache) {
    _ = cache.TryRefresh()
}
```

This makes `unit` an explicit statement about value flow rather than only
another spelling for CLR `void`.

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

An `if let` expression combines that meaning with a successful pattern:

```raven
let option: Option<int> = Some(42)
let value = if let Some(x) = option {
    x
} else {
    0
}
```

This says that both outcomes compute the same conceptual value, while `x`
exists only in the outcome where the pattern proved that the option contains
one. Use it for a local two-outcome decision. Use `match` when several cases
each carry distinct meaning or should be checked exhaustively.

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

## Choose metaprogramming by the structure you need

Metaprogramming mechanisms are not interchangeable. Choose one by identifying
which representation must be available and when it must be available:

| Mechanism | Structure it exposes | Typical phase | Choose it when |
| --- | --- | --- | --- |
| `Raven.CodeAnalysis` | Raven syntax and compiler semantics | Build time or tooling time | A tool must analyze, diagnose, navigate, or transform Raven code |
| .NET expression trees | Standardized, typed `System.Linq.Expressions` operations | Language conversion into a runtime API value | A library must inspect, rewrite, translate, or compile a supplied operation |
| .NET reflection | Emitted types, members, attributes, and metadata | Runtime | Code must discover or invoke compiled program structure |
| Raven macros | Authored syntax transformed through compiler expansion | Compile time | A reusable language abstraction must generate or reshape the program itself |
| Raven `compile!` | Raven expression syntax compiled to a delegate | Macro expansion at compile time; syntax construction and compilation at runtime | A program must generate or alter Raven syntax and then execute it as a typed delegate |

Raven's macro system is currently a work in progress. Use it for experimentation
with compile-time transformations, but expect the model and APIs to evolve. See
[Metaprogramming in Raven](../metaprogramming.md) for its current status.

Prefer the least powerful representation that directly matches the use case.
An ordinary generic, function, delegate, interface, or attribute is clearer
when no code or program structure needs to be inspected or transformed.

### Expression trees represent typed operations as data

An ordinary API call passes values and asks the callee to perform behavior.
Reflection inspects compiled types, members, and metadata. A .NET expression
tree occupies a useful middle position: it is an API value containing a typed
graph of operations that another component can inspect, rewrite, translate, or
compile.

```raven
let onlyActiveAdults: Expression<Func<User, bool>> =
    user => user.IsActive && user.Age >= minimumAge
```

The target type asks the Raven compiler to convert the lambda's supported
operations into an `Expression<Func<User, bool>>` object instead of emitting
only an opaque delegate. An API such as `IQueryable` can inspect that operation
graph and translate the predicate into another language, such as SQL. The tree
can also be compiled into a `Func<User, bool>` delegate and executed by the
running program. This is why an expression-tree parameter means something
different from a delegate parameter, even though both describe a predicate
with the same input and result types.

Expression trees are also distinct from Raven macros. A macro transforms
authored syntax during compilation and emits ordinary program syntax or bound
behavior. An expression tree converts supported operations into a structured
runtime value that the receiving API controls. Reflection starts from emitted
program structure; an expression tree starts from an operation deliberately
converted by the language compiler.

An expression tree is not a Raven syntax tree and does not preserve syntax.
`Raven.CodeAnalysis` represents Raven's own authored and semantic structure for
compiler tooling: syntax nodes, symbols, types, diagnostics, and operations. An
expression tree instead uses the standardized `System.Linq.Expressions`
concepts understood by .NET libraries. It contains no Raven syntax nodes,
tokens, trivia, or original source form.

That standardized operation vocabulary is intentionally narrower than Raven's
language semantics, and narrower than the complete semantics of any other .NET
language. It captures a shared set of common programming operations together
with .NET-specific concepts. A compiler can convert only language constructs
that have a valid representation in that object model. Use the code-analysis
APIs to understand Raven code; use expression trees to exchange supported
abstract operations with a .NET API.

Use an ordinary function or delegate when the receiver only needs to execute
behavior. Use an expression tree when the receiver must understand or
translate the behavior. Use reflection when the problem is discovering or
operating on compiled program structure. Use a macro when the program itself
must be transformed during compilation.

Reflection does not by itself make an untrusted assembly safe. When inspection
is the goal, prefer metadata-only facilities such as `MetadataLoadContext` or
`System.Reflection.Metadata`, which do not load the assembly for execution.
Loading an assembly into the application's execution context, invoking a
reflected member, or instantiating attributes crosses into executing its code
inside the current process. Treat that boundary with the same care as runtime
source compilation, and isolate or reject untrusted inputs according to the
application's threat model.

Raven's expression-tree support is currently incremental rather than complete.
Consult the repository's [expression-tree support
status](https://github.com/marinasundstrom/raven/blob/main/docs/compiler/development/expression-trees.md)
before relying on a particular body shape or operator.

### Expression trees and `#quote` quote different representations

A target-typed expression-tree lambda is a form of compiler-integrated
quotation. The programmer writes an ordinary lambda, and the compiler produces
an object graph of its supported operations:

```raven
let predicate: Expression<Func<User, bool>> =
    user => user.IsActive
```

Raven's `quote!` intrinsic applies the same broad idea to Raven syntax. The
programmer writes a Raven fragment, and the compiler produces the
`ExpressionSyntax` construction needed to recreate that fragment:

```raven
let syntax: ExpressionSyntax = quote! {
    left + right
}
```

Both mechanisms avoid starting with source text, invoking a parser, or manually
assembling the entire initial object graph. Both produce ordinary objects that
code can traverse and rewrite. Because the object models are immutable,
“modifying” a quoted tree means constructing a new tree from the original,
through visitors, replacement APIs, factories, or quote holes.

The representations and destinations are different:

| Quotation | Result | Retains | Primary destination |
| --- | --- | --- | --- |
| Expression-tree conversion | `Expression<TDelegate>` operation graph | Supported standardized .NET operations and types | Runtime libraries that translate, inspect, or compile operations |
| Raven `#quote` | Raven `ExpressionSyntax` tree | Raven syntax, tokens, and trivia | Macros and tools that generate or transform Raven program structure |

`#quote` is therefore more expressive for Raven program generation than
expression trees: it is not limited to the shared
`System.Linq.Expressions` operation vocabulary. That does not make expression
trees obsolete. A .NET API expecting `Expression<TDelegate>` needs precisely
that portable operation representation, while a Raven macro normally needs
Raven syntax.

The authored quote surface can support only the syntax categories and splice
forms implemented by the compiler. Once a syntax object exists, however, macro
code can inspect it and construct a transformed tree using the full compatible
`Raven.CodeAnalysis` syntax API. Syntax quotation does not bind names or types;
the generated syntax acquires semantic meaning only after insertion into a
compilation and normal binding.

### `compile!` reconnects Raven syntax to executable behavior

`quote!` deliberately stops at Raven syntax. `compile<TDelegate>!` uses the
same quotation and hole model, then asks the Raven runtime compiler to bind and
emit that syntax as a strongly typed delegate:

```raven
let increment = compile<System.Func<int, int>>! {
    value => #(SyntaxFactory.IdentifierName("value")) + 1
}
```

This is useful when Raven syntax itself is the representation that must be
generated or transformed. Unlike an expression tree, the intermediate value
can retain Raven tokens and trivia and can represent Raven syntax outside
`System.Linq.Expressions`' standardized operation vocabulary. Unlike a normal
compile-time macro expansion, the final binding and emission occur while the
program runs.

The stages provide different assurances. Quotation parses the authored macro
body immediately, so malformed or incomplete Raven structure is rejected
during the containing compilation. `compile!` then takes the final syntax,
including runtime hole values, through Raven parsing, binding, and emission.
It returns the requested delegate only when that code is semantically
consistent; otherwise it throws a `RavenCompilationException` containing the
compiler diagnostics.

That extra power has a cost. Runtime compilation produces and loads an
assembly, so compile once and reuse the returned delegate. It is also a code
execution boundary: do not compile syntax influenced by untrusted input
without an application-specific security boundary. Prefer an ordinary
delegate when only execution matters, and prefer an expression tree when a
.NET API expects its portable operation model.

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
9. Before choosing metaprogramming, identify the required structure and phase:
   Raven source and semantics for CodeAnalysis, standardized operations for
   expression trees, emitted metadata for reflection, or compile-time
   transformation for macros.
10. Do not infer Raven support from similar C#, F#, Rust, or Swift syntax.
   Consult the language reference and compile the code.
11. Treat compiler diagnostics as semantic feedback. Fix the modeled mismatch
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
- [EF Core expression
  trees](https://github.com/marinasundstrom/raven/tree/main/samples/projects/efcore-expression-trees)
  demonstrates a typed, inspectable predicate passed through an ordinary
  `IQueryable` API.
- [Raven Core](../compiler/raven-core-library.md) documents the precise
  `Option<T>` and `Result<T, E>` contracts and framework API projections.
- [Language reference](spec/language-specification.md) is authoritative for
  supported syntax and behavior.
