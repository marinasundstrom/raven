# Proposal: Framework API Projections

> ℹ️ This proposal is under consideration

## Summary

Allow Raven to present a curated set of well-known .NET framework methods as
Raven-native APIs that return `Option<T>` or `Result<T, E>` while preserving
access to the underlying CLR methods.

The initial feature projects selected `TryParse` methods on known framework
types:

```raven
val port = int.TryParse(text)
// Option<int>

match port {
    Some(val value) => Console.WriteLine($"Port: {value}")
    None => Console.WriteLine("Not a valid port")
}
```

Projects that need the ordinary .NET surface disable projections:

```xml
<PropertyGroup>
  <RavenFrameworkProjections>None</RavenFrameworkProjections>
</PropertyGroup>
```

Framework projections are selected by exact framework type and member
signatures. The MVP does not infer projections from a method's name or general
shape. A future extension model may allow trusted projection descriptors to be
generated or supplied for additional APIs.

## Motivation

.NET exposes several recurring API patterns that predate or cannot depend on
Raven's `Option<T>` and `Result<T, E>` types. For example, parsing commonly uses
a Boolean return value plus an `out` parameter:

```csharp
bool Int32.TryParse(string? text, out int value)
```

The pattern is efficient and interoperable, but it exposes two logically
related results as separate values. Raven already has a standard vocabulary for
this contract:

```raven
func TryParse(text: string?) -> Option<int>
```

A framework projection lets Raven code use that vocabulary without replacing
the CLR API, requiring wrappers in every project, or pretending that arbitrary
methods with similar names necessarily have the same semantics.

The feature should provide:

* a consistent Raven-facing API for common framework operations;
* predictable and inspectable lowering to existing CLR calls;
* no metadata or runtime changes to the projected framework types;
* an explicit project-level opt-out that restores the original .NET surface;
* one semantic model shared by compilation and language services; and
* a controlled path toward projections supplied outside the compiler.

## Goals

* Project an explicit allowlist of known framework members.
* Use `Option<T>` when failure means only that no value was produced.
* Leave room for `Result<T, E>` when failure information is stable and useful.
* Preserve the source and binary behavior of the underlying framework method.
* Preserve ordinary CLR overloads and interop when projections are disabled.
* Make projected members visible in lookup, overload resolution, semantic APIs,
  completion, hover, and signature help.
* Keep the projection catalog declarative enough to support generated catalogs
  in the future.
* Allow projects to disable framework projections.

## Non-goals

The initial feature does not:

* project every method named `TryParse`, `TryGet`, `Find`, or `Parse`;
* infer semantics from `bool` plus an `out` parameter alone;
* modify framework assemblies or emit new members onto framework types;
* make `Option<T>` or `Result<T, E>` part of the CLR framework contract;
* add source-generator support to Raven;
* allow arbitrary referenced assemblies to inject compiler behavior;
* project exception-throwing `Parse` methods in the MVP; or
* guarantee that every overload of a projected method has a Raven projection.

## Terminology

A **source method** is the real method imported from framework metadata.

A **projected method** is the Raven-facing callable symbol synthesized from a
projection descriptor. It has its own Raven signature and records its source
method and lowering strategy.

A **projection catalog** is a set of exact source-method identities and their
Raven-facing signatures and lowering strategies.

The word *projection* in this proposal describes an API view. It is unrelated
to the projection of generic union case types onto a constructed union carrier.

## Proposed project option

Framework projections are a compilation option because they affect member
lookup, overload resolution, binding, and semantic-model results. They are not
a parse option and do not change Raven syntax.

Projects configure the feature with an MSBuild property:

```xml
<PropertyGroup>
  <RavenFrameworkProjections>Standard</RavenFrameworkProjections>
</PropertyGroup>
```

The initial modes are:

| Mode | Meaning |
| --- | --- |
| `None` | Expose only the ordinary imported .NET API. |
| `Standard` | Replace known framework member families with Raven's curated projections. This is the default. |

Raven source-file applications and projects use `Standard` unless they opt out.
The option selects an API view for the compilation; it is not an instruction to
mix projected and CLR signatures from the same known member family.

## MVP

### Supported transformation

The MVP supports this exact semantic transformation:

```text
static bool TryParse(string? input, out T value)
    =>
static Option<T> TryParse(string? input)
```

The source method must be identified by its containing metadata type, name,
parameter types, parameter ref-kinds, return type, and applicable target
framework. Merely matching the displayed shape is insufficient.

In the standard view, the projected family replaces the source family for Raven
member lookup:

```raven
val projected = int.TryParse(text) // Option<int>
```

With projections set to `None`, the projected member is absent and the ordinary
CLR overloads participate instead. The MVP does not add a per-call escape hatch.

### Initial type set

The first useful vertical slice should cover:

| Raven type | Framework type | Projected result |
| --- | --- | --- |
| `int` | `System.Int32` | `Option<int>` |
| `long` | `System.Int64` | `Option<long>` |
| `double` | `System.Double` | `Option<double>` |
| `decimal` | `System.Decimal` | `Option<decimal>` |
| `Guid` | `System.Guid` | `Option<Guid>` |
| `DateTime` | `System.DateTime` | `Option<DateTime>` |

The catalog contains only the simplest string-input overload for each type.
Overloads involving spans, format styles, format providers, or additional flags
are deferred until the basic symbol and lowering model is proven.

The remaining integral and floating-point primitives are natural additions
after the initial slice:

* `byte` and `sbyte`;
* `short` and `ushort`;
* `uint` and `ulong`;
* `nint` and `nuint`;
* `float`; and
* `Half`, when available on the target framework.

### Lowering

A projected call evaluates each supplied argument exactly once, calls the real
framework method, and constructs the corresponding option case:

```raven
// Conceptual lowering of T.TryParse(input)
var parsed: T
if T.TryParse(input, out parsed) {
    Some(parsed)
} else {
    None
}
```

The concrete bound and lowered representation may differ, but it must preserve:

* argument evaluation order;
* single evaluation of side-effecting expressions;
* the source method's overload and target-framework identity;
* correct construction of `Option<T>`; and
* diagnostics rather than exceptions if `Raven.Core` or the required option
  cases are unavailable.

The compiler emits an ordinary call to the source method plus ordinary
`Option<T>` construction. No projected member is emitted into the framework
type and no runtime reflection surface is invented.

### Symbols and semantic APIs

The compiler should represent a projected method as a method symbol with:

* its Raven-facing name, parameters, and return type;
* a reference to the exact source method;
* a projection descriptor or lowering kind; and
* stable original-definition behavior for constructed framework types.

The projected symbol appears to belong to the projected receiver type, not to
an adapter or extension container. For example, symbol display for the standard
integer projection describes `int.TryParse(string?) -> Option<int>` and its
containing type is `System.Int32`.

Framework projections are not extension methods and do not participate through
extension-method precedence. Ordinary lookup retains its normal rule that a
real receiver member wins over an extension member with the same effective
signature. The projection layer selects a compiler-defined API view before
ordinary member and extension lookup; generated Raven.Core methods, if used,
are implementation adapters recorded by the projected symbol rather than
user-visible extension candidates.

The normal public semantic APIs remain authoritative. `GetSymbolInfo`,
`GetTypeInfo`, operation creation, completion, hover, and signature help should
all observe the same projected method selected by the binder. Language-service
code must not recreate the projection catalog or bypass ordinary semantic
lookup.

The displayed signature should make the projected nature discoverable without
making ordinary code noisy. Tooling may append a short annotation such as
`framework projection` and provide navigation to the source framework method
or to Raven documentation for the projection.

### Lookup and overload resolution

Projected methods participate in static member lookup only when their catalog
entry is enabled and its exact source method is present in the compilation's
references.

When a catalog entry is active, its projected members are the visible overload
family. The corresponding source family does not participate in ordinary Raven
lookup. Disabling projections restores the source members.

This replacement model permits later `Parse -> Result` projections with the
same argument list as the source method. Each mapping must still identify the
exact source signature and define how users recover the CLR view through the
project option.

In particular, a `Parse` projection must not be implemented by allowing a
Raven.Core extension method to shadow `System.Int32.Parse` or another real
receiver member. The compiler exposes a projected `Parse` symbol in `Standard`
mode and lowers it to its mapped adapter or recipe. In `None` mode, normal
receiver lookup sees the CLR `Parse` methods and ordinary extension precedence
is unchanged.

### Availability and compatibility

Catalog entries should be resolved against metadata rather than assuming one
specific runtime implementation. A projected member is available only when its
source signature and Raven.Core dependencies are available for the target
framework.

The catalog is part of the Raven compiler and standard-library compatibility
surface. Changes to enabled entries can affect overload resolution and must be
versioned and documented like other language behavior.

## Common candidate cases

The following cases motivate the wider feature. They are candidates, not all
part of the MVP.

### Parsing to `Option<T>`

These APIs normally communicate only success or failure and discard the reason
for failure, which makes `Option<T>` a natural projection.

| API family | Possible Raven surface | Notes |
| --- | --- | --- |
| Numeric `TryParse` | `T.TryParse(text) -> Option<T>` | Primary MVP family. |
| `Guid.TryParse` | `Guid.TryParse(text) -> Option<Guid>` | Primary MVP member. |
| `DateTime.TryParse` | `DateTime.TryParse(text) -> Option<DateTime>` | Culture-dependent behavior remains that of the selected source overload. |
| `DateTimeOffset.TryParse` | `DateTimeOffset.TryParse(text) -> Option<DateTimeOffset>` | Straightforward follow-up. |
| `DateOnly.TryParse` | `DateOnly.TryParse(text) -> Option<DateOnly>` | Target-framework dependent. |
| `TimeOnly.TryParse` | `TimeOnly.TryParse(text) -> Option<TimeOnly>` | Target-framework dependent. |
| `TimeSpan.TryParse` | `TimeSpan.TryParse(text) -> Option<TimeSpan>` | Straightforward follow-up. |
| `Version.TryParse` | `Version.TryParse(text) -> Option<Version>` | Produces a reference type; the option still distinguishes failure explicitly. |
| `IPAddress.TryParse` | `IPAddress.TryParse(text) -> Option<IPAddress>` | Requires the networking assembly/reference. |

Style- and provider-aware overloads may later preserve their non-`out`
parameters while removing only the projected result parameter:

```raven
val value = decimal.TryParse(text, NumberStyles.Currency, culture)
// Option<decimal>
```

Each such overload still requires an explicit catalog entry. The compiler must
not assume all overloads in a projected family have equivalent semantics.

### Enum parsing

Enum parsing is useful but should be a separate implementation slice:

```raven
val color = Color.TryParse(text)
// Option<Color>
```

The underlying API is a generic static method on `System.Enum`, while the
desired Raven member appears on the concrete enum type. This requires projected
static-member lookup for enum symbols and generic construction of the source
method. It is more than another closed metadata-signature entry.

The case-sensitivity overload could become:

```raven
val color = Color.TryParse(text, ignoreCase: true)
// Option<Color>
```

The compiler must preserve the source method's enum constraints and should not
offer the projection for a non-enum type.

### Lookup to `Option<T>`

Some well-known lookup methods also have a value-or-absence contract:

| API family | Possible Raven surface | Design concern |
| --- | --- | --- |
| `Dictionary<TKey, TValue>.TryGetValue` | `dictionary.TryGetValue(key) -> Option<TValue>` | Must project an instance generic method and preserve comparer behavior. |
| `IReadOnlyDictionary<TKey, TValue>.TryGetValue` | `dictionary.TryGetValue(key) -> Option<TValue>` | Interface dispatch must remain visible in lowering and symbol identity. |
| `ConditionalWeakTable<TKey, TValue>.TryGetValue` | `table.TryGetValue(key) -> Option<TValue>` | Reference and lifetime semantics must remain unchanged. |
| `ConcurrentDictionary<TKey, TValue>.TryGetValue` | `dictionary.TryGetValue(key) -> Option<TValue>` | A read projection is reasonable; mutating `Try*` methods are not equivalent. |

These should not be generalized to all `TryGetValue` methods. Some methods
have multiple outputs, distinguish several states, mutate the receiver, or use
the Boolean result for more than absence.

Raven.Core already supplies option-returning LINQ helpers such as
`FirstOrNone`, `LastOrNone`, `SingleOrNone`, and `ElementAtOrNone`. Framework
projections should complement those helpers rather than introduce duplicate
names or competing lookup behavior.

### Creation and discovery APIs

Some framework APIs can be considered individually:

| API family | Possible projection | Design concern |
| --- | --- | --- |
| `Uri.TryCreate` | `Uri.TryCreate(...) -> Option<Uri>` | Has multiple overload families and creation modes. |
| `Activator.CreateInstance` | `Option<object>` or `Result<object, E>` | Failure is exception-rich; `Option` may discard too much information. |
| Reflection `Get*` methods returning `null` | `Option<MemberInfo>` | A nullable-to-option projection may be useful but changes many signatures. |
| Environment/configuration lookup | `Option<string>` | Often already represented by nullable returns rather than `Try*` methods. |

Nullable-return projections deserve a separate design because nullability
annotations, oblivious metadata, and `Option<T>` conversions already interact
with Raven's nullable semantics.

### Methods better represented by `Result<T, E>`

Some APIs provide or imply useful failure information. Collapsing that
information into `None` would make the API less expressive.

Potential families include:

* exception-throwing `Parse` methods;
* decoding and deserialization;
* file, URI, and resource loading;
* conversion APIs that distinguish format, overflow, unsupported type, or
  invalid state; and
* APIs following `bool TryX(..., out T value, out E error)`.

These require a stable error contract. Framework exception sets are not always
closed, consistently documented, or suitable as an exhaustively matched public
union. A Raven projection should not claim that an operation cannot throw merely
because expected failures are returned as `Result`.

## Why `Parse` is deferred

An exception-throwing source method already has the desired argument list:

```raven
int.Parse(text) // int in the CLR API
```

A projected method with the same name and arguments but a `Result` return type
cannot coexist under ordinary .NET-style overload resolution. Return type alone
does not distinguish overloads.

Several future designs are possible:

1. **Use a different name.** For example,
   `int.ParseResult(text) -> Result<int, ParseError>`.
2. **Replace the ordinary view while projections are enabled.** This requires
   an explicit and reliable way to call the raw CLR member.
3. **Add a member-access qualifier.** For example, distinct projected and CLR
   views, with exact syntax decided separately.
4. **Use contextual return-type resolution.** This is a significant departure
   from the normal overload model and could make untyped calls unstable.
5. **Expose only `TryParse -> Option` as the native parsing API.** `Parse`
   remains unchanged and throwing by design.

The error type also needs a decision. Direct exception unions such as
`FormatException | OverflowException` preserve framework details but couple
exhaustiveness to framework behavior. A Raven-owned `ParseError` union is more
stable but creates a standard-library compatibility commitment and requires a
mapping for each source method.

The MVP therefore leaves every `Parse` method unchanged.

## Cases that must not be projected by shape alone

The following common patterns demonstrate why convention matching is unsafe as
the initial behavior:

* `Monitor.TryEnter` returns whether a lock was acquired and has synchronization
  side effects; it is not a value lookup.
* `ConcurrentDictionary.TryAdd` and `TryUpdate` report whether a mutation took
  place; `Option<T>` would not express their contract naturally.
* `Queue<T>.TryDequeue` and `Stack<T>.TryPop` both retrieve and mutate. An
  option-returning view may be useful, but it requires an explicit semantic
  choice rather than a global rule.
* `WeakReference<T>.TryGetTarget` is affected by lifetime and garbage
  collection between calls.
* methods with several `out` parameters may have no single value that should be
  wrapped;
* a `false` result may mean invalid input, absence, contention, cancellation,
  failure to mutate, or a recoverable operational error; and
* some `Try*` methods can still throw for argument errors or environmental
  failures.

Names and parameter shapes are useful inputs to tooling, but they are not a
complete semantic contract.

## Declarative projection catalog

The compiler-owned catalog should be a checked-in mapping file rather than
hard-coded special cases spread through member lookup and lowering. The mapping
file is the reviewable source of truth for both compiler lookup and generated
Raven.Core adapters. A conceptual descriptor is:

```text
owner: System.Int32
source:
  name: TryParse
  return: System.Boolean
  parameters:
    - System.String
    - out System.Int32
projected:
  owner: source-owner
  name: TryParse
  parameters:
    - source-parameter[0]
  return: System.Option<System.Int32>
lowering:
  kind: bool-out-to-option
  valueParameter: 1
  exceptions: []
availability:
  framework: any-with-exact-source-signature
```

Descriptors need enough information to:

* resolve the exact source method without display-name ambiguity;
* construct projected generic types and methods;
* identify which input and output parameters participate;
* select a compiler-supported lowering strategy;
* declare required Raven.Core types;
* express target-framework availability; and
* provide documentation and tooling metadata.

The catalog must not embed arbitrary executable compiler hooks. Projection
lowering should select from a small set of compiler-defined, validated recipes.

### Exception mappings

Every descriptor declares its exception behavior, including descriptors whose
expected exception list is empty. For `TryParse -> Option`, `false` maps to
`None` and the mapping catches no exceptions. Exceptions that the source method
can still throw remain exceptions.

A future `Parse -> Result` descriptor must list each expected exception and its
Raven error case explicitly:

```text
source: System.Int32.Parse(System.String)
projected: System.Result<System.Int32, System.ParseIntError>
lowering:
  kind: catch-to-result
  exceptions:
    - type: System.ArgumentNullException
      case: Null
    - type: System.FormatException
      case: InvalidFormat
    - type: System.OverflowException
      case: Overflow
```

The list is not inferred from documentation, method names, or neighboring
overloads. Reviewers must verify it for the exact target-framework signature.
An unlisted exception is not swallowed or converted to a generic error. This
keeps `Result` honest about which failures it models without claiming that a
framework call cannot throw.

### Mapping artifact as an extension boundary

The built-in mapping file is also the intended future extension boundary. A
source generator, package tool, or project-local generator may eventually emit
the same versioned artifact for additional libraries. The compiler validates
the artifact against referenced metadata before exposing its projections.

Generated mappings should remain deterministic build inputs that both the
command-line compiler and language server can consume without executing the
generator during semantic lookup. Convention-based discovery may help produce
the file, but the resulting exact signatures, failure recipes, and exception
mappings remain explicit and reviewable.

## Future extension model

The long-term model can separate **discovery** from **semantics**:

1. conventions identify methods that might be projection candidates;
2. a generator or tool produces explicit projection descriptors;
3. the compiler validates each descriptor against referenced metadata; and
4. validated descriptors participate through the same symbol and lowering path
   as the built-in catalog.

This preserves the MVP's exactness while making conventions useful as an
authoring mechanism.

### Convention-assisted generation

A future generator could search an assembly for configured shapes such as:

```text
bool TryX(A..., out T value) => Option<T>
```

It should generate candidates for review or require library-authored metadata;
the compiler should not automatically accept every match. Configuration must
be able to exclude methods, select the value output, rename the projected
method, and choose a supported lowering recipe.

For example, a library might describe:

```text
projection MyParser.TryRead(string, out Item)
    as MyParser.TryRead(string) -> Option<Item>
    using bool-out-to-option
```

The exact source syntax or file format is outside this proposal.

### Possible providers

Projection descriptors could eventually come from:

* Raven source generators, once generator support exists;
* a build-time tool that emits a deterministic manifest;
* attributes authored by a library that controls both the source API and the
  projection contract;
* a package containing a projection catalog for another package; or
* explicit project configuration for local interop.

All providers should converge on the same validated descriptor model. They
must not require the language server to load and execute arbitrary build logic
while answering member lookup.

### Trust, scope, and versioning

Externally supplied projections can change overload resolution and the apparent
public surface of referenced types. The extension model therefore needs rules
for:

* explicit project opt-in and dependency scope;
* deterministic catalog identity and caching;
* conflicts between built-in, library-authored, package, and project entries;
* behavior when the source assembly version changes;
* diagnostics for stale or invalid descriptors;
* whether a projection may shadow a real member;
* security and build isolation for generated catalogs; and
* language-service availability before or without a full build.

Built-in projections should have a defined precedence. External projections
should not silently override built-in projections or real members.

### Additional lowering recipes

After `bool-out-to-option`, future validated recipes might include:

| Source contract | Projected contract | Notes |
| --- | --- | --- |
| `bool + out T` | `Option<T>` | MVP recipe. |
| nullable `T?` return | `Option<T>` | Requires nullability policy. |
| `bool + out T + out E` | `Result<T, E>` | Requires exact success/error mapping. |
| throwing `T` return | `Result<T, E>` | Requires explicit caught exception set and raw-call access. |
| sentinel return | `Option<T>` | Requires an explicit sentinel and comparison rule. |
| `Task<bool> + out T`-like pattern | asynchronous option/result | CLR cannot use `out` across `async`; real APIs use other carrier shapes. |

Recipes should preserve source exceptions that are outside the explicitly
projected failure contract. A descriptor must not turn unexpected exceptions
into `None` unless that behavior is explicitly part of the recipe.

## Alternatives considered

### Raven.Core extension methods only

Ordinary extension methods are attractive because they require less compiler
machinery and emit normal method calls. They cannot fully provide the proposed
surface when the desired API is a static member on a framework type, and they
can create naming and import differences between instance and static APIs.

Raven.Core helpers remain appropriate where their names communicate different
semantics, as with the existing LINQ `FirstOrNone` family. Framework projections
are for the smaller set where preserving the familiar framework owner and name
materially improves the Raven experience.

Even when Raven.Core contains generated adapter methods, those adapters are not
the projected API and must not be inserted through normal extension lookup.

### Automatically project every matching method

This is compact to implement but assigns semantics based on naming and shape.
The counterexamples above make it unsuitable as default language behavior.

### Rewrite calls after binding

Binding the CLR call first and rewriting it later would give semantic APIs and
language services the wrong return type and symbol. Projection must participate
in member lookup and binding so all compiler consumers observe the same call.

### Generate wrapper source into every project

Generated wrappers make lowering visible but add source, imports, names, and
incremental-build state to each project. A compiler-owned projected symbol can
still lower to an ordinary CLR call without creating per-project public API.
Future external providers may use generated descriptors without generating all
wrapper implementations as Raven source.

## Diagnostics

The feature should diagnose configuration or catalog failures that users can
act on, including:

* an unknown framework projection mode;
* a requested projection whose Raven.Core carrier type is unavailable;
* an externally supplied descriptor that does not match its source method;
* conflicting projected signatures; and
* a projection suppressed because a real member now occupies its signature.

Missing built-in projections caused only by targeting an older framework should
normally behave as unavailable members rather than producing global compilation
errors. A diagnostic is appropriate when source code attempts to call such a
member.

## Implementation outline

The MVP affects these compiler layers:

1. Add a framework-projection mode to `CompilationOptions` and project-system
   evaluation.
2. Define immutable projection descriptors and the built-in catalog in
   `Raven.CodeAnalysis`.
3. Resolve catalog entries lazily against the compilation's metadata references.
4. Add projected method symbols to static member lookup without mutating the
   imported framework type.
5. Include projected candidates in ordinary overload resolution.
6. Bind a projected invocation with its source method and lowering recipe.
7. Lower `bool-out-to-option` calls while preserving evaluation semantics.
8. Expose the projected symbol and invocation through semantic and operation
   APIs.
9. Present the same symbol through completion, hover, signature help, and
   navigation.
10. Document the enabled catalog and project option.

The projection cache belongs to the compilation/binder semantic layer and must
be invalidated with compilation options and references. Language services
should query normal compiler APIs rather than maintain a separate catalog or
cache policy.

## Test plan

Focused coverage should include:

* option return types and projected method symbols for every MVP entry;
* successful and unsuccessful observable parsing behavior;
* single evaluation and argument evaluation order;
* replacement of the original family in `Standard` mode;
* restoration of the original `out` overload in `None` mode;
* `None` when parsing fails without exposing an uninitialized union carrier;
* behavior when projections are disabled;
* behavior when Raven.Core is unavailable;
* target-framework availability and exact metadata-signature matching;
* semantic-model and operation shape;
* completion, hover, and signature-help presentation; and
* incremental compilation after the projection mode or references change.

Tests should assert diagnostics, symbols, operation shape, and observable
runtime behavior rather than emitted instruction sequences.

## Open questions

1. Should the option remain binary (`None`/`Standard`) or eventually allow named
   catalog sets?
2. What annotation should symbol display and documentation use for a projected
   method?
3. Should navigation prefer Raven projection documentation or the underlying
   framework method?
4. When a future framework adds a real member matching a projection, should the
   projection be silently suppressed, warned, or versioned by language version?
5. Should provider/style overloads be added individually to the built-in
   catalog or generated from reviewed family descriptors?
6. Should a future per-call syntax select the raw CLR member, or is the
   compilation-level `None` view sufficient?
7. Should throwing `Parse` methods project to `Result`, and if so, should
   their error channel use CLR exceptions or Raven-owned error unions?
8. Should enum projections appear directly on every enum type or use a
   Raven.Core helper until static projected lookup is mature?
9. How should external mapping artifacts be discovered, versioned, and scoped
   so command-line builds and language services consume identical descriptors?
10. Should exception mappings vary by target framework when framework contracts
    differ?

## Recommended staging

### Stage 1: closed `TryParse` MVP

Implement `bool-out-to-option` for the six initial framework types, behind the
project option, with full semantic-model and language-service support.

### Stage 2: catalog expansion

Add reviewed primitive, date/time, networking, and version parsing entries,
then provider/style overloads where their projected signatures remain clear.

### Stage 3: generic and instance projections

Evaluate enum parsing and selected dictionary-style lookups. Prove generic
substitution, interface dispatch, and constructed-type symbol identity.

### Stage 4: result projections

Choose raw-member access and stable error modeling before adding throwing
`Parse`, decoding, or other `Result<T, E>` projections.

### Stage 5: external catalogs

Define a validated, versioned descriptor artifact and allow explicitly selected
packages or build tools to contribute projections.

### Stage 6: convention-assisted authoring

Allow generators or tooling to discover candidates by convention and emit
explicit descriptors. Convention matching remains an authoring convenience,
not an unchecked compiler rule.
