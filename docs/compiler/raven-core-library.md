# Raven Core Library

`Raven.Core` is the standard library that ships with the compiler. It contains
foundational types implemented in Raven and compiled into `Raven.Core.dll` so
programs can rely on a consistent Option/Result vocabulary across projects,
plus a handful of generated shims the compiler produces automatically when it
builds the library.

## How Raven.Core is built

The `src/Raven.Core` project builds directly from the Raven sources in
`Option.rav` and `Result.rav`. During a solution build, MSBuild runs the
`CompileRavenCore` target, which:

- Invokes `Raven.Compiler` with `--emit-core-types-only` to compile the Raven
  sources into a class library for the current target framework.
- Emits the assembly to `src/Raven.Core/bin/<Configuration>/<TargetFramework>/Raven.Core.dll`.
- Exposes the DLL via the standard `BuiltProjectOutputGroup` so it is copied
  alongside the CLI and into compilation outputs.

Because this target runs before the rest of the build, `dotnet build Raven.sln`
produces `Raven.Core.dll` automatically and keeps it in sync with the compiler.
You can still override the reference with `rvn --raven-core <path>` when
compiling standalone projects.

## Framework API projections

Raven exposes a curated Raven-facing view of selected framework APIs by
default. The initial catalog projects `TryParse(string, out T)` overloads for
`int`, `long`, `double`, `decimal`, `Guid`, and `DateTime`, including the common
numeric style/provider and `DateTime` provider/style forms:

```raven
val number = int.TryParse(text) // Option<int>
val currency = decimal.TryParse(text, NumberStyles.Currency, culture)
// Option<decimal>
```

It also replaces `int.Parse(string)` and `Guid.Parse(string)` with
result-returning Raven signatures:

```raven
val number = int.Parse(text)
// Result<int, ArgumentNullException | FormatException | OverflowException>
val id = Guid.Parse(text)
// Result<Guid, ArgumentNullException | FormatException>
```

The catalog is an exact, versioned mapping rather than a naming convention.
Each entry records its exact source signature, projected signature, stable
projection ID, bridge implementation, and expected exception mapping. The
`TryParse` entries catch no exceptions: a `false` result becomes `None`, while
source exceptions remain exceptions. The `int.Parse` and `Guid.Parse` bridges
preserve their documented exception types directly in standard unions.

This exception-preserving surface is intended for predictable .NET interop. At
an application or domain boundary, map those framework exceptions into your
own error records or error unions so the domain model does not expose
infrastructure-specific failures throughout the program.

Set `RavenFrameworkProjections` to `None` in a project to restore the ordinary
.NET member families, including their `out` parameters. See the framework API
projections proposal for the planned mapping-file extension model.

## Types included in Raven.Core

### `Option<T>`

A union carrier with two cases:

- `Some(value: T)` holds a value.
- `None` represents the absence of a value.

`Option<T>` is a plain `union` that uses Raven's standard struct carrier model:

- runtime values are `Option<T>` carriers
- case values convert into the carrier
- the carrier exposes a conventional `Value: object?` property containing the active case value
- the carrier exposes `HasValue: bool` for initialization-safe inspection across interop boundaries
- extraction uses `TryGetValue(out Some<T>)` / `TryGetValue(out None)` and pattern matching

For the built-in `Option<T>` struct carrier, `HasValue` is `true` for
constructed values and `false` for the default carrier value. `Value` points at
the active case object (`Some<T>` or `None`) when initialized. The carrier
itself is the stable runtime type; `Value` is the authoritative active-case
projection.

Because `Option<T>` is a struct union, `default(Option<T>)` is an inactive
carrier state rather than `None`. Raven code that receives an `Option<T>` as a
parameter or `self` can assume an active carrier because the call boundary
rejects possibly inactive arguments before entry. Fields and properties remain
storage/interop boundaries until local flow proves them active. Match
exhaustiveness is still source-checked over the semantic cases (`Some` and
`None`); the inactive carrier is not a source case. Use a catch-all arm only when
the program intentionally handles a physically possible inactive carrier. Locals
initialized from `Some(...)` or `None` are known active and report redundant
catch-all arms.

`Option<T>` extension helpers:

- State checks: `HasSome`, `HasNone`
- Mapping and composition: `Map`, `Then`, `Where`, `Filter`, `OrElse`
- Interop with `Result`: `ThenResult`, `MapResult`, `IsOkOr(error)`,
  `IsOkOr(errorFactory)`
- Pattern/value helpers: `Match`, `Tap`, `TapNone`
- Unwrap helpers: `UnwrapOrElse`, `UnwrapOrDefault`, `UnwrapOrThrow`,
  `UnwrapOr(defaultValue)`
- Sequence helpers: `ToEnumerable`, `GetEnumerator`
- Nested helper (`Option<Option<T>>`): `Flatten`
- Nullable conversions:
  - `Option<T : class> <-> T?`
  - `Option<T : struct> <-> T?`

### `Result<T, E>`

`Result<T, E>` is also a plain `union` struct carrier:

- `Ok(value: T)` for success.
- `Error(data: E)` for failure.

Like `Option<T>`, `Result<T, E>` uses carrier semantics rather than inheritance semantics. Case values convert into the carrier, and extraction happens through `TryGetValue(out CaseType)` or pattern matching.
The carrier also exposes a conventional `Value: object?` property
containing the active case value.
The carrier also exposes `HasValue: bool`, where constructed values report
`true` and the default carrier value reports `false`.

For constructed `Result<T, E>` values, `Value` points at either the active
`Ok<T>` or `Error<E>` case object.

Because `Result<T, E>` is a struct union, `default(Result<T, E>)` is an inactive
carrier state rather than an `Error`. Raven code that calls a `Result<T, E>`
helper with a visible default-capable receiver is diagnosed at the call boundary.
If external code or unsafe construction still forces an inactive carrier into a
helper, the source match is exhaustive over `Ok` and `Error` and the emitted
defensive fallback handles the invalid representation state. Code that forwards
a `Result<T, E>` across a call or return boundary must pass or return a value
that flow analysis knows is active.

`Result<T, E>` extension helpers:

- State checks: `HasOk`, `HasError`
- Channel projection: `IsOk`, `IsError`
- Mapping and composition: `Map`, `Then`, `MapError`, `OrElse`
- Pattern/value helpers: `Match`, `Tap`, `TapError`
- Unwrap helpers: `UnwrapOrElse`, `UnwrapOrDefault`, `UnwrapOrThrow`,
  `UnwrapOr(defaultValue)`, `UnwrapError`
- Sequence helpers: `ToEnumerable`, `GetEnumerator`

### LINQ-style carrier extensions (`System.Linq`)

`IEnumerable<T>` gets Raven.Core helpers for carrier-friendly queries:

- Option-returning:
  - `FirstOrNone()`, `FirstOrNone(predicate)`
  - `LastOrNone()`, `LastOrNone(predicate)`
  - `SingleOrNone()`, `SingleOrNone(predicate)`
  - `ElementAtOrNone(index)`
- Result-returning (custom error):
  - `FirstOrError(errorFactory)`, `FirstOrError(predicate, errorFactory)`
  - `LastOrError(errorFactory)`, `LastOrError(predicate, errorFactory)`
  - `SingleOrError(errorFactory)`, `SingleOrError(predicate, errorFactory)`
  - `SingleOrError(errorIfNone, errorIfMany)`
  - `SingleOrError(predicate, errorIfNone, errorIfMany)`
  - `ElementAtOrError(index, errorFactory)`
- Result-returning (captured exception):
  - `ToArrayOrException() -> Result<T[], Exception>`
  - `ToListOrException() -> Result<List<T>, Exception>`
  - `ToHashSetOrException() -> Result<HashSet<T>, Exception>`
  - `ToDictionaryOrException(keySelector) -> Result<Dictionary<TKey, T>, Exception>`
  - `ToDictionaryOrException(keySelector, elementSelector) -> Result<Dictionary<TKey, TValue>, Exception>`
- Result-returning (mapped error):
  - `ToArrayOrError(errorFactory: Exception -> E) -> Result<T[], E>`
  - `ToListOrError(errorFactory: Exception -> E) -> Result<List<T>, E>`
  - `ToHashSetOrError(errorFactory: Exception -> E) -> Result<HashSet<T>, E>`
  - `ToDictionaryOrError(keySelector, errorFactory) -> Result<Dictionary<TKey, T>, E>`
  - `ToDictionaryOrError(keySelector, elementSelector, errorFactory) -> Result<Dictionary<TKey, TValue>, E>`

These unions provide lightweight error-handling primitives while keeping Raven
programs compatible with the .NET type system.

## Raven.Core union stability contract

Raven.Core treats its built-in union carriers as .NET-compatible value types by
default. This keeps the ABI aligned with the C# generated-union direction:

- Plain `union` declarations synthesize struct carriers.
- C# callers can use the public single-payload constructors, `HasValue`,
  `Value`, and typed `TryGetValue(out T)` overloads without Raven-specific
  runtime helpers.
- `union class` remains available for reference-type carrier semantics and is
  the right choice when a union must satisfy `class` constraints or intentionally
  behave like an object hierarchy at runtime.
- `default(Option<T>)`, `default(Result<T, E>)`, and default values of standard
  `Union<...>` carriers are inactive carrier states. They are not semantic
  `None`, `Error`, or `null` cases.
- Raven.Core helpers should rely on the active `self` contract rather than
  writing source-level default arms. Visible default-capable receivers are
  rejected at the call boundary; forced invalid carriers are handled by the
  emitted match fallback.
- Match expressions and statements are source-exhaustive when they cover the
  declared semantic cases. Raven source does not require a discard/default arm
  just because a struct carrier has a physical inactive default state.
- Lowering and emit preserve a defensive runtime fallback for source-exhaustive
  matches, throwing when no arm matches so metadata consumers and forced default
  carriers cannot fall through silently.
- Code may still write an explicit discard/default arm when it intentionally
  wants to handle a possible inactive carrier at runtime. Such an arm is not
  redundant when flow says the matched struct-union value may be default.
- Passing or returning a struct-union value across a method boundary requires a
  value that flow analysis knows is active. Parameters and `self` are already
  active inside the callee; fields, properties, and default-capable locals must
  be proven active before they are forwarded.
- Local struct-union variables may temporarily hold the inactive default state,
  either because they were explicitly assigned `default` or because analysis has
  not yet proved an active assignment. That is a valid intermediate program
  state. The diagnostic boundary is where such a value is passed, returned, or
  otherwise exposed to code that expects an initialized carrier.
- JSON converters defensively serialize inactive default carriers as JSON
  `null`. This is a runtime safety behavior for forced serialization of an
  invalid carrier state, not a language-level `null` union case.
- Built-in carriers have a documented payload-first `System.Text.Json`
  contract. Tagged/discriminator serialization is opt-in through
  `RavenTaggedUnionJsonConverterAttribute` or explicit converter registration;
  no tagged union policy is installed globally.

This contract deliberately separates Raven semantics from the physical .NET
carrier representation. A union's declared cases describe the semantic domain;
the inactive default carrier exists because struct values in .NET always have a
zero-initialized state.

## Future work

The current implementation is intentionally conservative. Future Raven.Core and
compiler work should focus on:

- Exhaustiveness: keep fallback lowering covered as new match forms are added,
  including the planned prefix-standard and trailing/postfix match expression
  split.
- Boundary analysis: keep expanding assignment/active-state analysis only where
  new language constructs create genuinely new boundaries. The current
  contract covers ordinary calls, generic method calls, extension receivers,
  delegate invocation, params elements, constructors, async/lambda returns, and
  field/property forwarding; future work should focus on richer control-flow
  joins, nested projections, and interop/imported-union cases while keeping
  diagnostics at the call or return boundary.
- JSON deserialization policy: decide whether JSON `null` should deserialize to
  active `None`/fallback `Error`, remain a serializer-only representation for
  inactive carriers, or become configurable per converter.
- Null modeling: defer any `null` pseudo-type, `Null` case-stand-in, or
  nullable union extension until the C# union design settles further. Raven
  should not reintroduce `null` as a pseudo member type in the core union ABI.
- Analyzer support: add lightweight Raven.Core audits that flag helper methods
  returning raw boundary carriers or unconstrained `default` values from
  `Option`, `Result`, or `Union<...>` APIs.
- Extension coverage: continue adding focused runtime tests for Raven.Core
  extension helpers that accept union receivers, especially helpers that forward
  `self`, return nested unions, or bridge `Option` and `Result`.

Project builds include a generated prelude source file that globally imports
the standard `System` namespaces, `System.Result.*`, and `System.Option.*`.
That prelude is why standard cases such as `Ok`, `Error`, `Some`, and `None`
are usually available as simple names in project code. User-defined union cases
require qualification, target-typed member syntax, or an explicit
`import UnionType.*`. Global imports are hoisted across the compilation but
still bind as ordinary imports; type-scope and direct nested-case imports require
the imported type or nested type to be available to the compilation.

When you want OOP-style subtype semantics, prefer Raven sealed hierarchies instead of unions.

JSON serialization behavior for `Option<T>`, `Result<T, E>`, standard type
unions, and opt-in tagged Raven unions is documented in
[Raven Core JSON serialization](json-serialization.md).

### `Unit`

`unit` is Raven's "no meaningful value" type. The compiler represents it as a
`System.Unit` struct with a single static `Value` field so callers can observe a
unit value even though the underlying IL maps the type to `void`. Code that
returns `unit` emits `Unit.Value` as needed to bridge the gap between the
expression-oriented language semantics and .NET metadata.
When `unit` appears in generic type arguments or parameter positions, the
compiler preserves it as `System.Unit` so constructed signatures can resolve
against emitted union cases and helper methods.

### Generated types

When the compiler builds Raven.Core (and when it embeds core shims for
standalone compilations) it also synthesizes a few supporting types that aren't
hand-written in `src/Raven.Core`:

- `System.Runtime.CompilerServices.UnionAttribute` and
  `UnionCaseAttribute` annotate union types and their cases so the
  emitted IL retains union metadata.
- `System.Unit` is emitted whenever a project depends on the `unit` type,
  ensuring the `Unit.Value` shim is available even when compiling without an
  existing Raven.Core reference.
