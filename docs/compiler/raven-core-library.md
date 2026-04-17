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

## Types included in Raven.Core

### `Option<T>`

A union carrier with two cases:

- `Some(value: T)` holds a value.
- `None` represents the absence of a value.

`Option<T>` is a `union class` that uses Raven's standard union carrier model:

- runtime values are `Option<T>` carriers
- case values convert into the carrier
- the carrier exposes a conventional non-nullable `Value: object` property containing the active case value
- the carrier exposes `HasValue: bool` for initialization-safe inspection across interop boundaries
- extraction uses `TryGetValue(out Some<T>)` / `TryGetValue(out None)` and pattern matching

For the built-in `Option<T>` class carrier, `HasValue` is expected to be `true`
for constructed values and `Value` points at the active case object
(`Some<T>` or `None`). The carrier itself is the stable runtime type; `Value`
is the authoritative active-case projection.

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

`Result<T, E>` is also a `union class` carrier:

- `Ok(value: T)` for success.
- `Error(data: E)` for failure.

Like `Option<T>`, `Result<T, E>` uses carrier semantics rather than inheritance semantics. Case values convert into the carrier, and extraction happens through `TryGetValue(out CaseType)` or pattern matching.
The carrier also exposes a conventional non-nullable `Value: object` property
containing the active case value.
The carrier also exposes `HasValue: bool`, though constructed class-carrier
instances are expected to report `true`.

For constructed `Result<T, E>` values, `Value` points at either the active
`Ok<T>` or `Error<E>` case object. `HasValue` exists for consistency with the
general union contract even though class carriers do not have the extra
default/uninitialized state that struct carriers can expose.

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

When you want OOP-style subtype semantics, prefer Raven sealed hierarchies instead of unions.

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
