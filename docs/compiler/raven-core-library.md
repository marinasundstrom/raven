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
You can still override the reference with `ravc --raven-core <path>` when
compiling standalone projects.

## Types included in Raven.Core

### `Option<T>`

A discriminated union with two cases:

- `Some(value: T)` holds a value.
- `None` represents the absence of a value.

Extension helpers provide ergonomic accessors:

- `UnwrapOrDefault()` returns the contained value or `default(T)`.
- `UnwrapOrThrow()` returns the value or throws `InvalidOperationException` if
  none is present.
- `UnwrapOr(defaultValue: T)` returns the value or the supplied fallback.

### `Result<T>` and `Result<T, E>`

Two result shapes are available:

- `Result<T>` differentiates success (`Ok(value: T)`) from failure
  (`Error(message: string)`), with `IsOk`, `IsError`, and unwrap helpers mirroring
  `Option<T>`'s patterns.
- `Result<T, E>` separates payload and error data by allowing an arbitrary error
  payload type in the `Error(data: E)` case.

These unions provide lightweight error-handling primitives while keeping Raven
programs compatible with the .NET type system.

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

- `System.Runtime.CompilerServices.DiscriminatedUnionAttribute` and
  `DiscriminatedUnionCaseAttribute` annotate union types and their cases so the
  emitted IL retains union metadata.
- `System.Runtime.CompilerServices.TypeUnionAttribute` marks union-typed fields and parameters. The compiler emits this namespaced attribute in generated metadata but will also honor user-defined `TypeUnionAttribute` types with the same constructor signature, regardless of their namespace.
- `Null` provides a concrete type for the language's `null` literal when
  unions mention a null case.
- `System.Unit` is emitted whenever a project depends on the `unit` type,
  ensuring the `Unit.Value` shim is available even when compiling without an
  existing Raven.Core reference.
