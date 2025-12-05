# Raven Core Library

`Raven.Core` is the standard library that ships with the compiler. It contains
foundational types implemented in Raven and compiled into `Raven.Core.dll` so
programs can rely on a consistent Option/Result vocabulary across projects.

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
