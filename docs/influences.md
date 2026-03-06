# Influences

Raven draws from several languages, but combines them into its own model and style.

## C#

- Roslyn-inspired compiler architecture (syntax trees, binding, semantic model, diagnostics).
- Deep .NET runtime interop and IL emission.
- Familiar type/member model adapted to Raven conventions (members are public by default).

## F#

- Expression-first programming and practical functional composition.
- Pattern matching as a central control-flow mechanism.
- Strong preference for explicit data modeling of success/failure.

## Kotlin

- Explicit mutability (`val` and `var`).
- Primary-constructor ergonomics that influenced promoted constructor parameters.
- Pragmatic blend of OO and functional styles.

## Swift

- Readable, low-ceremony syntax.
- API-focused design with clear declaration forms.
- Emphasis on clarity over boilerplate.

## Rust

- Explicit handling of fallible flow as data.
- Immutable-by-default mindset (mirrored by idiomatic `val` usage in Raven).
- Lightweight propagation patterns (`?`) for linear success paths.

## Beef

- Pragmatic, modern language design for .NET-adjacent ecosystems.
- C#-friendly syntax surface with room for richer type constructs (e.g., unions).

## Summary

Raven is not a clone of any single language. Its current style intentionally combines:

- C#/.NET tooling and runtime pragmatism,
- F#/Rust-like explicit flow modeling (`Result`/`Option`),
- Kotlin/Swift-like syntax ergonomics and mutability clarity.

That combination results in Raven's own identity: expression-first code, public-by-default members, explicit `val`/`var`, and carrier-based control flow.
