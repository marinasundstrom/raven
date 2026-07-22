---
_layout: landing
---

# Raven documentation

Raven is an experimental programming language and .NET compiler inspired by
Swift, Rust, and F#. It combines expression-oriented syntax, pattern matching,
explicit mutability, and direct access to the .NET ecosystem.

> [!IMPORTANT]
> Raven is under active development. The documentation describes the current
> compiler, but language and tooling details can change before a stable release.

## Start using Raven

- [Get started](getting-started.md) to build the compiler and run a program.
- Follow the [language introduction](introduction.md) for a guided tour.
- Use [Raven for C# developers](raven-for-csharp-developers.md) to translate
  familiar .NET concepts into Raven.
- Read [Raven for absolute beginners](raven-for-absolute-beginners.md) for a
  slower introduction to programming and the language.

## Reference

- The [language specification](lang/spec/language-specification.md) defines the
  intended observable language behavior.
- The [non-normative EBNF grammar](lang/spec/grammar.ebnf) summarizes Raven's
  structural syntax.
- The [tooling documentation](compiler/index.md) covers command-line tools,
  projects, diagnostics, and editor support.
- The [compiler API documentation](compiler/api/README.md) covers the
  `Raven.CodeAnalysis` programming model separately from language features.

## Documentation status

Tutorials and reference pages describe implemented behavior unless they
explicitly say otherwise. Compiler implementation notes, investigations, and
language proposals remain in the source repository, but are intentionally not
part of this user documentation.
