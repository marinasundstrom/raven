# Raven documentation

Raven is a pragmatic, typed application language for .NET that makes functional
composition, algebraic modeling, and object-oriented design complementary parts
of one toolset, with direct access to the .NET runtime and ecosystem.

Raven is under active development. These pages describe the compiler as it
works today and identify areas whose design is still changing.

## Learn Raven

- [Choose a learning path](learn.md) based on your programming experience.
- See [Raven in 60 seconds](raven-in-60-seconds.md) for the language's core
  ideas in one small program.
- [Install and run Raven](getting-started.md) from a source checkout.
- Follow the [language introduction](introduction.md) for a guided tour.
- Use [Raven for C# developers](raven-for-csharp-developers.md) to translate
  familiar .NET concepts into Raven.
- Read [Metaprogramming in Raven](metaprogramming.md) to choose between
  procedural macros, .NET reflection, and the in-process compiler APIs.
- Read [Raven for absolute beginners](raven-for-absolute-beginners.md) for a
  slower introduction to programming and the language.

## Language and tooling

- The [language documentation](lang/README.md) organizes syntax, features, and
  common language rules by topic.
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
