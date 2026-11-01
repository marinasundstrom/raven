# Raven node-emitter library

This project ports `SyntaxKindGenerator`, `NodeFactsGenerator`, `TokenGenerator`, `VisitorGenerator`
and their immutable kind models from the C# node generator. It preserves the
existing emitters' output modes: C# syntax kinds/facts/token factories and Raven
syntax kinds/facts, plus Roslyn-based C# visitors and rewriters for green/red nodes. It references the Raven shared-model library.

The kind models use primary-constructor records in both languages, retaining
value equality and init-only properties. Raven requires record instance data to
be declared in the primary constructor. Required names/types are constructor
arguments; optional token characteristics retain their defaults. Private helpers
are explicitly private, and local generator functions retain closure captures.

`test/GeneratorParity` runs shared C# behavior tests and compares complete outputs
against the C# emitters using the repository XML models. Run
`scripts/test-generator-parity.sh` from the repository root.

The production NodeGenerator executable still uses C#. This is not yet a port of
its command-line driver, model loaders or green/red node emitters. No generated-source language syntax was changed by this translation.

The visitor translation retains iterator dispatch grouping, named tuples and
Roslyn syntax-factory calls. Its mutable argument list uses the fully qualified
`System.Collections.Generic.List<ArgumentSyntax>()`: the imported Roslyn factory
also defines `List<T>()`. Raven call syntax lacks C#'s `new` distinction, so an
unqualified invocation selects that factory instead of constructing a mutable
list. This is a name-resolution adjustment, not a generator-algorithm rewrite.
