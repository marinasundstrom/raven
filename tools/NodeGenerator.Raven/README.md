# Raven node-emitter library

This project ports `SyntaxKindGenerator`, `NodeFactsGenerator`, `TokenGenerator`,
`VisitorGenerator`, `GreenNodeGenerator`, `RedNodeGenerator` and their immutable kind models from the C# node generator. It preserves the
existing emitters' output modes: C# syntax kinds/facts/token factories and Raven
syntax kinds/facts, plus Roslyn-based C# green/red nodes and factories, closed hierarchies and
visitors/rewriters for green/red nodes. It references the Raven shared-model
library.

The kind models use primary-constructor records in both languages, retaining
value equality and init-only properties. Raven requires record instance data to
be declared in the primary constructor. Required names/types are constructor
arguments; optional token characteristics retain their defaults. Private helpers
are explicitly private, and local generator functions retain closure captures.

`test/GeneratorParity` runs shared C# behavior tests and compares complete outputs
against the C# emitters using the repository XML models. Run
`scripts/test-generator-parity.sh` from the repository root.

The production NodeGenerator executable still uses C#. Its XML loading methods
are extracted into `SyntaxModelLoader` and translated in this library; validation,
incremental hashing and command-line orchestration remain in C#. No generated
language syntax was changed by this translation.

The visitor translation retains iterator dispatch grouping, named tuples and
Roslyn syntax-factory calls. Its mutable argument list uses the fully qualified
`System.Collections.Generic.List<ArgumentSyntax>()`: the imported Roslyn factory
also defines `List<T>()`. Raven call syntax lacks C#'s `new` distinction, so an
unqualified invocation selects that factory instead of constructing a mutable
list. This is a name-resolution adjustment, not a generator-algorithm rewrite.

The migration proceeds in two stages: first compile the Raven translation and
verify output parity with the C# implementation; then add Raven syntax-tree,
factory, visitor and rewriter output. Preserve C# output for the bootstrap
codebase. The existing Raven syntax-kind/fact output does not imply that full
Raven syntax-node generation is already implemented.

Green-node output retains constructors, ordered slot properties, update/With
identity checks, red-node creation and visitor dispatch. Factory methods return
null for abstract models; that return contract is nullable in both languages.
The constructor local is scoped to its concrete-node branch in both versions.

Red-node output retains lazy child/list accessors, token wrappers, constructors,
updates, With methods and visitor dispatch. Factory output retains optional-token
and nullable-slot defaults, explicit overloads, aliases and documentation trivia.
The abstract-node factory return contract is nullable in both implementations.

The five XML loaders retain required-attribute errors, optional factory files,
boolean/integer fallback rules, ordering and all node/hierarchy/factory/kind/token
metadata. Shared C# tests exercise both implementations. Emitter parity fixtures
use these production loaders, and independent C#/Raven loading of the repository
XML is compared before generated-output parity. The loader is an internal tool
component; it does not add a public compiler API.

Both loader implementations use `XName.Get` explicitly, matching the shared XML
serializer. Raven currently does not refine XName's nullable implicit-conversion
result using `NotNullIfNotNull`; this explicit API choice preserves the same XML
names without weakening nullability rules.

Begin Raven syntax-tree output component by component once the corresponding
translated emitter is validated. Pair the first generated node hierarchy with
its authored Raven runtime foundations so the emitted source can be compiled
and exercised by shared C# tests. Completing the entire command-line port is not
a prerequisite for this output work; preserve the C# output mode throughout.
