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
incremental hashing and command-line orchestration remain in C#. The C# output path remains available; nullable slot annotations are aligned in both output paths.

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


`RavenGreenNodeGenerator.GenerateNode` and `GenerateFactory` now provide the first
experimental Raven green-node output in both implementations. They emit node
inheritance, typed/raw constructors, ordered slot properties, visitor dispatch,
red-node creation, identity-preserving updates and factories. Abstract nodes do
not have factories. Explicit aliases for SyntaxKind and SyntaxAnnotation avoid
importing conflicting red-node names into the green-node namespace.

The shared C# tests parse output for all repository models, compare exact Raven
output between the C# and Raven emitters, and compile/run a generated hierarchy
against a focused runtime-contract fixture. The production CLI still generates
C# nodes; the fixture is not the full production runtime port. This work fixed
compiler gaps in override matching/dispatch, nominal nullable-reference constraints
and constructor-initializer target typing. Both output paths now explicitly mark
raw green slots nullable. See
[the output readiness investigation](../../docs/investigations/raven-syntax-output-readiness.md).
`RavenVisitorGenerator.GenerateVisitor` now emits generic and non-generic Raven
visitor partial declarations for either the public red or internal green namespace.
Abstract-root dispatch includes transitive concrete descendants in ordinal order;
unknown nodes and unoverridden concrete methods call the authored `DefaultVisit`.
The runtime owns traversal entry points, token/list handling and default behavior.
Shared C# tests compare both emitters, parse both namespaces, and execute generated
green visitor dispatch through two abstract levels and fallback paths.

`RavenRewriterGenerator.GenerateRewriter` adds concrete rewriting methods over those
visitors. Generated methods preserve explicit kinds, delegate tokens/lists to the
runtime hooks, skip absent optional children and call Update to preserve identity.
Required green lists explicitly assert the nullable runtime VisitList result is
present, matching their required-slot contract. Abstract dispatch retains the C# rewriter’s
ordinal cases and returns unknown nodes unchanged without invoking DefaultVisit.
Shared C# runtime checks cover generic/typed children, recursive replacement, lists,
deletion and no-op identity. List traversal remains authored runtime behavior.

`RavenRedNodeGenerator.GenerateNode` now emits Raven red-node declarations in
both implementations. It retains green/typed constructors (including `: self`),
lazy child/list caching, positioned token wrappers, Update/With identity and
visitor dispatch. Abstract roots can carry closed-hierarchy metadata. Repository
output is parsed and compared; a focused authored runtime fixture exercises
children, ordinary node lists, tokens, explicit kinds and red visitors on .NET.
Optional tokens, separated/token lists and closed-hierarchy metadata have source
coverage, not complete runtime coverage in that fixture.

Raven red factories, the production runtime port and CLI output selection remain
subsequent slices. No C# output mode has been replaced.
