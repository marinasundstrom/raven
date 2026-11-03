# Generator foundation parity

These separate C# xUnit projects run the same tests against the C# and Raven
implementations of `NodesShared` and the self-contained node emitters. The Raven
project compiles Raven sources with the repository C# compiler seed. Identity
checks verify that the tested types come from the selected implementation.
A Raven-only comparison test references the C# model/generator assemblies through
explicit aliases and compares all output modes on the real repository XML inputs;
ordinary shared tests still exercise the selected implementation directly.

Run from the repository root:

```sh
scripts/test-generator-parity.sh
```

Results and compiler/implementation hashes are saved under
`artifacts/generator-parity` (override with `RAVEN_GENERATOR_PARITY_RESULTS_DIR`).
The current runtime is .NET 11; the C# tools library still targets .NET 10.
`Target=Shared` describes runtime-independent model/XML contracts, not proof of
execution on neoCLR. No documentation or website generator is run by this script.

Coverage includes mutable model defaults and identity, slot type names, XML
metadata and escaping, every collection slot in the repository syntax model,
and changes that must affect generator input hashing. Syntax-kind, node-facts and
token emitters are also ported, preserving their C# and Raven output modes.
Visitors and rewriters also have Raven translations, retaining Roslyn as their
C# syntax-building dependency. Coverage checks transitive dispatch ordering,
empty hierarchies, explicit kinds and green/red slot traversal, and compares all
three visitor/rewriter outputs against C# using the full repository node model.
Green-node classes and factories are also translated. Shared tests inspect
constructors, slot order, abstract/inherited properties, update behavior shape and
nullable factory overloads. Cross-implementation coverage compares node and
factory source for all 256 models (including 26 abstract nodes).
Red-node classes, closed hierarchies and factories are translated too. Their
shared tests cover accessors, updates, visitor dispatch, convenience defaults and
explicit aliases. Cross-implementation tests compare all repository red nodes and
both automatic and XML-defined factory overloads, including documentation trivia.
The five XML model loaders are extracted from the production C# driver and
translated too. Fixtures now call those production loaders. Shared tests cover
required attributes, defaults, optional factory input, ordering and flags; an
independent comparison checks all repository model data against the C# loader.
Model validation and command-line orchestration remain in C# for now.


Raven red-node output is also compared and parsed for every repository model.
The focused red runtime fixture executes constructors, lazy child/list caching,
positions, updates and red visitor dispatch. Both implementations run the same
C# tests; the fixture is not the production syntax runtime. Closed-hierarchy
attributes and optional token/separated-list output are currently source-level
coverage only. Automatic and XML-defined Raven factories are parsed and compared
for every model. Runtime checks execute full/convenience overloads, aliases,
default tokens, nullable children and explicit kinds in both query orders.
Complete source-set tests check every generated path, parse the repository output,
compare exact contents across implementations, and execute token factories/facts
with literal dollar signs and control characters. The C# driver now provides an
explicit Raven source-tree mode; production runtime translation remains later work.

## Symbol visitor output

The C# project also tests `BoundNodeGenerator`'s Raven symbol visitor output.
This generator has not itself been ported to Raven. Its opt-in mode reads the
same symbol contracts as the existing C# mode:

```sh
dotnet run --project tools/BoundNodeGenerator -- --raven-symbol-visitors <output-directory>
```

It writes `SymbolVisitor.g.rvn` and `SymbolVisitor.Generic.g.rvn`, preserving C#
outputs and their incremental stamp. Authored Raven visitor base methods and the
symbol interfaces belong to the consuming component. Tests parse deterministic
output, compile it with a small symbol fixture, and execute default forwarding,
overrides, inherited generic dispatch and null entry-point behavior. Integration
with the full Raven compiler symbol dependency set is a separate porting step.
