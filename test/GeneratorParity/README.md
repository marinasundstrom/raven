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
