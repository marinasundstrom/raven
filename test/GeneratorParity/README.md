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
Green/red node classes, visitors, rewriters, model loading and the command-line
orchestration remain in C# for now.
