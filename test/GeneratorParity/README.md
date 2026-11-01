# Generator foundation parity

These separate C# xUnit projects run the same tests against `tools/NodesShared`
and `tools/NodesShared.Raven`. The Raven project compiles Raven sources with the
repository C# compiler seed. Identity checks verify that the tested types come
from the selected implementation; the Raven tests do not reference the C# model
assembly.

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
and changes that must affect generator input hashing. This is the shared model
and serializer port, not a claim that the node emitters are already Raven code.
