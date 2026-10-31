# Two-target reconciliation checks — 2026-10-30–31

These receipts describe validation of the merge candidate from Raven main
`6a02b0c42` and `codex/neoclr` `f4fd726ad`. See the
[reconciliation ledger](../../target-reconciliation-2026-10.md) for final status,
limitations and the post-merge refactoring direction. They are not a bootstrap
release receipt. Absolute paths record the actual local invocations.

- `final-target-controls.json`: refreshed after the qualified lookup, readonly
  receiver and cold import-scope fixes. The same compiler builds/runs .NET and neoCLR controls; native
  source-core/Self probes and three frozen signals consumers pass. Hashes identify
  compiler, adapter, metadata library and frozen runtime binaries.
- `runtime.json`: complete runtime workflow membership, distinguishing the final
  cold-import correction and its 208 focused/270 language-server follow-up checks.
- `framework-matrix.json`: .NET 10/.NET 11 library builds and representative executions.
- `baseline.json`: the completed earlier full baseline; it predates the final
  compiler corrections, whose subsequent checks are recorded separately.
- `combined-driver.json`: the same net11.0 compiler driver targets .NET 10 and
  native neoCLR. Both programs return 42. Artifact hashes identify this checkpoint.
  `final-combined-driver.json` is a later intermediate checkpoint; both predate
  `final-target-controls.json`.
- `native-source-core.json`: native source-defined core execution, including
  expected faults for negative array length and uninitialized element access.
- `native-signals.json`: frozen production-library reflection and async callbacks
  run in the interpreter and return success. Ordinary SignalsDemo also passes,
  as recorded in the ledger.
- `native-documentation.json`: native imported documentation generation and
  sidecar lookup. Nonzero exits in this receipt are intentional invalid-input
  and invalid-output controls; the checker asserts each expected status.

The external metadata library and runtime were built from the neoCLR checkout
at `4ca2b990b` (the checkout advanced from `a079cffd` during validation through
release documentation/script changes). Production library artifacts were copied
to temporary directories before consumer compilation. Native signals are qualified
only in the interpreter; native JIT/AOT support is not claimed. Neither .NET
Framework nor NanoFramework was executed.

Reproduction uses the repository .NET 11 SDK and an explicit optional native
metadata dependency:

```sh
export NeoClrMetadataProject=/path/to/neoclr/tools/metadata/NeoCLR.Metadata.Experimental/NeoCLR.Metadata.Experimental.csproj
scripts/codex-build.sh
scripts/test-baseline.sh
scripts/test-runtime-isolated.sh
scripts/test-target-framework-matrix.sh
```

For native fixture/probe commands and the required empty seed and Object-root
configuration, follow `tools/NeoClrMetadataProbe/README.md`. The default legacy
container probe is not a current qualification gate. Full release/bootstrap
sample and packaging gates remain separate.
