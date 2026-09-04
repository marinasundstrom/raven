# Incremental compilation stabilization matrix

This matrix is the repeatable editor-recovery corpus for Raven. It builds every
selected project with the one-shot SDK compiler, then applies transient edits
through the language-service workspace. Every source snapshot must round-trip
through a cold parse; diagnostics and semantic results after undo must match the
initial workspace snapshot.

| Project | Shape covered | Edit coverage |
| --- | --- | --- |
| `hello-world` | Small single-file application | Whitespace, unexpected tokens, undo, empty file |
| `conditional-compilation` | Directives and forced full-parse policy | Whitespace, unexpected tokens, undo, empty file |
| `top-level-members` | Multi-file namespace functions and constants | Whitespace, unexpected tokens, undo, empty file |
| `repository-result-patterns` | Multi-file generics, `Result`, repository abstractions, and union errors | Whitespace, unexpected tokens, undo, empty file |

Run the matrix with:

```bash
scripts/test-incremental-project-matrix.sh
```

The test reports source-file count, source size, snapshot count, and elapsed time
for each project. Timing is intended as a coarse regression signal rather than a
microbenchmark. Correctness is the release gate: valid baselines must build with
the SDK compiler, syntax must round-trip exactly in cold and incremental parsing,
no diagnostic request may throw, and diagnostics plus public semantic queries
must return to their baseline after every undo.

When adding a project, prefer a distinct compiler boundary rather than another
sample with the same shape. Keep projects without external services in the core
matrix so it stays deterministic and suitable for local stabilization runs.
