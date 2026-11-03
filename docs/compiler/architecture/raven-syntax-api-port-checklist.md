# Raven syntax API completion checkpoint

Author direction (2026-11-03): finish the syntax API, enforce Raven formatting,
commit this slice, then pause. The final repository-project validation passed:
893 C# tests pass against the complete Raven-owned runtime, including public-type
ownership checks. The C# implementation parity project passed 887 tests.

## Required surface

| Area | Implementation and validation contract |
| --- | --- |
| Generated syntax | Red/green nodes, factories, kinds, facts, visitors and rewriters are emitted by the preserved C# and Raven generator implementations. |
| Runtime and navigation | Tokens, trivia, lists, nodes, references, locations, annotations, diagnostics, extension/reflection helpers and source writing are Raven-owned. |
| Grammar | The lexer, contexts, checkpoints, declaration/expression/pattern parsers, interpolation helper and LanguageParser are translated. |
| Public parsing | SyntaxFactory fragment/member/compilation-unit APIs and SyntaxTree parsing, incremental replacement, fallback and submission completeness are translated. |
| Presentation | Normalization, green/red tree printers, printer options and RavenQuoter/options/output language are translated. |
| Ownership | C# tests assert that the syntax/runtime/parser types belong to the selected implementation assembly. Semantic symbol interfaces remain explicit bootstrap dependencies. |
| Shared regressions | Compiler fixes exposed by the translation are covered by focused diagnostics and runtime tests; shared behavior is corrected before target-specific emission. |

## Project layout and reproducibility

The C# bootstrap stays in `src/Raven.CodeAnalysis`; the Raven port stays in
`src/Raven.CodeAnalysis.Raven`. `Raven.CodeAnalysis.Raven.rvnproj` generates the
complete syntax tree beneath its intermediate output and includes those files
in both build-time and compiler project evaluation. Generated Compile items are
declared after explicit SDK imports so the language targets' default obj exclusion
cannot remove them during compiler reevaluation. The old kind/facts-only
source input is excluded. The bootstrap project reference is explicitly aliased
for unported semantic contracts.

Run `scripts/test-compiler-parity.sh` to build the checkout's C# seed and execute
the shared C# tests against both implementations. Run
`scripts/test-generator-parity.sh` for the two generator implementations.
The parity script records implementation identities, SDK, source state and hashes
under `artifacts/compiler-parity`. Neither component tests using bootstrap nodes
nor successful emission alone establish source-owned runtime parity.

## Formatting and boundaries

Use same-line opening braces, blank lines between declarations, explicit private
helpers and `let` for locals that are never reassigned. Preserve C# and Raven
output in generator templates. Formatting outside the syntax work was committed
separately as `1b40af6c2`.

.NET remains the default target. Shared semantic fixes apply before target
lowering; .NET runtime execution is not evidence of native neoCLR execution.
The semantic layer, code generator and compiler driver remain future port slices.
Do not generate RavenDoc or website output during porting.

See [the port progress log](raven-port-progress.md) for historical component
checkpoints, discovered gaps, exact validation results and known baseline failures.
