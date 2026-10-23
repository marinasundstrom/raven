# Optional naming convention

Raven.Core and neoCLR use `System.Optional<T>` with `Some(value)` and `None`.
This replaces `Option<T>` on both targets. Migrate source references and rebuild
libraries and consumers; no Option alias is added. The Some/None cases, absence
semantics, propagation protocol and JSON representation are unchanged.
Raven.Core's converter and extension container types also use Optional names.

Compiler conveniences share internal `OptionalFacts` constants for the type,
qualified/metadata names and cases. This covers propagation, conditional access,
None parameter defaults, completion, diagnostics and generated prelude imports.
The existing None-default metadata marker is retained so this API rename does
not unnecessarily change the compiler's parameter-default encoding. CLR optional
parameters and arbitrary domain unions remain separate concepts.

The framework projection catalog returns Optional from supported .NET Try APIs.
Projection IDs and Runtime Contract settings are unchanged. neoCLR's temporary
CLI projection and native metadata retain ordinary named-union representations;
the identity changes to Optional, without adding an opcode or layout convention.
A constant is not a runtime naming switch: authored declarations, API references,
editor grammar, examples and consumers must migrate together.

The same migration preserves Result.Ok/Failure. Domain unions remain free to
name a case Error; imported custom-union and exception diagnostics fixtures
explicitly retain their independent names.

Validation: 183 final regression checks pass on this branch, including the
corrected naming fixtures and top-level context inference. All 84 Raven.Core tests
pass on .NET 10. The broader changed-fixture run exercised over 1,300 tests per
branch; its rename-related failures were corrected and rerun. Two existing
ExtensionMethodSemanticTests lowering-trace checks remain failing and reproduce
with pre-migration compiler sources from main `59a8d09ea`:
`Lowerer_WithLoweringTrace_RecordsNestedQueryPipelinesAcrossExtensionSources` and
`Lowerer_WithLoweringTrace_RecordsNestedExtensionInvocations`. They concern trace
receiver provenance, not Optional execution. Full diagnostics now prepare the
top-level return context without binding the statement body twice.

Both branches also pass all 84 Raven.Core tests and 18 ConditionalAttribute tests.
Raven main passes 144 hover/semantic-token tests (three existing skips) with the
matching rebuilt Raven.Core reference. Detached macro syntax uses its owning
semantic model for conditional symbols; the existing macro-hover tests cover this
regression. Native error-context and method-state suites pass on macOS ARM64 in
interpreter/JIT/AOT (42 combined executions). API snapshots and 6,064 website pages
pass; 26 website unit tests pass.
