# Binary operator services during porting

`BoundBinaryOperator` consumes the internal `BinaryOperatorServices` contract,
which extends the canonical type provider used by unary lookup. The contract
supplies conversion classification, nullable-result construction, and runtime
Self capability/projection. `Compilation` implements it using the existing
conversion engine, nullable factory, and target-selected Self contract.

Operator selection, candidate order, numeric promotion, pointer arithmetic,
nullable lifting, and the requirement for implicit standard conversions remain
owned by lookup. User-defined operator applicability still disables user-defined
conversions. The service boundary introduces no second cache or type mapping.

The Self projection returns no method when the selected target has no applicable
Self contract. Ordinary .NET remains the default; the neoCLR Self path still
uses `RuntimeSelfTypes` and `NativeSelfMethodSymbol` behind `Compilation`.

The Raven service interface is translated and emits in isolation. Binary lookup
itself is not yet ported: its concrete literal/nullable/error representation
checks must be separated before translation. Do not substitute simplified
conversion or Self implementations to make the port compile.

Validation uses modern .NET compilation tests, including neoCLR Self contract
fixtures with synthetic CLI metadata. It does not establish native neoCLR,
.NET Framework, or NanoFramework execution.

The baseline and post-change operator/Self suite both pass 43 tests. Three new
C# integration tests verify canonical promotion types, nullable-result factory
use, and standard-conversion requests for user-defined operators. The Raven
contract emits without diagnostics; binary-operator runtime parity is not yet
claimed because the implementation has not been translated.
