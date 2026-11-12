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

The Raven service interface and complete binary lookup implementation are now
translated. Representation checks use `ILiteralTypeInfo`,
`INullableTypeIdentity`, and `TypeKind.Error`; the existing concrete symbols
implement those contracts. `OperatorFacts` is now translated, including its readonly record-struct result. Do not substitute simplified
conversion or Self implementations to make the port compile. The lookup port
retains descriptor reference identity and the existing candidate search order.

Validation uses modern .NET compilation tests, including neoCLR Self contract
fixtures with synthetic CLI metadata. It does not establish native neoCLR,
.NET Framework, or NanoFramework execution.

Before the lookup port, the baseline and post-change operator/Self suite both passed 43 tests. Three new
C# integration tests verify canonical promotion types, nullable-result factory
use, and standard-conversion requests for user-defined operators. The Raven
contract emitted without diagnostics at that stage.

Six provider-based representation tests fail before the extraction and pass after
it: literal identity, nullable-reference identity, mixed nullable-value equality
in both operand orders, and early error recovery in both operand orders. The
combined operator, service, representation, and Self suite passes all 52 tests.
The suite now contains 27 shared C# tests and is linked into both parity projects.
All 27 pass against the emitted Raven implementation; the focused C#
operator/service/Self suite passes 73 tests. It covers numeric promotion,
checked copies, nullable arithmetic/equality, booleans, shifts, pointers, enums,
implicit standard conversions for user-defined candidates, numeric type-parameter
constraints, and runtime Self projection through the service contract. The Self
fixture tests delegation, not native execution. Full-project validation of this
new lookup slice remains pending; isolated emission includes the real translated
symbol helper implementations, not replacements.
