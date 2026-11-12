# Bound address and local-declaration contracts

`BoundAddressOfExpression` preserves the referenced value type separately from its
address type. Its optional `Storage` describes the complete addressable expression;
`Receiver` may also identify the receiver inside that storage. A walker visits
storage when present, allowing normal child traversal to visit the receiver once.
For receiver-only forms it visits the receiver directly. Visiting both would count
the shared receiver twice, while visiting only the receiver loses storage operands
and index expressions.

Variable declarators retain normal initializers, fixed address initializers and
pinned locals. The general bound walker visits the ordinary initializer and the
fixed address initializer when present. Pinned locals are symbol references, not
additional expression children. Local declarations preserve declarator order and
snapshot the input sequence.

Address types retain the original referenced type for equality and hashing,
forward its members and interfaces, and represent address type identity without
becoming value records. Their containing assembly and module may be null when the
referenced type has no owner; both implementations annotate that fact instead of
using null-forgiving expressions.

Constructor failures preserve their original order: a missing explicit value type
reports `ArgumentNullException` for `valueType`; missing or untyped storage reports
`ArgumentException` for `storage`. The explicit constructor validates before base
construction, so its body does not repeat the same null check.

These are shared compiler IR rules. The pointer/reference lowering and runtime
capabilities remain owned by their targets. Modern .NET tests of these contracts
do not claim native neoCLR execution.

Validation: seven shared C# tests pass against the C# implementation and are part
of the 100-test passing cumulative isolated Raven slice. Ninety adjacent semantic
and 41 runtime/emission tests pass on modern .NET. The required build script also
passes. The Raven source draft includes the original lazy member-resolution helper
and preserves derived-signature precedence; its full-project integration remains
pending the preceding bound-node batch.

## Concrete pointer-type wrapper

`PointerTypeSymbolImplementation` follows the same optional-owner contract as the
address wrapper. Its assembly/module properties preserve nulls from the pointed-at
type and expose nullable metadata. It retains pointer naming, `TypeKind.Pointer`,
non-reference/non-value classification, element-based equality/hashing and member
forwarding. The direct Raven draft uses the shared `SymbolBase` and member-resolution
helper; this does not alter either target's pointer capabilities or lowering.

Pointer validation: four of five baseline contract tests passed; the nullable-owner
metadata test failed before correction. After correction, all 32 focused C# pointer
contract/semantic tests and all five shared tests against the isolated Raven wrapper
pass on .NET 11. The Raven wrapper remains a draft pending dependency integration.
