# Record inheritance equality

Generated record-class typed equality and operators dispatch through virtual
Object.Equals after identity/null guards. The generated Object override checks exact
runtime type before component comparison. This prevents base-typed comparisons from
ignoring derived state or comparing sibling/root instances as equal. Inherited
positional properties participate in generated display/hash/equality, even when
not repeated in the derived primary parameter list. Value-record paths are unchanged.

This describes the existing equality implementation, not the final C# compatibility
contract. Native emission has its own selected-core implementation and requires
separate validation.

The failing emitted .NET baseline now passes through base-typed, interface and
Object dispatch, sibling/root rejection and inherited display. All 37 record
semantic tests also pass. No performance improvement is claimed.

Generated copy constructors mark required members initialized. An explicit derived
copy now compiles and retains both base and derived values; the 38-test focused
cohort includes this execution check.

## Record copying (2026-11-01)

Record classes now synthesize a public parameterless clone operation. Open roots
introduce a virtual slot; derived records override it, retaining the root return
type so neither backend needs covariant-return metadata for this operation.
Shared `with` lowering invokes the clone and converts the result to the receiver's
static type before applying updates. The runtime record type and original state
are preserved. Record structs retain their existing copy path.

The target Runtime Contract owns the metadata name (`<Clone>$` in the current
contracts). This lets C# consume emitted Raven records with `with`, including
base-typed, derived-typed and generic records. It does not establish support for
C# declarations inheriting from Raven records: copy-constructor accessibility and
other C# inheritance metadata remain a separate interoperability question.

The native declaration collector and portable callable planner explicitly include
this operation, using the same synthesized body and copy constructor. Native
record equality and formatting remain separately implemented and were not replaced
by the CLI implementations. See [the bridge record](neoclr-cli-bridge.md) for
validation and limits.

## C# record compatibility (2026-11-03)

C# record semantics are the author-directed compatibility model, including body
storage, constructors, value equality, copying, inheritance and generated members.
Raven keeps its source syntax and public-by-default convention. Missing behavior
is a stabilization issue, not an intentional restriction or a reason to replace a
ported record with an ordinary class.

The first extension admits body storage and additional constructors. Shared
binding registers these members normally; CLI synthesized equality/hash/copy
traverse declared instance fields (including auto-property backing fields), using
base record operations for inherited state. Computed getters are not equality or
copy components. Positional parameters alone determine synthesized deconstruction;
empty records have no generated Deconstruct. Constructor delegation uses the
existing `: self(...)` and `: base(...)` paths. Positional properties are not
implicitly marked required.

Generated CLI text follows C# record formatting, including nonpositional and
generic records, public fields/readable properties, computed getters, empty/null
values and base PrintMembers dispatch. It uses System.Text.StringBuilder and
synthesizes the appropriate private/protected PrintMembers method when absent.
Root formatting calls RuntimeHelpers.EnsureSufficientExecutionStack when available.
SynthesizeStructuralToString remains the existing opt-out configuration. No new
syntax tokens, syntax nodes or TextMate patterns are needed; existing member
binding supplies language-service symbols.

### Remaining compatibility work

These are implementation gaps to fix, not supported semantic divergences:

- Replace the current Object.Equals-centered dispatch with the C# EqualityContract
  and typed virtual equality hierarchy, including user-defined typed equality,
  derived sealed base-typed overrides and hashing contracts.
- Align copy-constructor accessibility, record-struct copying, required-member
  attributes and primary-constructor delegation diagnostics.
- Audit record-struct property mutability/readonly rules and diagnostics for
  user-defined generated-member collisions and invalid custom signatures.
- Validate C# inheritance from emitted Raven records, not only consumption via with.
- Extend and execute the native neoCLR paths for body storage and C#-compatible
  display/equality. The shared binder change does not prove native backend support.

Validation: the pre-change focused baseline passed 66 tests. The updated record
suite passes 65 tests, including nine runtime differential comparisons against
Roslyn-compiled C# for display/deconstruction and four runtime tests for stored
state, constructor delegation, equality/hash and copy behavior. Another 69 adjacent
property, struct, parser, sealed-hierarchy and portable-plan tests pass. These executions
use modern .NET, not native neoCLR, .NET Framework or NanoFramework. The
MetadataImportOptions direct Raven draft also passed its four C# tests with the
initial stored-state implementation; integration into the port remains separate.
