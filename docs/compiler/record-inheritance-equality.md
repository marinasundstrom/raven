# Record inheritance equality

Generated record-class typed equality and operators dispatch through virtual
Object.Equals after identity/null guards. The generated Object override checks exact
runtime type before component comparison. This prevents base-typed comparisons from
ignoring derived state or comparing sibling/root instances as equal. Inherited
positional properties participate in generated display/hash/equality, even when
not repeated in the derived primary parameter list. Value-record paths are unchanged.

This follows .NET record value equality across inheritance; Raven retains its text
quoting. It uses ordinary virtual calls rather than a second equality hierarchy.
No metadata ABI or Runtime Contract change. Native emission has its own selected-core
implementation and requires separate validation, not a whole-branch merge.

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
