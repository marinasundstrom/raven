# Record inheritance equality

Generated record-class object equality delegates to typed equality. Open records
have a virtual typed Equals(R?) member. Derived records seal the base-typed
override and route it through object dispatch so comparisons through base
references still observe derived state. Generated typed equality compares own
instance fields and calls the base typed equality nonvirtually. Root records
compare their virtual EqualityContract property; root hashes include it.

Record structs use typed equality for object equality and generated operators as
well. Authored typed equality therefore controls these paths consistently. The
class equality operator preserves reference identity and left-null handling,
then passes the right operand (including null) to typed equality. Inequality
negates equality. This allows authored equality to decide how to handle null,
as in C#.

## Record copying (2026-11-01)

Record classes now synthesize a public parameterless clone operation. Open roots
introduce a virtual slot; derived records override it, retaining the root return
type so neither backend needs covariant-return metadata for this operation.
Shared `with` lowering invokes the clone and converts the result to the receiver's
static type before applying updates. The runtime record type and original state
are preserved. Record structs retain their existing copy path.

The target Runtime Contract owns the metadata name (`<Clone>$` in the current
contracts). This lets C# consume emitted Raven records with `with`, including
base-typed, derived-typed and generic records. The copy-constructor metadata follow-up also validates C# declarations inheriting
from an emitted open Raven record, including base-typed copies and derived equality.
This covers the tested non-generic hierarchy; broader inheritance combinations
remain part of compatibility validation.

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

- Align record-struct copying, required-member attributes on non-copy constructors
  and primary-constructor delegation diagnostics.
- Audit record-struct property mutability/readonly rules and diagnostics for
  user-defined generated-member collisions and invalid custom signatures.
- Broaden C# inheritance validation beyond the tested non-generic open-record hierarchy.
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


### Typed equality validation

The typed equality extension passes 67 focused record/portable-plan tests. An
additional 18-test run covers updated display/equality metadata comparisons and
stored-state regressions: nine Roslyn comparisons now check equality member
accessibility, virtual/final flags and overloads as well as display/deconstruction.
Custom class/struct equality is exercised through object, IEquatable and operators,
including null and custom base equality. A union payload regression proves the
record correction propagates to union value comparison and hash consistency.

Unions retain their own carrier/case representation and quoted payload display;
they do not acquire the reference-record EqualityContract hierarchy. The relevant
shared correction is that payload object equality now honors authored typed record
equality. Union customization restrictions remain a separate language question.
The subsequent readonly metadata slice validates source record-struct IsReadOnly,
imported method markers and generated record/union metadata.

Final combined validation: 329 record/union semantic, runtime, interoperability,
completion and portable-plan tests pass. A malformed nested union fixture was
corrected (missing closing parenthesis); the newly exposed generic annotation
conversion bug was reproduced with the prior frozen compiler and fixed separately
in 8c67db2cd. Its 14 focused nullable substitution/runtime tests also pass.


### Copy-constructor metadata

Synthesized reference-record copy constructors are private for closed records and
protected for inheritable records. The shared clone operation remains the public
copy entry point used by `with`. A copy constructor satisfies required members
when its record or an ancestor declares them; the corresponding .NET attribute is
omitted when there are none. Required-member discovery happens after record body
members are completed rather than being frozen during early record synthesis.

Generated constructors have no authored syntax reference. Their required-member
marker must still be emitted, just like their compiler-generated metadata. These
symbol contracts are shared by the targets; .NET metadata checks do not establish
native neoCLR execution. Record-struct copy
synthesis and the existing conservative required-member handling for other record
constructors remain separate compatibility work.


The five differential metadata cases cover closed, open and derived record
classes, including own and inherited required properties. An additional runtime
interop case compiles a C# derived record against Raven metadata and checks
base-typed `with` copies and derived equality. Two diagnostic cases also reject
direct calls from unrelated code to synthesized copy constructors. All eight
focused tests pass on .NET.
