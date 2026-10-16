# Explicit compound operator lowering

Development implementation, 2026-10-16. This is shared Raven language behavior,
with separate qualification on `main` (.NET) and `codex/neoclr` (native neoCLR).
The author requested C# 14-style compound operator overloads after function-feature
integration. Broader structural work remains parked.

## Contract and representation

`func +=(rhs: R)` is an instance `MethodKind.UserDefinedOperator` with one ordinary
parameter and a unit result. The parser uses the existing operator declaration
node and existing compound tokens. Source symbol creation checks arity, instance
status, accessibility, result and parameter passing mode. Symbol display uses the
source token for both source and imported metadata.

| Raven | Metadata name |
| --- | --- |
| `+=` | `op_AdditionAssignment` |
| `-=` | `op_SubtractionAssignment` |
| `*=` | `op_MultiplicationAssignment` |
| `/=` | `op_DivisionAssignment` |
| `&=` | `op_BitwiseAndAssignment` |
| `\|=` | `op_BitwiseOrAssignment` |
| `^=` | `op_ExclusiveOrAssignment` |
| `%=` | `op_ModulusAssignment` |
| `<<=` | `op_LeftShiftAssignment` |
| `>>=` | `op_RightShiftAssignment` |
| `>>>=` | `op_UnsignedRightShiftAssignment` |

Binding considers instance operators on the receiver through ordinary member
lookup and overload resolution. It only does so for variable storage, preserving
property/indexer fallback and event subscription binding. An applicable operator
becomes an ordinary instance invocation; there is no hidden allocation, replacement
assignment or extra receiver evaluation. Existing receiver address emission handles
value-type storage. Assignment expressions retain Raven's unit result, including
calls to imported CLI-void methods.

CLI emission uses public instance special-name methods and a void return. The
metadata is compatible with the C# 14 compound names. The native compiler must
emit the same operation as an ordinary instance call in native metadata; it needs
no new runtime opcode or Runtime Contract capability flag. Qualification must use
the native compiler and a matching library bundle, not just successful CLI emission.

## Boundaries and verification

`CompoundOperatorTests` covers source metadata, alias preservation, struct locals,
array receiver evaluation, precedence/fallback, invalid declarations, readonly
locals and an emitted-library consumer. Existing operator tests protect the static
operator path. TextMate recognizes the new assignment and unsigned-shift tokens. Public syntax
kinds distinguish each assignment; `BinaryOperatorKind.UnsignedRightShift` exposes
the zero-filling operation to semantic API consumers.

Validation on Raven main: 70 focused operator/conversion/reference-field tests passed
on .NET 11 (2026-10-16). Native evidence is recorded separately after carrying the
shared change to `codex/neoclr`.

The initial slice does not add checked forms, compound extension/interface members,
virtual overrides. These remain separate work. Performance follows ordinary instance
calls; no benchmark claim is made.

## Design sources

The [C# 14 specification](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-14.0/user-defined-compound-assignment)
defines in-place operators, variable eligibility, applicability-first fallback and
metadata names. The [author-supplied article](https://www.damirscorner.com/blog/posts/20250919-UserDefinedCompoundOperatorsInCs14.html)
illustrates the allocation/aliasing motivation. The former is the semantic model;
the latter is context. Raven deliberately preserves its unit-valued assignments.


## Native qualification on codex/neoclr

The shared change is carried from Raven main `7819e2fea`. Native callable planning
and linear invocation lowering admit instance compound operators through the
existing instance-method capability. A separate `ElementAddress` linear instruction
is enabled by the native adapter and maps to metadata-SDK `OpCode.Ldelema` (neoCLR
main `1b4561f3b`). This reuses the runtime's existing managed array address operation.
Build with a matching `NeoClrMetadataProject`; no Runtime Contract setting changes.

The neoCLR fixture `docs/validation/compound-operators/Main.rvn` returns 42 in the
interpreter and macOS ARM64 JIT/AOT. It tests reference alias identity, explicit
operator precedence, struct-local mutation and single evaluation of a reference
array index. `ValueArrays.rvn` additionally qualifies struct-array mutation in the
interpreter. AOT rejects nominal struct-array allocation explicitly; this remains a
runtime backend gap, not an alternative compound-assignment semantic. The checked-in
neoCLR qualification record contains bundle/compiler hashes and commands. This does
not claim native imported-library or all-platform coverage.
The remainder/shift follow-up adds `%=` / `<<=` / `>>=` / `>>>=` and binary
`>>>`, using .NET operator metadata names on both current targets. Predefined
shifts preserve an `int` count independently of the left operand width, and
compound shifts convert promoted small-integer results back to storage. Generic
closing brackets are split contextually from the longest-match shift tokens.
No new Runtime Contract setting is required. Native execution needs separate
qualification after carrying the shared change. Capability interfaces with
purpose-specific semantics remain separate design work; implementing `+` does
not automatically establish a numeric contract.

Follow-up validation on main: 101 focused compound/operator, assignment syntax,
generic-name and binary-binding tests pass on .NET 11 (2026-10-16); the required
generated-syntax solution build also passes.

Remainder/shift native carry from main `b5e6bb0d3`: 104 focused tests pass on
`codex/neoclr`. The expanded neoCLR consumer returns 42 in interpreter, macOS
ARM64 AOT and managed JIT, including all four compound declarations, zero fill,
masked counts and 64-bit operands. Native `>>>` lowers to the existing unsigned
shift operation; no new metadata encoding or runtime opcode is introduced.
The generator build reached a stale RavenDoc reference-resolution failure; an
explicit restore/build with `-p:NeoClrMetadataProject=<matching SDK project>`
succeeded. Use the explicit MSBuild property for this native compiler build.
## Instance increment and decrement

`func ++()` and `func --()` declare public parameterless instance operators
returning unit. CLI names are `op_IncrementAssignment` and
`op_DecrementAssignment`; the existing static one-parameter declarations retain
`op_Increment` / `op_Decrement`. Both forms may coexist.

For writable variables, instance operators take priority for prefix expressions
and discarded postfix expressions. Consumed postfix expressions retain static
overload selection, preserving the old value. Properties/indexers retain static
overload plus setter behavior. Consumed prefix expressions evaluate storage once,
invoke on a captured value, write value types back and return the updated value;
reference types preserve identity. Readonly targets are rejected. This follows
the linked C# 14 rules and requires no Runtime Contract flag.

Prefix fallback also stores its computed result, preventing double invocation of
a static operator. Newline-prefixed increments begin a new statement instead of
attaching to the preceding expression as postfix.

Instance increment validation: 231 focused compound, increment and user-defined
operator runtime tests pass on .NET 11, including class/struct results, array
index side effects, readonly rejection and consumed-postfix static dispatch.

Native increment carry from `94f133cab`: 233 focused compiler tests pass. The
expanded native consumer returns 42 under interpreter, macOS ARM64 AOT and JIT,
including class/struct prefix results and static consumed-postfix fallback.
Callable planning admits parameterless instance increment methods; execution uses
ordinary instance calls without new Runtime Contract settings or opcodes.
