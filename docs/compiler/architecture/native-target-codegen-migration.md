# Native target codegen migration

Status: development plan, 2026-10-01. The author requested an architecture review
before further builder wrappers, then directed continuation. This plan supersedes
expanding the native source-operation visitor independently. Metadata import is
explicitly deferred; the independent neoCLR metadata API remains outside Raven.

## Boundary and sequence

1. Consume compiler-lowered bodies in the shared bounded instruction planner.
   Keep language rewrites in the existing Lowerer and encoding in backend adapters.
   This first slice is implemented for the existing linear Int32/Unit subset.
2. Introduce typed target-neutral references and declaration handles incrementally.
   First callable slice implemented: per-emission symbol-identity tables with typed
   backend handles, native predeclared functions/methods and adapter-owned call encoding.
   Shared source callable plans now supply identity, logical owner, signature and body
   to both backends. Native collection/validation precedes builder creation. General
   type/field references and full declaration traversal remain pending. The primitive
   boundary now has shared value/no-result classification and typed backend mappers
   reused by signatures and local declarations.
   Replace shared-path System.Type/MemberInfo dependencies; declare identities,
   signatures/members, bodies, then finalize. Preserve assembly-owned native functions
   and the existing CLI carrier representation. The earlier static-type prototype is
   now a shared source-type plan retaining symbol identity and backend builder contracts.
   It covers public/internal top-level nongeneric static classes, including partial declarations
   coalesced by semantic identity with all parts validated, not the general type boundary.
3. Int32/Boolean initialized locals and standalone assignments are now shared with metadata
   writer support. Boolean equality and short-circuit &&/||, signed comparisons, bound if statements and lowered loop
   labels/branches now execute through both backends. Primitive Int32/Int64/Boolean signatures now preserve parameter/result identity through declarations, imports and native projection. Signed Int32↔Int64 conversions and Int64 locals/constants are now shared, together with signed unary +/−/~. Broader signatures/conversions,
   then instances/fields remain planned; negated comparisons and loop exits are validated, while exceptions remain unsupported. Pair each capability
   with metadata writer/reader and runtime validation support as required.
4. String literals, signatures, initialized locals and imported calls now share the
   body path; computed Console text uses explicit backend policy. Nulls, equality,
   concatenation and general object types remain outside this bounded support.
   Statement-call result use is now shared: primitive results are discarded, while
   no-result calls leave the stack unchanged; backend-specific inhabited Unit/Console
   representations remain explicit. Centralize the remaining explicit representation and capability policies: Unit, entry points,
   function ownership, runtime helpers and strings. Reject unsupported native output
   before writing; ordinary .NET remains the default.
5. Backend-owned immutable instruction/built-in-type profiles now admit shared body
   plans before builder use; restricted profiles prove selective admission. Signed
   Int32/Int64 division, remainder, bitwise AND/OR/XOR and signed shifts now execute on both targets using that same planner. Logical assembly-function/static-method/static-type category admission now shares
   those backend profiles; public/internal static type visibility is also admitted explicitly. Public/internal/private static method visibility now follows the shared callable plan and explicit backend admission. Block and expression bodies now share compiler lowering across both bounded backends. Eager Boolean AND/OR/XOR also use the shared instruction path with exact Boolean operands. Primitive value-producing conditionals now share branch joins across both adapters. Broader metadata categories remain pending.
   Compose compatible backend, runtime policies and capabilities through native target
   selection. The current EmitOptions backend override remains experimental. Reserve
   a semantic import boundary, but do not redesign metadata loading in these slices.

Each slice must run a relevant program on .NET and from a binary assembly loaded by
neoCLR, with focused C# contracts and a separate documented commit. This is a staged
migration of existing codegen, not a commitment to a complete new intermediate language.

## Evidence and tradeoffs

Compilation still composes DotNetCompilationTarget. CodeGenerator maps SourceSymbol
to MemberInfo, IILBuilder operands include Reflection types/members, and general body
emission consumes BoundTreeView.Lowered. These are distinct migration points; builder
factories alone cannot make general codegen portable.

Compared with .NET's existing Reflection.Emit layer, typed references let native
metadata use its own ownership and signatures. The cost is explicit resolution,
lifetime and capability contracts plus dual-backend tests. Compared with continuing
the source IOperation consumer, lowered-body reuse inherits language rewrites (such
as implicit returns) and avoids reimplementing them. It also couples the internal
adapter to compiler bound nodes; those nodes are not a public metadata-library API.

The shared instruction planner remains bounded and separate from the general .NET
visitor. Full visitor convergence, synthesized methods, general locals/control flow, broader
signatures and native target composition remain open. Debug/PDB and unsupported .NET
bodies retain the established generator. This refactor is a shared-line candidate;
validation on the consumer branch is not evidence of integration into main.

## Target-neutral emission contract — 2026-10-01

The author reaffirmed a shared abstraction that fits .NET and neoCLR without tying
the compiler to either metadata implementation. Keep symbol-based type/member
references, declaration plans and logical body operations in the compiler-owned
layer; concrete handles, metadata encodings and instruction selection belong to
target adapters. Additional instruction families and metadata categories should be
exposed through explicit target capabilities, with unsupported output diagnosed
before writing, rather than forcing one target's representation onto every backend.
The existing primitive mapper, callable table and bounded body planner implement
only part of this boundary; general declarations/types/fields and composable
capabilities remain work to do. Ordinary .NET behavior remains the default.

Standard CLI metadata and instructions remain the baseline for ordinary constructs,
with explicit neoCLR extensions. The current native payload plus throwing CLI
reference projection is a bridge, not interchangeable executable .NET/neoCLR
artifacts; reconciling that representation remains open. Transport readability
by .NET metadata tools does not establish executable compatibility.

## Codegen performance follow-up — 2026-10-01

The author asks that codegen performance remain a consideration, with a possible
later revisit. Preserve per-emission identity caches and avoid repeated reference
resolution or encoding in instruction loops. The current shared plan materializes
instructions before opening backend bodies to preserve fail-before-output behavior;
its allocation cost and duplicate work on unsupported .NET fallback bodies are not
yet measured. Do not infer a speedup from introducing abstractions.

A later benchmark should separate declaration collection, bound-body planning,
reference resolution, encoding/PE serialization and total emit time, recording
allocations and warm/cold runs for representative small applications and libraries.
Compare the shared .NET path with its established generator and measure native
projection/payload writing separately. Optimize observed costs while preserving
diagnostics, selected-core binding and identical executable results. This is a
recorded follow-up, not a new benchmark result or a reprioritization of correctness.

## Required expression results (2026-10-02)

The shared body planner unwraps BoundRequiredResultExpression through value lowering,
preserving match-arm results. This general correction adds no target policy or binder
change. Its focused Debug/Release fixture validates shared planning with explicit pattern
and managed-local admission, and ordinary .NET execution returns 42. The Reflection.Emit
shared profile still delegates patterns to its existing backend. Native ArrayList search
consumers independently exercise the wrapper with Option case results.
