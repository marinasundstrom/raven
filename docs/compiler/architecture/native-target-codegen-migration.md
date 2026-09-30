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
   type/field references and full declaration traversal remain pending.
   Replace shared-path System.Type/MemberInfo dependencies; declare identities,
   signatures/members, bodies, then finalize. Preserve assembly-owned native functions
   and the existing CLI carrier representation. The earlier static-type prototype is
   now a shared source-type plan retaining symbol identity and backend builder contracts.
   It covers public top-level nongeneric static classes, not the general type boundary.
3. Int32/Boolean initialized locals and standalone assignments are now shared with metadata
   writer support. Initial signed comparisons, bound if statements and lowered loop
   labels/branches now execute through both backends. Primitive Int32/Boolean signatures now preserve parameter/result identity through declarations, imports and native projection. Broader signatures/conversions,
   then instances/fields remain planned; negated comparisons and loop exits are validated, while exceptions remain unsupported. Pair each capability
   with metadata writer/reader and runtime validation support as required.
4. Centralize explicit representation and capability policies: Unit, entry points,
   function ownership, runtime helpers and strings. Reject unsupported native output
   before writing; ordinary .NET remains the default.
5. Compose compatible backend, runtime policies and capabilities through native target
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
