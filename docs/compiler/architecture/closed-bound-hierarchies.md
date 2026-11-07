# Closed bound-node families

Bound nodes are compiler-owned alternatives, not extension points. The C# compiler
uses `closed class` for abstract families and `sealed class` for concrete leaves.
The Raven port uses `sealed partial class` families and final concrete classes.
Visitors and walkers remain extensible so new analyses can operate on the model.
Nodes retain reference identity; closing the family does not change their value,
update, traversal or evaluation contracts. Exhaustive Raven dispatch retains the
C# visitors' null-input fallback through explicit guards and shared tests.

The bound-node generator derives Raven `permits` lists from the selected ported
nodes and their intermediate ancestors. Lists contain direct children only and
are deterministic. Adding a ported node updates its family's list through normal
generation. Handwritten roots and generated permits live in separate partial
declarations. Never bypass closure by opening a leaf to support a test fixture;
use an existing node with the required semantics.

C# generation recognizes `closed` as implicitly abstract, so families never become
concrete visitor cases. This requires the C# 15-capable toolchain and Roslyn 5.9 in
the bound-node generator. The compiler still builds for .NET 10 and .NET 11. The
.NET 10 assembly supplies the standard `IsClosedTypeAttribute` shape locally;
.NET 11 uses the framework attribute. Raven targeting .NET 11 emits the same
`DerivedTypes` metadata contract. The shared tests compare those lists with the
actual direct children and verify final leaves and extensible visitors.

Closure is a shared compiler-model invariant, not a .NET backend policy. Native
neoCLR execution remains separate from these .NET metadata and parity checks.
The prerequisite partial-permits fix exercises both .NET and neoCLR CLI semantic
configurations; see [partial declaration completion](partial-sealed-hierarchies.md).

Focused validation uses shared C# tests against the C# assembly and the emitted
Raven bound-tree slice. The C# parity harness passed 1,355 tests, and the focused
Raven slice passed eight hierarchy/control-flow tests. Another 154 declaration,
metadata, hierarchy and overload tests and 15 adjacent runtime tests passed with
the updated C# compiler on .NET 11. All 74 generator tests passed. Both C# target
frameworks build, and five metadata/dispatch contract tests also execute on .NET 10.
The full Raven-project parity run passed all 1,419 tests on .NET 11.
