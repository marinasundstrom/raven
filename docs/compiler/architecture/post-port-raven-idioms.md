# Raven idioms to revisit after the compiler port

During the port, record a candidate here when translating it directly makes
parity easier to establish, but a different Raven representation could make
the finished compiler easier to maintain. Include the member, the proposed
change, the reason to defer it, and a test that would detect a change in
behavior. Mark an entry resolved with the implementing commit when it lands.

## Open candidates

| Source | Change to evaluate after parity | What the change must preserve |
| --- | --- | --- |
| `SealedHierarchyFacts.TryGetCaseDefinition` | Consider a private union result with distinct cases for an unknown hierarchy, a known hierarchy with no matching case, and a found case. The current Boolean and two output parameters encode these three outcomes. Keep the shared signature during the port so the existing C# tests exercise it directly. | Preserve the root even when the case is absent, ordinal case-name lookup, and all three outcomes covered by `SealedHierarchyLookupTests`. |
| `SealedHierarchyFacts.ProjectCaseTypeToHierarchyArguments` and `TryCollectTypeParameterSubstitutions` | Use a half-open `for index in 0..count` loop for the paired argument arrays. The initial translation keeps the C# traversal recognizable while generic projection is being validated. | Check arity before indexing, stop at the first conflicting substitution, and retain empty-range behavior. Run the shared projection tests. |
| `SemanticFacts.SatisfiesTypeConstraintCore` | Evaluate a guarded match on the argument and bound together. This could put the structural-union and intersection rules in one ordered list. The current sequence makes it easier to compare evaluation order with the bootstrap compiler. | Preserve precedence when both operands have a structural shape, lazy constraint reads, and the recursion bound. Run the shared generic-constraint tests for both implementations. |

## Conventions already used during translation

Use `match` for mutually exclusive alternatives on one value. A statement suits
visitor dispatch. An expression suits selecting a member's type. A `when`
guard belongs on the arm whose condition it qualifies. For a guarded bound
pattern, the walker must visit each present child, so its two optional-child
checks remain independent. When visitors are generated, change the generator
and verify the emitted Raven through the normal parity project.

The [porting policy](raven-port-progress.md#layout-and-translation-policy)
and [bootstrap decision](decisions/0004-port-first-bootstrap.md) set the timing
for broader changes. Continue fixing compiler defects as the port exposes them.
