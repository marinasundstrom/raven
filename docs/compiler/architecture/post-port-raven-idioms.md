# Raven idioms to revisit after the compiler port

During the port, record a candidate here when translating it directly makes
parity easier to establish, but a different Raven representation could make
the finished compiler easier to maintain. Include the member, the proposed
change, the reason to defer it, and a test that would detect a change in
behavior. Mark an entry resolved with the implementing commit when it lands.

## Deferred syntax proposal: binding-first patterns

Author direction, 2026-11-13: defer a possible `name @ pattern` form for binding
the whole value matched by a pattern. The intended token is `@`, not `&`.
This is a proposal, not supported Raven syntax or an instruction to rewrite the
port. Existing `if let`, `is` and whole-pattern designations remain in use.

Revisit the proposal separately, including binding scope, narrowed type,
mutability, precedence with `and`/`or`/`not`, escaped identifiers and consistency
across conditional bindings and match arms. Validation must preserve one-time
input evaluation and introduce the name only when the complete pattern succeeds.
Assess readability where the whole matched value is needed; do not apply it
mechanically to every conditional or pattern.

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

## Capture review

Use `is` for Boolean pattern tests, `if let` for branch-local captures and
`let ... else` for required captures used afterward. Review surviving capturing
`if ... is` conditions during each port and the post-port cleanup; retain them
only where they communicate the intent more clearly, such as a cohesive
short-circuit condition. Preserve evaluation order and single evaluation of
properties. The C# counterpart may retain its natural pattern-capture syntax.

## Nullability and formatting review

Repeat the [intentional-nullability audit](bootstrap-procedure.md#intentional-nullability-is-a-porting-directive)
across the ported compiler and its retained C# counterpart. This is a required
cleanup check, not a reason to defer known annotation defects during porting.

- Annotate intentionally absent inputs, results, stored state and collection
  elements in both languages, including generator output. Distinguish accepted
  absence from defensive checks on required non-null inputs.
- Review suppressions and sentinel nulls. Prefer accurate nullable contracts and
  small guard/binding rewrites; document any remaining proven invariant. Preserve
  evaluation order and exception behavior, with shared null/non-null coverage.
- Respect Raven's stricter null rules. Do not assume that C# acceptance alone
  proves the Raven translation is safe, or that defensive `?.` makes a nullable
  annotation unnecessary.
- Check same-line opening braces, multiline nonempty bodies, declaration spacing
  and immutable `let` bindings. Keep retrospective formatting in its own commit;
  annotation or behavioral fixes receive separate review and relevant tests.

Complete this audit before freezing C#. Subsequent ordinary cleanup follows the
Raven-only maintenance policy; the retained bootstrap seed is not routinely
restyled or reannotated after the freeze.

## Constructor and equality review

During translation and the post-port review, consider primary constructors for
simple initialization in both maintained implementations. Preserve constructor
signatures, validation order, property storage, nullability and accessibility.
Review records independently where value semantics suit the type; retain class
or struct representation and keep identity-bearing IR objects as ordinary
classes. Follow the [constructor policy](bootstrap-procedure.md#prefer-primary-constructors-when-they-clarify-initialization)
and validate each adoption with shared tests rather than applying a blanket rewrite.
