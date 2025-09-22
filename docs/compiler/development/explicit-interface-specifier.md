# Explicit Interface Specifier Syntax Investigation

This note captures the parser work needed to support explicit interface member
names in the Raven syntax tree. The end goal was to align the surface structure
with Roslyn, while keeping the changes restricted to syntax modeling and
parsing.

## Syntax tree modeling

* `Model.xml` now gives `BasePropertyDeclaration` a nullable
  `ExplicitInterfaceSpecifier` slot. This aligns property-like declarations with
  `MethodDeclaration`, which already exposed the specifier. Adding the slot to
  the base node ensures that both properties and indexers can surface the value
  without duplicating metadata in every derived node.
* The generated red/green nodes and factories pick up the new slot after running
  `tools/NodeGenerator` in the `src/Raven.CodeAnalysis/Syntax` directory.

## Parser changes

* `ParseMember()` can no longer rely on `PeekToken(1)` to decide between
  methods, properties, and indexers—the explicit interface qualifier inserts a
  `.` before the name. The routine now creates a checkpoint, calls the shared
  `ParseMemberNameWithExplicitInterface()` helper, inspects the token that
  follows the parsed name, and rewinds the stream before delegating to the
  concrete member parser. This makes the decision resilient to interface
  qualifiers.
* `ParsePropertyDeclaration()` and `ParseIndexerDeclaration()` were updated to
  call `ParseMemberNameWithExplicitInterface()` directly. They now accept the
  explicit interface specifier and reuse the same helper that methods rely on.
* `ParseMemberNameWithExplicitInterface()` handles the qualifier by parsing a
  `TypeSyntax` and, when the result is a `QualifiedNameSyntax`, splitting off
  the right-most simple name. The left side feeds the new
  `ExplicitInterfaceSpecifier`, while the right-most token continues to populate
  the existing `Identifier` slot. Falling back to the previous identifier logic
  keeps the helper backwards-compatible for unqualified members.

## Implementation notes

* The helper converts the right-most name token into an identifier with
  `ToIdentifierToken` to preserve existing expectations about the kind stored in
  the `Identifier` slot.
* Because lookahead uses checkpoints, the parser rewinds cleanly after probing
  the stream to decide which member form to parse.
* No semantic or emitter changes were required; the syntax tree now models the
  qualifier and the rest of the pipeline can opt into consuming it later.
