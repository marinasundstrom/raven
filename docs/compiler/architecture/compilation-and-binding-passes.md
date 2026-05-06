# Compilation and binding passes

This document describes the intended shape of Raven compilation, semantic binding, and incremental reuse. It is architectural documentation for compiler maintainers; public API details live in the compiler API docs.

## Goals

The compiler should answer language-service symbol questions without doing a full semantic pass when the required information is already known or can be declared cheaply.

The binding architecture follows these rules:

- Syntax trees are immutable and should preserve green nodes across edits when possible.
- Binders execute binding for a scope. They should not own cross-compilation cache policy.
- Compilation and semantic model state own reuse, invalidation, and transfer of cached binding descriptors.
- A symbol query should take the shortest valid path: declaration tables first, transferred descriptors next, then targeted binding, and full binding only when required.
- Incremental invalidation should be conservative where binding is order-sensitive, especially inside executable owners.

## Clean compilation

A clean compilation has no previous compilation state to reuse. It proceeds through these tiers.

### Parse

Each document is parsed into an immutable syntax tree.

The red tree provides parent-aware syntax nodes for consumers. The green tree stores immutable structure and text. Later increments can reuse green nodes for unchanged subtrees.

### Declaration discovery

The compilation discovers top-level and member declarations and records declaration keys. This phase should be cheap and syntax-driven.

Declaration discovery produces stable compiler-level knowledge such as:

- source namespaces and types
- method and property declaration locations
- declaration keys used by `GetDeclaredSymbol`

### Member signature declaration

Member signatures are declared before root binding when the syntax shape is sufficient.

This tier creates reusable source symbols for simple member signatures, including method and property symbols with parameter and return/property types when they can be resolved without binding the body.

The purpose is to let symbol queries such as hover, definition, and rename answer member questions without forcing the root binder.

### Targeted semantic queries

`SemanticModel` symbol APIs should prefer targeted lookup:

- `GetDeclaredSymbol` should use declaration tables and member signature state.
- `GetSymbolInfo` should use transferred descriptors or bind only the relevant local area.
- Language-server hover should use the normal semantic model API. It should not require a separate symbol-query semantic model path.

### Complete semantic pass

The complete pass creates root binders and binds all source declarations that require full semantic analysis.

This pass is required for full diagnostics, lowering, emit, and any API that explicitly asks for complete semantic information.

## Binding tiers

The current architecture treats binding work as tiers. Lower tiers are cheaper and should be exhausted before higher tiers.

| Tier | Purpose | Typical trigger | Expected cost |
| --- | --- | --- | --- |
| Declaration table | Identify declared syntax and declaration keys. | Compilation creation, `GetDeclaredSymbol`. | Syntax-only. |
| Member signature symbols | Create cheap member symbols without body binding. | Hover/definition on members, type member lookup. | Signature-only. |
| Transferred owner-relative descriptors | Reuse previous binding answers for unchanged nodes inside matched executable owners. | First query after an edit. | Cache lookup. |
| Targeted local binding | Bind the smallest required scope or expression root. | Symbol info for a changed local node. | Local to owner or binding root. |
| Complete binding | Bind all source declarations. | Diagnostics, emit, full semantic validation. | Whole compilation. |

## Incremental compilation

An incremental compilation starts from a previous compilation and a new solution snapshot.

The workspace compares syntax trees and classifies them into:

- reused syntax trees: the exact tree instance is still valid
- changed syntax trees: the tree was reparsed or incrementally updated
- matched executable owners: methods, constructors, accessors, properties, function expressions, global statements, or compilation units that can be related across the edit
- changed executable owners: owners whose text or structure changed

This classification is carried from workspace to compilation as an incremental compilation plan. The workspace owns document-version comparison and old-tree/new-tree analysis. The compilation owns consuming the plan: transferring reusable descriptor state, registering changed executable owners, and registering matched executable owners for later targeted semantic queries.

For reused syntax trees, exact descriptor state can be copied.

For changed syntax trees, only owner-relative state can be considered for transfer. The transfer policy remaps descriptor keys from the previous owner span to the current owner span.

Executable owner matching is syntax-driven and intentionally more precise for callable owners than simple name matching. Methods, functions, and constructors match by signature shape: owner kind, name where applicable, type-parameter arity, parameter ref/type shape, and return type for callables. Parameter names are excluded so a parameter rename can still be classified as a signature/declaration edit for the same owner. Parameter type and return type changes stop matching that owner and therefore drop its owner-relative descriptors. This avoids transferring descriptors from one overload to another when a same-name overload with the same arity is inserted before an existing method.

## Owner-relative descriptor transfer

Owner-relative descriptors are cached binding facts keyed by:

- executable owner descriptor
- relative start inside the owner
- node length
- syntax kind

They allow a symbol query after an edit to reuse facts for unchanged syntax even when absolute file positions shifted.

Owner-relative changed spans use the owner `Span` coordinate system, not `FullSpan`. Trivia can be preserved or changed without shifting semantic descriptor coordinates.

The transfer policy is intentionally outside binders. It decides whether a previous descriptor is still valid for a current syntax node.

The current policy is conservative:

- If the owner did not change, the descriptor can be remapped to the current owner.
- If the descriptor intersects the changed owner-relative span, it is invalidated.
- If the descriptor starts at or after the changed span, it is invalidated because earlier executable binding can affect later locals, diagnostics, overload resolution, and symbol meaning.
- If the descriptor is before the changed span, the current syntax node must still be incrementally equivalent to the previous descriptor node.

Incremental equivalence currently requires the same syntax kind, same descriptor span length, and either the same green node or identical full text.

This means a body edit can keep descriptor facts before the edit while dropping the edited node and everything after it. A method signature edit cascades into the body because body symbols may depend on parameters, type parameters, return context, and receiver/member state established by the signature.

## Binding invalidation model

Invalidation should be expressed in compiler state, not in individual binders.

The intended model is:

1. Classify the syntax edit.
2. Match reusable executable owners.
3. Compute owner-relative changed spans.
4. Transfer only descriptors allowed by the transfer policy.
5. Let binders recreate missing facts on demand.

This keeps binders focused on binding and lets incremental policy evolve independently.

Future improvements should make edit classification more explicit. For example:

- body-only expression edits can preserve declarations and descriptors before the edit
- local declaration edits should invalidate later body descriptors
- method signature edits should invalidate the method body but not unrelated members
- type member list edits should invalidate member lookup for the containing type without forcing unrelated method bodies
- green-node reuse can be used as a stronger signal that a subtree survived the edit

The compiler currently records owner-relative changes as `BodyExpression`, `BodyDeclaration`, `SignatureOrDeclaration`, or `Unknown`. Signature or declaration edits drop owner-relative descriptors for that owner. Body expression edits can reuse descriptors before the changed span, after identity validation. Body declaration edits are stricter for declaration-sensitive caches: visible-value scope declaration lists and contextual/interest binding roots are dropped because local declarations can change scope contents and targeted binding regions even when the queried node starts before the edit. Unknown edits are treated conservatively.

Nested executable owners can still match through a changed parent when they are fully before the parent's changed span and their syntax text is identical. This allows nested function rebind-root descriptors to transfer across later body edits while still dropping nested owners after or inside the edit.

Member-list edits are handled by rebuilding declaration and member-signature state for the current syntax tree. Declared member symbols are keyed to the current syntax tree and span, so added overloads and re-added members are declared in the new compilation without adopting stale symbols from the previous tree. Symbol queries for explicit member signatures should still stop before root binding.

## Language server expectations

The language server should use the same semantic model APIs as compiler consumers.

Hover, definition, references, and rename should benefit from the lower binding tiers:

- declaration/member hovers should stop after declaration or signature tiers
- unchanged local hovers after edits should use transferred descriptors
- changed local hovers should bind only the local owner or binding root needed for the query
- diagnostics may force complete semantic binding, but should not poison later targeted queries

The goal is that hovering a symbol after typing remains effectively immediate when the relevant syntax and binding facts are reusable.
