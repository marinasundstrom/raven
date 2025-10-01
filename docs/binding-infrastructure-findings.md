# Binding Infrastructure Findings

## Overview
This document captures several risks uncovered while reviewing the binder and related symbol implementations. Each subsection lists the observation, the potential fallout, and ideas for mitigation.

## Findings

### 1. Namespace lookup favors globals over the current scope
*Observation.* `Binder.LookupNamespace` queries the global namespace before the binder's current namespace, and only afterwards walks outward through parent binders.【F:src/Raven.CodeAnalysis/Binder/Binder.cs†L164-L175】

*Why it matters.* If a compilation defines `namespace Company { namespace Diagnostics { … } }` alongside a top-level `namespace Diagnostics`, any lookup for `Diagnostics` inside `Company` will resolve to the global declaration, making the nested namespace effectively unreachable without a fully qualified name. That contradicts typical name lookup rules where the innermost scope should win.

*Potential direction.* Reverse the lookup order (current → parents → global), or otherwise ensure the active namespace scope is checked before falling back to globals.

### 2. Qualified name resolution prefers namespaces over types
*Observation.* When resolving the left side of a qualified name, the binder probes for a namespace before attempting to resolve a type with the same name.【F:src/Raven.CodeAnalysis/Binder/Binder.cs†L567-L593】

*Status.* **Resolved.** `ResolveQualifiedNamespaceOrType` now consults type symbols before namespaces so nested/aliased types win over equally named namespaces, producing diagnostics when type arguments are omitted rather than silently binding to the namespace.【F:src/Raven.CodeAnalysis/Binder/Binder.cs†L567-L593】

*Why it matters.* In languages that allow a namespace and type to share a simple name, this ordering causes the namespace to shadow the type. As a result, constructs such as `Container.NestedType` can bind to the namespace path instead of the intended nested type, breaking member lookup and leading to confusing diagnostics.

*Potential direction.* Evaluate whether type symbols should be preferred (or at least considered on equal footing) when both a namespace and a type are viable matches. Adjusting the resolution order or reporting ambiguity would prevent silent misbinding.

### 3. By-ref syntax loses ref/out information during binding
*Observation.* Originally, `ResolveType` simply returned the element type for `ByRefTypeSyntax` nodes, ignoring the presence of the ref/out modifier even though a `ByRefTypeSymbol` exists in the type system.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ByRefTypeSymbol.cs†L5-L55】

*Status.* **Resolved.** `Binder.ResolveType` now constructs `ByRefTypeSymbol` instances (using contextual `RefKind` hints when supplied) so by-ref declarations retain their metadata through binding.【F:src/Raven.CodeAnalysis/Binder/Binder.cs†L328-L438】

*Why it matters.* Any code that relies on tracking ref, in, or out semantics during binding (for overload resolution, assignment checks, or emission) will see those types as plain value types. That can lead to incorrect conversions, missed diagnostics, or invalid IL generation.

*Potential direction.* Construct the appropriate `ByRefTypeSymbol` (respecting `RefKind`) when encountering `ByRefTypeSyntax`, so downstream phases receive accurate type information.

### 4. Nullable type symbols expose unfinished APIs
*Observation.* Nullable type syntax is lowered to `NullableTypeSymbol`, but that implementation throws `NotImplementedException` from its `LookupType` member (and other members rely on the underlying type).【F:src/Raven.CodeAnalysis/Binder/Binder.cs†L383-L387】【F:src/Raven.CodeAnalysis/Symbols/Constructed/NullableTypeSymbol.cs†L6-L52】

*Status.* **Resolved.** `NullableTypeSymbol` now delegates member and type lookups to its underlying and base types so the binder reports missing-member diagnostics instead of throwing.【F:src/Raven.CodeAnalysis/Symbols/Constructed/NullableTypeSymbol.cs†L23-L68】【F:test/Raven.CodeAnalysis.Tests/Semantics/NullableTypeTests.cs†L44-L70】

*Why it matters.* Any attempt to bind members or nested types on a nullable instance (for example via `T?.Nested`) will hit the unimplemented path at runtime, crashing the compilation process instead of producing diagnostics.

*Potential direction.* Either forbid such lookups explicitly during binding with clear diagnostics or flesh out the nullable symbol implementation so that member lookup is well-defined.

## Next steps
Prioritize fixes based on how frequently these scenarios occur in real-world code. A small suite of targeted unit tests around namespace shadowing, by-ref parameter binding, and nullable member access would help lock in the expected behavior once the issues are addressed.
