# Generic parameter attributes

Raven accepts attribute lists before a generic parameter's variance modifier
and name. Type and method parameters expose these through
`ITypeParameterSymbol.GetAttributes()`, and `SemanticModel.GetDeclaredSymbol`
resolves their declaration syntax. Attribute argument, target, and duplicate
validation uses the ordinary attribute rules.

The CLR backend emits custom attributes on `GenericParam` rows. The PE importer
loads those attributes on the same parameter symbols. This follows the .NET
metadata model; no Runtime Contract configuration or target-specific mapping is
needed. Retention analysis remains the responsibility of the consuming runtime
or trimming tool. This compiler change does not itself implement a trimmer.

The existing global TextMate attribute pattern covers attribute lists inside
angle brackets. No new keyword or editor-only syntax is introduced. Semantic
queries for the parameter and its attribute constructor are regression-tested.

Validation on 2026-10-28: `scripts/codex-build.sh` passed, including syntax
regeneration. The focused AttributeParsingTests, AttributeUsageTests, and
CustomAttributeEmissionTests passed 61 tests on .NET 11 (58 before this change).
The new tests cover variance/constraints, source preservation, semantic queries,
invalid usage/duplicates, CLR metadata placement, and PE re-import. Other runtime
targets were not executed by this validation.
