# Independent compiler fixes extracted from neoCLR integration

The author requested a dedicated fix branch based on main for changes that benefit
Raven independently. `codex/compiler-fixes-from-neoclr` starts at `d7040e21d`.
The fixes below carry no neoCLR adapter, metadata-library dependency or shared-emission
refactor. Ordinary .NET remains the default; Runtime Contract options are unchanged.
Tests are C# and use normal Raven compilation/emission rather than target plan assertions.
Source commit IDs record provenance from `codex/metadata-consumer`; mixed commits are
extracted by behavior rather than cherry-picked wholesale.

## Parse complete assignment right-hand expressions

Parse logical, coalescing and prefix-not expressions as complete assignment right-hand sides; retain right-associative chained assignments.

Source: 762bebad0. Validation: six focused parser cases; five fail on the original main.

## Honor accessible setters independently of public val mutability

Allow assignment, compound assignment and increments through accessible ordinary setters on val properties, while preserving private-setter access checks and public read-only semantics.

Source: 935598201. Validation: four binding cases including outside-scope rejection; positive case fails on original main.

## Preserve property accessor and backing-field identity during binding

Reuse completed auto-property accessors and stored backing fields during repeated binding; complete forward-bound initializers without replacing field identity. Validate ordinary .NET property values, aliases and constructor initialization.

Source: f90784db4, 92c509aa4, eaebf1934. Validation: six Debug/Release semantic-identity and execution cases, failing on original main.

## Check accessibility of qualified type expressions

Check accessibility when qualified member expressions resolve to types; external internal types now report RAV0500 while public wrappers remain usable.

Source: ee07a991e. Validation: two cross-assembly binding cases; inaccessible-type case fails on original main.

## Resolve generic method calls on their constructed declaring type

Resolve source method builders through their constructed generic owner before applying method arguments, preserving independent owner/method parameter scopes and avoiding invalid CLR programs.

Source: 339587142. Validation: Debug/Release execution with reordered owner and forwarded method parameters; both fail with InvalidProgramException on original main.

## Emit the implied constructor flag for struct constraints

Include the implied default-constructor flag in CLR metadata for struct constraints; preserve class and explicit new() flags.

Source: d09b5e8a1. Validation: three reflection-based metadata cases; struct flag case fails on original main.

## Validation and integration boundary (2026-10-01)

On base `d7040e21d`, the extracted C# fixtures produce 16 failures and seven
passes. With these six fixes, all 23 cases pass. The surrounding assignment,
property, generic-method/constraint and accessibility suites pass 174/174.
The fresh-worktree `scripts/codex-build.sh` and subsequent compiler build pass.
Execution evidence is modern .NET (net11.0); it does not establish .NET Framework,
NanoFramework or native neoCLR execution. No full-suite or release claim is made.

Reproduce the extracted regressions with:

```sh
dotnet test test/Raven.CodeAnalysis.Tests -p:WarningLevel=0 --filter 'FullyQualifiedName~AssignmentRightHandSideParserTests|FullyQualifiedName~PrivateSetterAssignmentTests|FullyQualifiedName~AutoPropertySymbolStabilityTests|FullyQualifiedName~NominalPropertyInitializerTests|FullyQualifiedName~GenericOwnerMethodResolutionTests|FullyQualifiedName~SpecialTypeConstraintMetadataTests|FullyQualifiedName~QualifiedTypeAccessibilityTests'
```

Surrounding validation uses the same command with this filter:

```text
FullyQualifiedName~AssignmentExpressionSemanticTests|FullyQualifiedName~AssignmentStatementTests|FullyQualifiedName~PropertyBindingTests|FullyQualifiedName~CodeGen.PropertyTests|FullyQualifiedName~ValuePropertyAssignmentTests|FullyQualifiedName~GenericMethodTests|FullyQualifiedName~TypeParameterConstraintDiagnosticsTests|FullyQualifiedName~AccessibilityDiagnosticsTests|FullyQualifiedName~ImportedGenericMethodContextTests
```

The author authorized integrating proven fixes into main. This branch is eligible
for that integration without the neoCLR backend or metadata format experiments.
Shared vector-loop/transfer lowering (`a9defade8`), field-initialization plans and
backend abstractions remain candidates for separate isolation and validation.
The Option constructor mismatch exposed by the order-collections application is
still unfixed; it needs an independent compiler regression before integration.

## Concrete union-case argument target typing (2026-10-01)

The collections sample exposed a general binding bug: `Choice<Item>(None())`
selected the carrier's `Some<Item>` constructor. The contextual-typing helper
accepted any case from a parameter's union family, so the first candidate could
supply the wrong concrete case target before overload resolution. Match concrete
case names directly; retain family lookup only for a union carrier parameter.
No codegen recovery or target-specific rule is added. Runtime Contracts are unchanged.

`ImportedUnionConstructorTests` checks selected semantic constructor parameters and
executed payload/empty results across Debug/Release, both case declaration orders,
and `None()`, `.None()`, `None`, `.None`. All 16 cases pass; the original two-case
repro failed before the fix. Existing union semantic tests pass 184/184, and the
union codegen/overload/target-typing group passes 139/139. This addresses the recorded
carrier-selection blocker; native importer/runtime acceptance is a separate check.
