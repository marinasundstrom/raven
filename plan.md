# Sealed Class Hierarchies - Implementation Plan

## Overview

Add Kotlin-style `sealed` class hierarchies to the Raven compiler. When `sealed` is explicitly written on a class declaration, it creates a **closed hierarchy** where only same-file types (or `permits`-listed types) may directly inherit.

### Key Design Insight

Raven classes are sealed-by-default (no inheritance without `open`/`abstract`). Currently `sealed` on a class is redundant. This feature gives the explicit `sealed` keyword a new, distinct meaning: **sealed hierarchy** — inheritance IS allowed, but only from a closed set of types. This is MORE permissive than a plain class (which allows NO inheritance), but MORE restrictive than `open` (which allows ANY inheritance).

The `IsSealed` property on `INamedTypeSymbol` retains its existing meaning ("cannot be inherited from" in general). A new `IsSealedHierarchy` property distinguishes hierarchy-sealed types. For inheritance validation, a sealed-hierarchy type is NOT treated as sealed in the traditional sense — it allows controlled inheritance.

---

## Phase 1: Syntax (Tokens, Model, Parser)

### Step 1.1: Add `permits` contextual keyword to Tokens.xml

**File**: `src/Raven.CodeAnalysis/Syntax/Tokens.xml`

Add after the `OpenKeyword` line (~line 92):
```xml
<TokenKind Name="PermitsKeyword" Text="permits" IsReservedWord="false" />
```

### Step 1.2: Add `PermitsClauseSyntax` and update `ClassDeclarationSyntax` in Model.xml

**File**: `src/Raven.CodeAnalysis/Syntax/Model.xml`

Add new node definition (near BaseList):
```xml
<Node Name="PermitsClause" Inherits="Node">
    <Slot Name="PermitsKeyword" Type="Token" DefaultToken="PermitsKeyword" />
    <Slot Name="Types" Type="SeparatedList" ElementType="Type" />
</Node>
```

Add `PermitsClause` slot to `ClassDeclaration` node, between `ConstraintClauses` and `OpenBraceToken`:
```xml
<Slot Name="PermitsClause" Type="PermitsClause" IsNullable="true" />
```

### Step 1.3: Run the NodeGenerator tool

```bash
cd tools/NodeGenerator && dotnet run -- -f
```

This regenerates all Green/Red nodes, factories, visitors, and rewriters.

### Step 1.4: Update TypeDeclarationParser to parse `permits` clause

**File**: `src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/TypeDeclarationParser.cs`

In `Parse()`, after parsing constraint clauses (~line 79) and before parsing `{`:
- Check if next token is `PermitsKeyword`
- If so, parse a comma-separated list of type names
- Construct `PermitsClauseSyntax`
- Pass it to the `ClassDeclaration` factory

Update the `ClassDeclaration` factory call to include the permits clause parameter.

### Step 1.5: Parser tests

Write parser tests that verify:
- `sealed class Expr permits Lit, Add { }` parses correctly with PermitsClause
- `sealed class Expr { }` parses with null PermitsClause
- `class Expr permits Lit { }` parses (permits on non-sealed class — binder will reject)
- Permits clause with generic types: `sealed class Expr permits Lit<T> { }`

---

## Phase 2: Symbol Infrastructure

### Step 2.1: Add sealed hierarchy data to SourceNamedTypeSymbol

**File**: `src/Raven.CodeAnalysis/Symbols/Source/SourceNamedTypeSymbol.cs`

Add new fields and properties:
```csharp
private bool _isSealedHierarchy;
private bool _hasExplicitPermits;
private ImmutableArray<INamedTypeSymbol> _permittedDirectSubtypes = ImmutableArray<INamedTypeSymbol>.Empty;
private string? _sealedHierarchySourceFile;

internal bool IsSealedHierarchy => _isSealedHierarchy;
internal bool HasExplicitPermits => _hasExplicitPermits;
internal ImmutableArray<INamedTypeSymbol> PermittedDirectSubtypes => _permittedDirectSubtypes;
internal string? SealedHierarchySourceFile => _sealedHierarchySourceFile;
```

Add internal setters:
```csharp
internal void MarkAsSealedHierarchy(string sourceFile, bool hasExplicitPermits)
internal void SetPermittedDirectSubtypes(ImmutableArray<INamedTypeSymbol> subtypes)
```

### Step 2.2: Add IsSealedHierarchy to INamedTypeSymbol interface

**File**: `src/Raven.CodeAnalysis/Symbols/ISymbol.cs` (where INamedTypeSymbol is defined)

Add to `INamedTypeSymbol`:
```csharp
bool IsSealedHierarchy => false;
ImmutableArray<INamedTypeSymbol> PermittedDirectSubtypes => ImmutableArray<INamedTypeSymbol>.Empty;
```

---

## Phase 3: Diagnostics

### Step 3.1: Add new diagnostic descriptors

**File**: `src/Raven.CodeAnalysis/DiagnosticDescriptors.xml`

Add these diagnostics (using RAV0334+ range):

| ID | Identifier | Message | Severity |
|---|---|---|---|
| RAV0334 | SealedHierarchyInheritanceDeniedSameFile | "'{typeName}' cannot inherit from sealed type '{baseName}' because it is not declared in the same source file." | Error |
| RAV0335 | SealedHierarchyInheritanceDeniedNotPermitted | "'{typeName}' cannot inherit from sealed type '{baseName}' because it is not listed in the 'permits' clause." | Error |
| RAV0336 | PermitsTypeNotFound | "Type '{typeName}' in permits clause could not be found." | Error |
| RAV0337 | PermitsTypeDuplicate | "Type '{typeName}' is listed more than once in the permits clause." | Error |
| RAV0338 | PermitsTypeNotDirectSubtype | "Type '{typeName}' in permits clause does not directly inherit from '{baseName}'." | Error |
| RAV0339 | PermitsClauseRequiresSealed | "The 'permits' clause is only valid on sealed classes." | Error |
| RAV0340 | SealedHierarchyMatchNotExhaustive | "Match expression on sealed type '{typeName}' is not exhaustive. Missing cases: {cases}." | Error |

### Step 3.2: Run the DiagnosticsGenerator tool

```bash
cd tools/DiagnosticsGenerator && dotnet run
```

### Step 3.3: Add diagnostic report extension methods

**File**: `src/Raven.CodeAnalysis/CompilerDiagnostics.cs` (or wherever report helper extensions are)

Add `Report*` methods for each new diagnostic that the binder will call.

---

## Phase 4: Binder — Declaration Phase

### Step 4.1: Detect sealed hierarchy in DeclareClassSymbol

**File**: `src/Raven.CodeAnalysis/SemanticModel.Binding.cs`

In `DeclareClassSymbol()` (~line 164):

After extracting modifiers, detect if the user explicitly wrote `sealed`:
```csharp
var hasSealedModifier = classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.SealedKeyword);
```

Key logic changes:
- If `hasSealedModifier` is true AND the class is not `static`:
  - Mark as sealed hierarchy (`IsSealedHierarchy = true`)
  - The class is NOT `IsSealed` in the traditional sense (it allows controlled inheritance)
  - Set `isSealed = false` for the standard sealed check (since hierarchy inheritance is allowed)
  - Store source file path from `classDecl.SyntaxTree.FilePath`
  - Check for `PermitsClause` on the declaration
- If `hasSealedModifier` is false:
  - Existing behavior: `isSealed = isStatic || (!hasOpen && !isAbstract)`

Also check: if `permits` clause is present but `sealed` modifier is not, report `RAV0339`.

### Step 4.2: Resolve permits list (second pass)

This must happen after ALL type declarations are created (since permits-listed types may be forward references).

**File**: `src/Raven.CodeAnalysis/SemanticModel.Binding.cs` or new dedicated method

After all types are declared, iterate sealed hierarchy types:
- If has explicit permits: resolve each name in the permits list to a type symbol
  - Report `RAV0336` if not found
  - Report `RAV0337` if duplicate
  - Store resolved types via `SetPermittedDirectSubtypes()`
- If no explicit permits (same-file closure): discover all types in same source file that directly inherit from this type
  - Store discovered types via `SetPermittedDirectSubtypes()`

### Step 4.3: Validate permits list entries

After subtypes are resolved and base types bound:
- For each permits-listed type, verify it actually directly inherits from the sealed base
  - Report `RAV0338` if not a direct subtype

---

## Phase 5: Binder — Inheritance Validation

### Step 5.1: Update inheritance checking

**File**: `src/Raven.CodeAnalysis/Binder/TypeDeclarationBinder.cs`

In `ReportInvalidInheritedBaseType()` (~line 95):

Current logic:
```csharp
if (baseType.IsSealed)
    Diagnostics.ReportCannotInheritFromClosedType(...)
```

New logic:
```csharp
if (baseType is SourceNamedTypeSymbol sourceBase && sourceBase.IsSealedHierarchy)
{
    // Check sealed hierarchy rules instead of flat rejection
    if (sourceBase.HasExplicitPermits)
    {
        // Only permitted types may inherit
        if (!sourceBase.PermittedDirectSubtypes.Contains(derivingType))
            Diagnostics.ReportSealedHierarchyInheritanceDeniedNotPermitted(...)
    }
    else
    {
        // Same-file closure
        if (derivingType.SyntaxTree.FilePath != sourceBase.SealedHierarchySourceFile)
            Diagnostics.ReportSealedHierarchyInheritanceDeniedSameFile(...)
    }
}
else if (baseType.IsSealed)
{
    Diagnostics.ReportCannotInheritFromClosedType(...)
}
```

Note: `ReportInvalidInheritedBaseType` currently only receives the `baseList` and `baseType`. We'll need to also pass the deriving type symbol to check file identity / permits membership.

---

## Phase 6: Match Exhaustiveness

### Step 6.1: Add sealed hierarchy branch to evaluator

**File**: `src/Raven.CodeAnalysis/MatchExhaustivenessEvaluator.cs`

In `Evaluate()`, after the type union check (~line 52) and before the else fallthrough:

```csharp
else if (scrutineeType is INamedTypeSymbol namedScrutinee && namedScrutinee.IsSealedHierarchy)
{
    missingCases = GetMissingSealedHierarchyCases(scrutineeType, arms, namedScrutinee, options);
}
```

### Step 6.2: Implement `GetMissingSealedHierarchyCases`

Algorithm:
1. Compute transitive closure of **concrete leaf subtypes** from `PermittedDirectSubtypes`
   - Recursively traverse: if a permitted subtype is itself a sealed hierarchy, include ITS leaves
   - If a permitted subtype is abstract, it's not a leaf — traverse its subtypes
   - If concrete (not abstract), it's a leaf
2. Create `HashSet<INamedTypeSymbol>` of remaining leaf types
3. For each match arm, remove covered types (using type pattern matching)
4. Return missing type names

### Step 6.3: Codegen for hidden fallback branch

**File**: `src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs` (or relevant match codegen)

When generating match expressions on sealed hierarchy types:
- Even if statically exhaustive, always emit a fallback branch
- The fallback throws `RavenMatchExhaustivenessException` with the runtime type name
- This protects against reflection or cross-language violations

---

## Phase 7: IL Emission

### Step 7.1: Update TypeGenerator for sealed hierarchies

**File**: `src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs`

In `DefineTypeBuilder()` (~line 84):

```csharp
if (named.IsSealed)
    typeAttributes |= TypeAttributes.Sealed;
```

Change to:
```csharp
if (named is SourceNamedTypeSymbol sn && sn.IsSealedHierarchy)
{
    // Do NOT emit TypeAttributes.Sealed — hierarchy allows controlled inheritance
}
else if (named.IsSealed)
{
    typeAttributes |= TypeAttributes.Sealed;
}
```

### Step 7.2: Emit internal constructors for sealed hierarchy base

After type builder is created, if it's a sealed hierarchy:
- Ensure no public/protected instance constructors are emitted
- Emit at least one internal instance constructor
- This blocks inheritance outside the assembly at the CLR level

### Step 7.3: Emit `[ClosedHierarchy]` attribute

**File**: `src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs`

Add method (following `CreateDiscriminatedUnionAttribute` pattern):
```csharp
internal CustomAttributeBuilder CreateClosedHierarchyAttribute(Type[] permittedTypes)
```

**File**: `src/Raven.Core` (or wherever runtime attributes are defined)

Define:
```csharp
[AttributeUsage(AttributeTargets.Class, Inherited = false)]
public sealed class ClosedHierarchyAttribute : Attribute
{
    public ClosedHierarchyAttribute(params Type[] permittedTypes) { ... }
    public Type[] PermittedTypes { get; }
}
```

In `TypeGenerator.cs`, after applying user attributes for sealed hierarchy types:
```csharp
if (sourceNamed.IsSealedHierarchy)
{
    var permittedClrTypes = sourceNamed.PermittedDirectSubtypes
        .Select(t => TypeSymbolExtensionsForCodeGen.GetClrType(t, CodeGen))
        .ToArray();
    var attr = CodeGen.CreateClosedHierarchyAttribute(permittedClrTypes);
    TypeBuilder.SetCustomAttribute(attr);
}
```

---

## Phase 8: Specification & Documentation Deliverables

### 8.1: Modifier Legality Matrix

| Modifier Combo | Valid? | Notes |
|---|---|---|
| `sealed class` | Yes | Sealed hierarchy, same-file closure |
| `sealed class ... permits` | Yes | Sealed hierarchy, explicit permits |
| `sealed abstract class` | Yes | Abstract sealed hierarchy (ADT-style) |
| `sealed open class` | Error | Contradictory |
| `sealed static class` | Error | Static already sealed |
| `class ... permits` | Error | Permits requires sealed |

### 8.2: Error codes summary

| Code | When |
|---|---|
| RAV0334 | Inheriting sealed base from different file (no permits) |
| RAV0335 | Inheriting sealed base but not in permits list |
| RAV0336 | Unknown type in permits clause |
| RAV0337 | Duplicate type in permits clause |
| RAV0338 | Permits-listed type doesn't directly inherit base |
| RAV0339 | Permits clause without sealed modifier |
| RAV0340 | Non-exhaustive match on sealed hierarchy |

---

## Phase 9: Tests

### Test File: `test/Raven.CodeAnalysis.Tests/Semantics/SealedHierarchyTests.cs`

**Syntax tests:**
- Parse `sealed class Expr { }` — verify SealedKeyword in modifiers
- Parse `sealed class Expr permits Lit, Add { }` — verify PermitsClause present with 2 types

**Same-file closure (no permits):**
- Valid: sealed base + derived in same source → compiles
- Invalid: sealed base in file A, derived in file B → RAV0334

**Permits exclusivity:**
- Valid: `sealed class Expr permits Lit {}` + `class Lit : Expr {}` → compiles
- Invalid: `class NotListed : Expr {}` in same file → RAV0335
- Invalid: permits lists type that doesn't inherit → RAV0338

**Permits validation:**
- Duplicate type in permits → RAV0337
- Unknown type in permits → RAV0336
- Permits without sealed → RAV0339

**Modifier combinations:**
- `sealed abstract class` → valid, both flags set
- `sealed open class` → error (contradictory)

**Match exhaustiveness:**
- Exhaustive match on sealed hierarchy → compiles
- Non-exhaustive match → error with missing cases listed
- Match with `_` default arm → compiles

**IL emission (codegen tests):**
- Sealed hierarchy base does NOT have IL `sealed` attribute
- `[ClosedHierarchy]` attribute is present on emitted type
- Constructors are internal

**Runtime fallback:**
- Even exhaustive match emits hidden throw branch

---

## Implementation Order

1. **Phase 1** (Syntax) — Foundation, no behavioral changes
2. **Phase 3** (Diagnostics) — Define all error codes up front
3. **Phase 2** (Symbols) — Add data structures
4. **Phase 4** (Binder declaration) — Detect and register sealed hierarchies
5. **Phase 5** (Binder validation) — Enforce inheritance rules
6. **Phase 7** (IL emission) — Correct codegen
7. **Phase 6** (Exhaustiveness) — Match checking
8. **Phase 9** (Tests) — Comprehensive test suite
9. **Phase 8** (Docs) — Spec text finalization
