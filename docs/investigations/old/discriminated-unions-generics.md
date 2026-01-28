# Generic discriminated union pattern investigation

## Reproduction

Building `samples/discriminated-unions-generics.rav` succeeds, but running the emitted DLL fails with `BadImageFormatException` before the first `Console.WriteLine` executes.【F:samples/discriminated-unions-generics.rav†L1-L24】【cb7ab9†L1-L6】

## Findings

* **Patterns drop the scrutinee's type arguments.** `BindCasePattern` discovers the containing union from the scrutinee but keeps the unconstructed symbol returned by `TryGetDiscriminatedUnion`, so the bound case uses the generic definition instead of `Result<int, string>` or another constructed instance.【F:src/Raven.CodeAnalysis/BoundTree/BoundIsPatternExpression.cs†L512-L593】
* **Code generation replays the open generic types.** Because the bound pattern stores the open symbols, `EmitPattern` resolves the CLR types for those definitions and emits `isinst`/`unbox.any` instructions against ``Result`1`` and ``Result`1/Ok`` instead of their instantiated forms. The resulting IL tries to unbox a `Result<int, string>` scrutinee as ``Result`1``, which the runtime rejects and surfaces as the `BadImageFormatException`.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1135-L1180】【cc434e†L128-L190】
* **Constructed union symbols should behave like any other generic type.** When a discriminated union is closed over concrete type arguments, the `Cases` (and any other shape enumerations) should surface the correspondingly closed case symbols. This likely overlaps with the infrastructure used for nested types that capture enclosing generic parameters and would let consumers operate on `Result<int, string>.Ok` instead of the open definition.
* **Case detection must reject non-union symbols.** Constructed union symbols implement both the union and case interfaces, so `TryGetDiscriminatedUnion(Case)` must also check `IsDiscriminatedUnion`/`IsDiscriminatedUnionCase` to avoid treating a constructed union as a case symbol during conversions and crashing binding.【F:src/Raven.CodeAnalysis/Symbols/DiscriminatedUnionSymbolExtensions.cs†L1-L38】
* **Case type parameters duplicated the containing union's generics.** Giving cases their own copy of the union's type parameters produced nested types like `Result`1/Ok`1<!T, !1>` whose `TryGet*` members referenced a non-existent `!1` generic parameter, triggering `BadImageFormatException` at runtime. Cases need to rely on the containing union's generic parameters instead of declaring their own to keep the nested signatures valid.【F:src/Raven.CodeAnalysis/SemanticModel.cs†L916-L1034】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1118-L1193】【F:samples/discriminated-unions-generics.rav†L1-L24】【103139†L1-L4】

## Plan

1. ✅ **Carry constructed union/case symbols through pattern binding.** Constructed generic unions/cases now keep their discriminated union identity via `ConstructedNamedTypeSymbol`, exposing closed case symbols, storage fields, and constructor parameters for substituted type arguments.
2. ✅ **Instantiate CLR types with the active type arguments.** `EmitPattern` now instantiates the constructed CLR types so pattern codegen targets `Result<int, string>`/`Result<int, string>.Ok` instead of open generic definitions.
3. ✅ **Align case generic arity with the containing union.** Cases rely on the containing union's generic parameters instead of declaring their own, keeping the nested case types well-formed while still allowing constructed unions to substitute case members.
4. ✅ **Add regression coverage.** Added a semantics regression to decode `Result<int, string>` TypeSpecs and ensure they include constructed case types, plus a sample execution test to keep `discriminated-unions-generics.rav` running end-to-end.
