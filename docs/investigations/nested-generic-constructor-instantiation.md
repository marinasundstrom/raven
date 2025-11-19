# Nested generic object creation emits open constructors

## Problem statement
Constructing a nested type through a closed generic receiver (for example `Foo<int>.Bar`) emits a `newobj` instruction that targets the open constructor definition instead of the constructed nested type. As a result, the IL looks like `newobj instance void Foo`1/Bar::.ctor()` even though the local type is `Foo`1/Bar<int32>`. The issue also shows up with deeper nesting such as `Outer<int>.Inner<string>` where the instantiated constructor should mention both `<int32, string>` in metadata but only supplies one argument for the inner generic arity.

## Reproduction steps
1. Regenerate the syntax, bound-node, and diagnostics sources so the solution builds with the current generators.
2. Compile `samples/generics/nested-classes.rav` into a temporary assembly.
3. Disassemble the resulting `nested.dll` with `ilspycmd` to inspect the emitted IL for `Program.Main`.

```bash
(cd src/Raven.CodeAnalysis/Syntax && dotnet run --project ../../../tools/NodeGenerator -- -f)
(cd src/Raven.CodeAnalysis      && dotnet run --project ../../tools/BoundNodeGenerator -- -f)
(cd src/Raven.CodeAnalysis      && dotnet run --project ../../tools/DiagnosticsGenerator -- -f)
dotnet run --project src/Raven.Compiler -- samples/generics/nested-classes.rav -o nested.dll
ilspycmd nested.dll -t Program -il
```

The IL dump shows that both locals are constructed types, yet the `newobj` opcodes target the open generic definitions, so the runtime never receives the concrete type arguments:

```
.locals init (
    [0] class Foo`1/Bar<int32>,
    [1] class Outer`1/Inner`1<int32, int32>
)

IL_0000: newobj instance void Foo`1/Bar::.ctor()
IL_0005: stloc.0
IL_0006: newobj instance void class Outer`1/Inner`1<int32>::.ctor()
IL_000b: stloc.1
```

## Analysis
* `BoundObjectCreationExpression` passes the constructor symbol it bound directly into `MethodSymbolExtensionsForCodeGen.GetClrConstructorInfo` before emitting `newobj`. If the symbol is still the original source definition, `GetClrConstructorInfo` returns the raw constructor builder so the emitter never tells the CLR about the containing type’s type arguments.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1604-L1649】【F:src/Raven.CodeAnalysis/MethodSymbolExtensionsForCodeGen.cs†L42-L122】
* `ConstructedNamedTypeSymbol` substitutes nested types, but it only substitutes their members when the binder queries the nested type itself. During `Foo<int>.Bar()` binding the type expression is constructed, yet the lookup that resolves `.ctor` still returns the original `SourceMethodSymbol` instead of wrapping it inside `SubstitutedMethodSymbol`, so code generation cannot recover the substituted containing type despite having it in the bound expression.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L215-L330】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L472-L688】
* Because constructor substitution never happens, `ConstructedNamedTypeSymbol.GetAllTypeArguments` has no chance to flow outer arguments to inner constructors, and the nested runtime types remain partially open when the backend instantiates them.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L125-L188】

## Status update (2025-11-18)
After flowing inherited type arguments through `ConstructedNamedTypeSymbol.GetTypeInfo`, the emitted IL still targets the open nested constructors. Re-running the reproduction commands—after regenerating syntax/bound/diagnostic sources, compiling the `generics/nested-classes` sample, and disassembling the output via `ilspycmd nested.dll -t Program -il`—shows that `newobj` continues to reference `Foo&#96;1/Bar::.ctor()` instead of the closed `Foo&#96;1/Bar<int32>::.ctor()`. The deeper instantiation `Outer<int>.Inner<string>()` also emits `newobj instance void class Outer&#96;1/Inner&#96;1<int32, int32>::.ctor()` because the constructor only sees the inner type parameter.

This confirms that the constructor symbol passed to `BoundObjectCreationExpression` is still the original source definition, so the backend never learns about the constructed containing type despite `ConstructedNamedTypeSymbol` exposing the correct runtime instantiation.

## Status update (2025-11-18, constructor binding)
`BlockBinder.BindObjectCreationExpression` now wraps resolved constructors in `SubstitutedMethodSymbol` whenever the instantiated type is a `ConstructedNamedTypeSymbol`, ensuring `BoundObjectCreationExpression` records the constructed containing type. The constructor initializer binder uses the same helper so chained `base(...)` calls inside nested generics participate in the substitution pipeline. New semantic coverage in `ObjectCreationBindingTests` binds `Foo<int>.Bar()` and `Outer<int>.Inner<string>()`, asserting that each `BoundObjectCreationExpression.Constructor` is a substituted member belonging to the constructed nested type.

## Status update (2025-11-18, constructor codegen guard)
`MethodSymbolExtensionsForCodeGen.GetClrConstructorInfo` now calls a local `EnsureConstructedConstructor` helper before touching the runtime cache, forcing any constructor whose containing type is a `ConstructedNamedTypeSymbol` to flow through `SubstitutedMethodSymbol` even if the binder forgot to substitute it. The helper covers both source and metadata symbols, preventing emission from accidentally asking for a constructor builder on the open definition. Rebuilding `samples/generics/nested-classes.rav` and dumping `nested.dll` via `ilspycmd -t Program -il` still shows `newobj` opcodes that target the open nested constructors, so the remaining gaps sit in how `GetClrConstructorInfo` materializes runtime constructors for nested constructed types rather than in how the binder supplies them.

## Plan
- [x] **Capture the failing IL and document the reproduction environment.** Complete — the sample build plus `ilspycmd` dump now live in this investigation, providing a stable baseline to compare against future fixes.
- [x] **Ensure runtime type materialization collects inherited arguments.** Complete — `ConstructedNamedTypeSymbol.GetTypeInfo` now appends the containing type’s instantiation before handing the runtime type back to codegen. This eliminated partially open runtime types but did not fix constructor binding because we are still invoking the source definition.
- [x] **Bind object creation against substituted constructors.** Both object creation and constructor initializers now reuse a helper that wraps resolved constructors in `SubstitutedMethodSymbol` whenever the target type is a `ConstructedNamedTypeSymbol`, so `BoundObjectCreationExpression.Symbol` already carries the constructed containing type. The new `ObjectCreationBindingTests` lock this in place.
- [x] **Harden `GetClrConstructorInfo` for nested generics.** `MethodSymbolExtensionsForCodeGen.GetClrConstructorInfo` now normalizes every constructor symbol through a local helper that returns a `SubstitutedMethodSymbol` whenever the containing type is constructed, ensuring runtime emission can never reach the CLR using a definition-only constructor handle again.
## Status update (2025-11-19, plan re-evaluation)
After refreshing all generators, recompiling `samples/generics/nested-classes.rav`, and disassembling the resulting `nested.dll` (`DOTNET_ROLL_FORWARD=Major ilspycmd nested.dll -t Program -il`), the emitted IL still targets the open nested constructors. Worse, the supposedly closed nested instantiation `Outer<int>.Inner<string>()` now shows both metadata arguments bound to `int32`, proving that the nested type’s explicit `<string>` argument is being dropped somewhere inside the substitution pipeline:

```
.locals init (
    [0] class Foo`1/Bar<int32>,
    [1] class Outer`1/Inner`1<int32, int32>
)

IL_0000: newobj instance void Foo`1/Bar::.ctor()
IL_0005: stloc.0
IL_0006: newobj instance void class Outer`1/Inner`1<int32, int32>::.ctor()
IL_000b: stloc.1
```

Because binder substitution and constructor-codegen guards are already in place, the remaining bug has to live in how we construct nested `INamedTypeSymbol`s (likely `ConstructedNamedTypeSymbol.SubstituteNamedType`) and how those constructed types hand their argument lists to runtime type creation.

## Plan
- [x] **Capture the failing IL and document the reproduction environment.** Complete — the sample build plus `ilspycmd` dump now live in this investigation, providing a stable baseline to compare against future fixes.
- [x] **Ensure runtime type materialization collects inherited arguments.** Complete — `ConstructedNamedTypeSymbol.GetTypeInfo` now appends the containing type’s instantiation before handing the runtime type back to codegen. This eliminated partially open runtime types but did not fix constructor binding because we are still invoking the source definition.
- [x] **Bind object creation against substituted constructors.** Both object creation and constructor initializers now reuse a helper that wraps resolved constructors in `SubstitutedMethodSymbol` whenever the target type is a `ConstructedNamedTypeSymbol`, so `BoundObjectCreationExpression.Symbol` already carries the constructed containing type. The new `ObjectCreationBindingTests` lock this in place.
- [x] **Harden `GetClrConstructorInfo` for nested generics.** `MethodSymbolExtensionsForCodeGen.GetClrConstructorInfo` now normalizes every constructor symbol through a local helper that returns a `SubstitutedMethodSymbol` whenever the containing type is constructed, ensuring runtime emission can never reach the CLR using a definition-only constructor handle again.
- [ ] **Diagnose nested type argument propagation.** Instrument or unit-test `ConstructedNamedTypeSymbol.SubstituteNamedType` (and the `SymbolQuery` paths that call it) to prove whether `Outer<int>.Inner<string>` ever builds a constructed symbol whose `TypeArguments` are `[int, string]`; this will reveal whether we are dropping explicit inner arguments while inheriting the outer instantiation.
- [ ] **Fix constructed nested type materialization.** Once the broken substitution site is identified, update the nested-type construction logic so the explicit inner arguments survive through `GetAllTypeArguments()`/`GetTypeInfo()` and ensure the resulting runtime types surface the correct generic argument ordering.
- [ ] **Add regression coverage.** Introduce a targeted IL or reflection-based test that instantiates `Foo<int>.Bar()` and `Outer<int>.Inner<string>()`, then verifies the emitted constructor references include both outer and inner type arguments. Consider expanding the existing `SymbolQuery` or `ConstructedNamedTypeSymbol` tests to cover this scenario and/or add a `CodeGen` test that inspects `MethodBodyGenerator` output.
- [ ] **Validate the samples and full test suite.** Rebuild `samples/generics/nested-classes.rav` and run `dotnet test test/Raven.CodeAnalysis.Tests /property:WarningLevel=0` to ensure the fix integrates cleanly with the rest of the compiler.
