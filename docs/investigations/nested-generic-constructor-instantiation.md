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

## Plan
- [x] **Capture the failing IL and document the reproduction environment.** Complete — the sample build plus `ilspycmd` dump now live in this investigation, providing a stable baseline to compare against future fixes.
- [x] **Ensure runtime type materialization collects inherited arguments.** Complete — `ConstructedNamedTypeSymbol.GetTypeInfo` now appends the containing type’s instantiation before handing the runtime type back to codegen. This eliminated partially open runtime types but did not fix constructor binding because we are still invoking the source definition.
- [ ] **Bind object creation against substituted constructors.** Update `SymbolQuery`/`BindTypeSyntax` so nested generic type syntax (`Foo<int>.Bar`, `Outer<int>.Inner<string>`) yields a `ConstructedNamedTypeSymbol`, and ensure `BindObjectCreationExpression` (and any other constructor binders) pull constructors from that constructed symbol. The goal is for `BoundObjectCreationExpression.Symbol` to be a `SubstitutedMethodSymbol` whose `ContainingType` already reflects the closed runtime type. Instrument `SymbolQuery.LookupType` and `ConstructedNamedTypeSymbol.Constructors` as needed to guarantee this path.
- [ ] **Harden `GetClrConstructorInfo` for nested generics.** Extend `MethodSymbolExtensionsForCodeGen.GetClrConstructorInfo` so it refuses to emit `SourceMethodSymbol` instances whose containing type is constructed. When a definition slips through (e.g., because a metadata symbol bypasses substitution), wrap it in a `SubstitutedMethodSymbol`/`ConstructedMethodSymbol` that points at the constructed containing type before resolving the CLR constructor.
- [ ] **Add regression coverage.** Introduce a targeted IL or reflection-based test that instantiates `Foo<int>.Bar()` and `Outer<int>.Inner<string>()`, then verifies the emitted constructor references include both outer and inner type arguments. Consider expanding the existing `SymbolQuery` or `ConstructedNamedTypeSymbol` tests to cover this scenario and/or add a `CodeGen` test that inspects `MethodBodyGenerator` output.
- [ ] **Validate the samples and full test suite.** Rebuild `samples/generics/nested-classes.rav` and run `dotnet test test/Raven.CodeAnalysis.Tests /property:WarningLevel=0` to ensure the fix integrates cleanly with the rest of the compiler.
