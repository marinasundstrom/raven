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

## Plan
1. **Ensure object creation binds constructors against the constructed nested type.** When `Foo<int>.Bar` is the receiver, the binder should pick the nested type’s constructors from the constructed symbol so `BoundObjectCreationExpression.Symbol` becomes a `SubstitutedMethodSymbol`. This guarantees code generation sees the instantiated containing type without special cases in the backend.
2. **Update constructor resolution to require closed containing types.** Extend `MethodSymbolExtensionsForCodeGen.GetClrConstructorInfo` so it asserts that any constructor belonging to a constructed nested type is wrapped (either `SubstitutedMethodSymbol` or `ConstructedMethodSymbol`). If a raw definition sneaks through, convert it into a substituted form before asking for `ConstructorInfo`.
3. **Add regression coverage.** Add a semantic or code generation test that instantiates `Foo<int>.Bar()` and `Outer<int>.Inner<string>()` and inspects the resulting IL (or reflection metadata) to ensure the constructors are invoked on `Foo<int>.Bar<int>` and `Outer<int>.Inner<int,string>`.
4. **Validate samples and the full test suite.** Rebuild `samples/generics/nested-classes.rav` and run the compiler test suite to ensure the nested-generic fix does not regress other emission paths.
