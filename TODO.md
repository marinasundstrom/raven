# TODO

## Semantic analysis

## Build

### Bug: Build fails without generated syntax and diagnostics
Running `dotnet build` initially produced many `CS0246` errors for missing syntax node types.
Possible solution: run `tools/NodeGenerator` and `tools/DiagnosticsGenerator` before building to generate required sources.

## Code generator
### Bug: Issue with implicit vs explicit return

This fails.

```raven
let x = if true {
       return 42
    } else {
        return ()
    }
```

With stack trace

```
Unhandled exception. System.ArgumentNullException: Value cannot be null. (Parameter 'typeSymbol')
   at Raven.CodeAnalysis.TypeSymbolExtensionsForCodeGen.GetClrType(ITypeSymbol typeSymbol, CodeGenerator codeGen) in /Users/marina/Projects/Raven/src/Raven.CodeAnalysis/TypeSymbolExtensionsForCodeGen.cs:line 14
   at Raven.CodeAnalysis.CodeGen.MethodBodyGenerator.ResolveClrType(ITypeSymbol typeSymbol) in /Users/marina/Projects/Raven/src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs:line 292
   at Raven.CodeAnalysis.CodeGen.MethodBodyGenerator.Emit() in /Users/marina/Projects/Raven/src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs:line 69
   at Raven.CodeAnalysis.CodeGen.MethodGenerator.EmitBody() in /Users/marina/Projects/Raven/src/Raven.CodeAnalysis/CodeGen/MethodGenerator.cs:line 130
```

But not this (implicit return):

```raven
let x = if true {
        42
    } else {
        ()
    }
```

Code gen emits as intended.

## Type system
1. Implement member and type lookup for constructed type symbols (`ArrayTypeSymbol`, `UnionTypeSymbol`, `NullableTypeSymbol`, `TupleTypeSymbol`, `ConstructedNamedTypeSymbol`).
2. Support multi-dimensional arrays by respecting array rank in `TypeSymbolExtensionsForCodeGen` and `ArrayTypeSymbol`.
3. Complete tuple support, including `LookupType` and `Construct` in `TupleTypeSymbol` and tuple metadata on `ConstructedNamedTypeSymbol`.
4. Improve `TypeResolver` to short-circuit resolution for built-in types and cache results.
5. Add language support for declaring generic types and functions.
