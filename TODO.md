# TODO

## Semantic analysis

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
