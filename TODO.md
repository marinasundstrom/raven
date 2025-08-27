# TODO

## Semantic analysis

### Bug: Allows for multiple local functions with same signature, and crashes

You should not be able to define another local function in global scope using the same signature as an existing one.

Right now, the compiler crashes at code generation:

```raven
func test () {}
func test () {}
```

There should be a diagnostic for the second one telling that there already is a function with the same signature.

## Code generator

### Bug: Box values in type unions when directly returning from a method

Value types in a union should be boxed when returning since they are passed as `object`.

```raven
func test(flag: bool) -> int | () {
    if flag {
        return 42
    } else {
        return ()
    }
}
```

This works as intended when assigning a variable:

```
let x = if true {
        42
    } else {
        ()
    }
```

The 42 gets boxed.

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