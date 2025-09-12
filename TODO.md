# TODO

## Semantic analysis

### Bug: Invoking instance method as static reports wrong diagnostics
Calling an instance method with a static call produces an unexpected `RAV0030` and misses the expected `RAV0117` diagnostic.
Possible solution: adjust the binder's diagnostic logic so that static calls to instance members only report `RAV0117`.

### Bug: Spread operator in array literals doesn't enumerate
`CollectionExpressionTests.ArrayCollectionExpressions_SpreadEnumerates` fails because spread values aren't iterated.
Possible solution: update collection expression binding/generation to expand spread expressions when building arrays.

### Bug: Multi-line comment trivia parsing fails
`MultiLineCommentTrivia_IsLeadingTriviaOfToken` throws `InvalidOperationException` when parsing multi-line comments.
Possible solution: ensure the parser includes multi-line comment trivia in the token's leading trivia list.

### Bug: Tuple alias directive requires initialization
`AliasDirective_UsesAlias_Tuple` reports `RAV0166` for an uninitialized tuple alias variable.
Possible solution: relax initialization checks for tuple aliases so using an alias doesn't require a separate initializer.

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
