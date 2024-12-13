# Debugging

## Readable representation of the Syntax tree

You can obtain a readable representation of the syntax tree hierarchy that can be printed to the console.

Using the `PrintSyntaxTree` and `GetSyntaxTreeRepresentation` extension methods. 

### Example

Assuming you have a syntax node that represent this code:

```csharp
if (foo)  {
    return 0;
} else if (bar ) {
    return 1;
}
```

Invoking this:

```csharp
root.PrintSyntaxTree();
````

Will give you this:

```
├── CompilationUnit
│   ├── GlobalStatement
│   │   └── IfStatement
│   │       ├── IdentifierName
│   │       ├── Block
│   │       │   ├── ReturnStatement
│   │       │   │   ├── NumericLiteralExpression
│   │       └── ElseClause
│   │           └── IfStatement
│   │               ├── IdentifierName
│   │               └── Block
│   │                   ├── ReturnStatement
│   │                   │   ├── NumericLiteralExpression
```

Or if you want to include tokens to:

```csharp
root.PrintSyntaxTree(includeTokens: true)
```

Will give you this:

```
├── CompilationUnit
│   ├── GlobalStatement
│   │   └── IfStatement
│   │       ├── IfKeyword "if"
│   │       ├── OpenParenToken "("
│   │       ├── IdentifierName
│   │       │   └── IdentifierToken "foo"
│   │       ├── CloseParenToken ")"
│   │       ├── Block
│   │       │   ├── OpenBraceToken "{"
│   │       │   ├── ReturnStatement
│   │       │   │   ├── ReturnKeyword "return"
│   │       │   │   ├── NumericLiteralExpression
│   │       │   │   │   └── NumericLiteralToken "0"
│   │       │   │   └── SemicolonToken ";"
│   │       │   └── CloseBraceToken "}"
│   │       └── ElseClause
│   │           ├── ElseKeyword "else"
│   │           └── IfStatement
│   │               ├── IfKeyword "if"
│   │               ├── OpenParenToken "("
│   │               ├── IdentifierName
│   │               │   └── IdentifierToken "bar"
│   │               ├── CloseParenToken ")"
│   │               └── Block
│   │                   ├── OpenBraceToken "{"
│   │                   ├── ReturnStatement
│   │                   │   ├── ReturnKeyword "return"
│   │                   │   ├── NumericLiteralExpression
│   │                   │   │   └── NumericLiteralToken "1"
│   │                   │   └── SemicolonToken ";"
│   │                   └── CloseBraceToken "}"
│   └── EndOfFileToken ""
```

## Source generator

### Debugging

Uncommenting this line should make the debugger launch in Visual Studio:

```csharp
Debugger.Launch();
```

This occurs when the generator is running, that is, when the ``Raven.CodeAnalysis`` re-compiles:

### Inspecting generated source code

The output of the source generator is found in `Raven.CodeAnalysis/obj/Debug/net9.0/generated`