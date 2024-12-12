# Debugging

## Readable representation of the Syntax tree

You can obtain a readable representation of the syntax tree hierarchy that can be printed to the console.

Using the `PrintSyntaxTree` and `GetSyntaxTreeRepresentation` extension methods. 

### Example

Assume you have a syntax node that represent this code:

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
CompilationUnit
  GlobalStatement
    IfStatement
      IdentifierName
      Block
        ReturnStatement
          IdentifierName
      ElseClause
        IfStatement
          IdentifierName
          Block
            ReturnStatement
              IdentifierName
```

Or if you want to include tokens to:

```csharp
root.PrintSyntaxTree(includeTokens: true)
```

Will give you this:

```
CompilationUnit
  GlobalStatement
    IfStatement
      IfKeyword
      OpenParenToken
      IdentifierName
        IdentifierToken
      CloseParenToken
      Block
        OpenBraceToken
        ReturnStatement
          ReturnKeyword
          IdentifierName
            NumericLiteralToken
          SemicolonToken
        CloseBraceToken
      ElseClause
        ElseKeyword
        IfStatement
          IfKeyword
          OpenParenToken
          IdentifierName
            IdentifierToken
          CloseParenToken
          Block
            OpenBraceToken
            ReturnStatement
              ReturnKeyword
              IdentifierName
                NumericLiteralToken
              SemicolonToken
            CloseBraceToken
  EndOfFileToken
```