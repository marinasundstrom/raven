# API

This documents is meant to show the API of the Raven compiler.

## Syntax analysis

In this section we will walk through how to do basic syntax analysis.

### Parsing source code into a syntax tree

Assuming you want to parse source code into a syntax tree, this example shows you how to:

```csharp
  var code = """
            if (foo)  {
                return 0;
            } else if (bar ) {
                return 1;
            }
            """;

var syntaxTree = SyntaxTree.ParseText(code);

// Will get you the CompilationUnit (root syntax node)
var root = syntaxTree.GetRoot();
```

### Turn syntax tree into source code

If you want to turn the tree into source code:

```
var sourceCode = root.ToFullString();
```

### Print the syntax hierarchy

You can also print the node hierarchy to the console:

```
root.PrintSyntaxTree(includeTokens: true)
```

The printed syntax tree will look similar to this:

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