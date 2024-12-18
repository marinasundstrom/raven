# Debugging

## Readable representation of the Syntax tree

You can obtain a readable representation of the syntax tree hierarchy that can be printed to the console.

Using the `PrintSyntaxTree` and `GetSyntaxTreeRepresentation` extension methods. 

### Example

Assuming you have a syntax node that represent this code:

```csharp
  var sourceCode = """
            if (foo)  {
                return 0;
            } else if (bar ) {
                return 1;
            }
            """;
```

Parse the source code into a syntax tree:

```csharp
var syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceCode);

var root = syntaxTree.GetRoot();
```

Invoking this method that is available on every `SyntaxNode`:


```csharp
root.PrintSyntaxTree(includeTrivia: true, includeSpans: true, includeLocation: true);
````

Will give you this:

```
CompilationUnit [0..64] (1:1)
├── GlobalStatement [0..64] (1:1)
│   └── IfStatement [0..64] (1:1)
│       ├── IfKeyword "if" [0..2] (1:1)
│       │   [Trailing Trivia] WhitespaceTrivia: " "
│       ├── OpenParenToken "(" [3..4] (1:4)
│       ├── IdentifierName [4..7] (1:5)
│       │   └── IdentifierToken "foo" [4..7] (1:5)
│       ├── CloseParenToken ")" [7..8] (1:8)
│       │   [Trailing Trivia] WhitespaceTrivia: "  "
│       ├── Block [10..30] (1:11)
│       │   ├── OpenBraceToken "{" [10..11] (1:11)
│       │   │   [Trailing Trivia] CarriageReturnLineFeedTrivia: "\r\n"
│       │   ├── ReturnStatement [17..32] (2:5)
│       │   │   │   [Leading Trivia] WhitespaceTrivia: "    "
│       │   │   ├── ReturnKeyword "return" [17..23] (2:5)
│       │   │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │   │   ├── NumericLiteralExpression [24..25] (2:12)
│       │   │   │   └── NumericLiteralToken "0" [24..25] (2:12)
│       │   │   └── SemicolonToken ";" [25..26] (2:13)
│       │   │       [Trailing Trivia] CarriageReturnLineFeedTrivia: "\r\n"
│       │   └── CloseBraceToken "}" [28..29] (3:1)
│       │       [Trailing Trivia] WhitespaceTrivia: " "
│       └── ElseClause [30..64] (3:3)
│           ├── ElseKeyword "else" [30..34] (3:3)
│           │   [Trailing Trivia] WhitespaceTrivia: " "
│           └── IfStatement [35..64] (3:8)
│               ├── IfKeyword "if" [35..37] (3:8)
│               │   [Trailing Trivia] WhitespaceTrivia: " "
│               ├── OpenParenToken "(" [38..39] (3:11)
│               ├── IdentifierName [39..43] (3:12)
│               │   └── IdentifierToken "bar" [39..42] (3:12)
│               │       [Trailing Trivia] WhitespaceTrivia: " "
│               ├── CloseParenToken ")" [43..44] (3:16)
│               │   [Trailing Trivia] WhitespaceTrivia: " "
│               └── Block [45..64] (3:18)
│                   ├── OpenBraceToken "{" [45..46] (3:18)
│                   │   [Trailing Trivia] CarriageReturnLineFeedTrivia: "\r\n"
│                   ├── ReturnStatement [52..67] (4:5)
│                   │   │   [Leading Trivia] WhitespaceTrivia: "    "
│                   │   ├── ReturnKeyword "return" [52..58] (4:5)
│                   │   │   [Trailing Trivia] WhitespaceTrivia: " "
│                   │   ├── NumericLiteralExpression [59..60] (4:12)
│                   │   │   └── NumericLiteralToken "1" [59..60] (4:12)
│                   │   └── SemicolonToken ";" [60..61] (4:13)
│                   │       [Trailing Trivia] CarriageReturnLineFeedTrivia: "\r\n"
│                   └── CloseBraceToken "}" [63..64] (5:1)
└── EndOfFileToken "" [64..64] (5:2)
```

### Syntax tree to source code

You can output the syntax tree into source code [again]. This is useful for debugging when building your own trees programmatically.

This method is available on every `SyntaxNode`:

```csharp
var str = syntaxNode.ToFullString();
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