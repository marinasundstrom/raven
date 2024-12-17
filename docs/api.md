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
root.PrintSyntaxTree(includeTrivia: true, includeSpans: true, includeLocation: true);
```

The printed syntax tree will look similar to this:

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
│       │   │   └── [Trailing Trivia] CarriageReturnLineFeedTrivia: "\r\n"
│       │   └── CloseBraceToken "}" [28..29] (3:1)
│       │   └── [Trailing Trivia] WhitespaceTrivia: " "
│       └── ElseClause [30..64] (3:3)
│           ├── ElseKeyword "else" [30..34] (3:3)
│           │   [Trailing Trivia] WhitespaceTrivia: " "
│           └── IfStatement [35..64] (3:8)
│               ├── IfKeyword "if" [35..37] (3:8)
│               │   [Trailing Trivia] WhitespaceTrivia: " "
│               ├── OpenParenToken "(" [38..39] (3:11)
│               ├── IdentifierName [39..43] (3:12)
│               │   └── IdentifierToken "bar" [39..42] (3:12)
│               │   └── [Trailing Trivia] WhitespaceTrivia: " "
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
│                   │   └── [Trailing Trivia] CarriageReturnLineFeedTrivia: "\r\n"
│                   └── CloseBraceToken "}" [63..64] (5:1)
└── EndOfFileToken "" [64..64] (5:2)
```