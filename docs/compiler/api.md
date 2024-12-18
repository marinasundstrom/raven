# Compiler API

This documents is meant to show the API of the Raven compiler.

## Syntax analysis

In this section we will walk through how to do basic syntax analysis.

### Parsing source code into a syntax tree

Assuming you want to parse source code into a syntax tree, this example shows you how to:


```csharp
let x : int = 2

if (x > 2) {
    return (6 + 2) * 2;
} else
    return foo.bar(2)
        .GetId(1, z + 2, "Foo");
```

This is how you parse:

```csharp
var syntaxTree = SyntaxTree.ParseText(" ... ");

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
CompilationUnit [0..116] (1:1)
├── GlobalStatement [0..17] (1:1)
│   └── LocalDeclaration [0..17] (1:1)
│       ├── VariableDeclaration [0..17] (1:1)
│       │   ├── LetKeyword "let" [0..3] (1:1)
│       │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │   └── VariableDeclarator [4..17] (1:5)
│       │       ├── IdentifierName [4..6] (1:5)
│       │       │   └── IdentifierToken "x" [4..5] (1:5)
│       │       │       [Trailing Trivia] WhitespaceTrivia: " "
│       │       ├── TypeAnnotation [6..12] (1:7)
│       │       │   ├── ColonToken ":" [6..7] (1:7)
│       │       │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │       │   └── IdentifierName [8..12] (1:9)
│       │       │       └── IntKeyword "int" [8..11] (1:9)
│       │       │           [Trailing Trivia] WhitespaceTrivia: " "
│       │       └── EqualsValueClause [12..17] (1:13)
│       │           ├── EqualsToken "=" [12..13] (1:13)
│       │           │   [Trailing Trivia] WhitespaceTrivia: " "
│       │           └── NumericLiteralExpression [14..17] (1:15)
│       │               └── NumericLiteralToken "2" [14..15] (1:15)
│       │                   [Trailing Trivia] EndOfLineTrivia: "\n"
│       │                   [Trailing Trivia] EndOfLineTrivia: "\n"
│       └── SemicolonToken (Missing) "" [17..17] (3:1)
├── GlobalStatement [17..116] (3:1)
│   └── IfStatement [17..116] (3:1)
│       ├── IfKeyword "if" [17..19] (3:1)
│       │   [Trailing Trivia] WhitespaceTrivia: " "
│       ├── OpenParenToken "(" [20..21] (3:4)
│       ├── GreaterThanExpression [21..27] (3:5)
│       │   ├── IdentifierName [21..23] (3:5)
│       │   │   └── IdentifierToken "x" [21..22] (3:5)
│       │   │       [Trailing Trivia] WhitespaceTrivia: " "
│       │   ├── GreaterThanToken ">" [23..24] (3:7)
│       │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │   └── NumericLiteralExpression [25..27] (3:9)
│       │       └── NumericLiteralToken "2" [25..26] (3:9)
│       │           [Trailing Trivia] WhitespaceTrivia: " "
│       ├── CloseParenToken ")" [27..28] (3:11)
│       │   [Trailing Trivia] WhitespaceTrivia: " "
│       ├── Block [29..57] (3:13)
│       │   ├── OpenBraceToken "{" [29..30] (3:13)
│       │   │   [Trailing Trivia] EndOfLineTrivia: "\n"
│       │   ├── ReturnStatement [35..59] (4:5)
│       │   │   │   [Leading Trivia] WhitespaceTrivia: "    "
│       │   │   ├── ReturnKeyword "return" [35..41] (4:5)
│       │   │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │   │   ├── MultiplyExpression [42..53] (4:12)
│       │   │   │   ├── ParenthesizedExpression [42..50] (4:12)
│       │   │   │   │   ├── OpenParenToken "(" [42..43] (4:12)
│       │   │   │   │   ├── AddExpression [43..48] (4:13)
│       │   │   │   │   │   ├── NumericLiteralExpression [43..45] (4:13)
│       │   │   │   │   │   │   └── NumericLiteralToken "6" [43..44] (4:13)
│       │   │   │   │   │   │       [Trailing Trivia] WhitespaceTrivia: " "
│       │   │   │   │   │   ├── PlusToken "+" [45..46] (4:15)
│       │   │   │   │   │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │   │   │   │   │   └── NumericLiteralExpression [47..48] (4:17)
│       │   │   │   │   │       └── NumericLiteralToken "2" [47..48] (4:17)
│       │   │   │   │   └── CloseParenToken ")" [48..49] (4:18)
│       │   │   │   │       [Trailing Trivia] WhitespaceTrivia: " "
│       │   │   │   ├── StarToken "*" [50..51] (4:20)
│       │   │   │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │   │   │   └── NumericLiteralExpression [52..53] (4:22)
│       │   │   │       └── NumericLiteralToken "2" [52..53] (4:22)
│       │   │   └── SemicolonToken ";" [53..54] (4:23)
│       │   │       [Trailing Trivia] EndOfLineTrivia: "\n"
│       │   └── CloseBraceToken "}" [55..56] (5:1)
│       │       [Trailing Trivia] WhitespaceTrivia: " "
│       └── ElseClause [57..116] (5:3)
│           ├── ElseKeyword "else" [57..61] (5:3)
│           │   [Trailing Trivia] EndOfLineTrivia: "\n"
│           └── ReturnStatement [66..120] (6:5)
│               │   [Leading Trivia] WhitespaceTrivia: "    "
│               ├── ReturnKeyword "return" [66..72] (6:5)
│               │   [Trailing Trivia] WhitespaceTrivia: " "
│               ├── InvocationExpression [73..115] (6:12)
│               │   ├── SimpleMemberAccessExpression [73..98] (6:12)
│               │   │   ├── InvocationExpression [73..84] (6:12)
│               │   │   │   ├── SimpleMemberAccessExpression [73..80] (6:12)
│               │   │   │   │   ├── IdentifierName [73..76] (6:12)
│               │   │   │   │   │   └── IdentifierToken "foo" [73..76] (6:12)
│               │   │   │   │   ├── DotToken "." [76..77] (6:15)
│               │   │   │   │   └── IdentifierName [77..80] (6:16)
│               │   │   │   │       └── IdentifierToken "bar" [77..80] (6:16)
│               │   │   │   └── ArgumentList [80..84] (6:19)
│               │   │   │       ├── OpenParenToken "(" [80..81] (6:19)
│               │   │   │       ├── Argument [81..82] (6:20)
│               │   │   │       │   └── NumericLiteralExpression [81..82] (6:20)
│               │   │   │       │       └── NumericLiteralToken "2" [81..82] (6:20)
│               │   │   │       └── CloseParenToken ")" [82..83] (6:21)
│               │   │   │           [Trailing Trivia] EndOfLineTrivia: "\n"
│               │   │   │   [Leading Trivia] WhitespaceTrivia: "        "
│               │   │   ├── DotToken "." [92..93] (7:9)
│               │   │   └── IdentifierName [93..98] (7:10)
│               │   │       └── IdentifierToken "GetId" [93..98] (7:10)
│               │   └── ArgumentList [98..115] (7:15)
│               │       ├── OpenParenToken "(" [98..99] (7:15)
│               │       ├── Argument [99..100] (7:16)
│               │       │   └── NumericLiteralExpression [99..100] (7:16)
│               │       │       └── NumericLiteralToken "1" [99..100] (7:16)
│               │       ├── CommaToken "," [100..101] (7:17)
│               │       │   [Trailing Trivia] WhitespaceTrivia: " "
│               │       ├── Argument [102..107] (7:19)
│               │       │   └── AddExpression [102..107] (7:19)
│               │       │       ├── IdentifierName [102..104] (7:19)
│               │       │       │   └── IdentifierToken "z" [102..103] (7:19)
│               │       │       │       [Trailing Trivia] WhitespaceTrivia: " "
│               │       │       ├── PlusToken "+" [104..105] (7:21)
│               │       │       │   [Trailing Trivia] WhitespaceTrivia: " "
│               │       │       └── NumericLiteralExpression [106..107] (7:23)
│               │       │           └── NumericLiteralToken "2" [106..107] (7:23)
│               │       ├── CommaToken "," [107..108] (7:24)
│               │       │   [Trailing Trivia] WhitespaceTrivia: " "
│               │       ├── Argument [109..114] (7:26)
│               │       │   └── StringLiteralExpression [109..114] (7:26)
│               │       │       └── StringLiteralToken ""Foo"" [109..114] (7:26)
│               │       └── CloseParenToken ")" [114..115] (7:31)
│               └── SemicolonToken ";" [115..116] (7:32)
└── EndOfFileToken "" [116..116] (7:33)
```