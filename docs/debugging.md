# Debugging

## Readable representation of the Syntax tree

You can obtain a readable representation of the syntax tree hierarchy that can be printed to the console.

Using the `PrintSyntaxTree` and `GetSyntaxTreeRepresentation` extension methods. 

### Example

Assuming you have a syntax node that represent this code:

```csharp
let x : int = 2

if (x > 2) {
    return (6 + 2) * 2;
} else
    return foo.bar(2)
        .GetId(1, z + 2, "Foo");
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
CompilationUnit [0..115] (1:1)
├── GlobalStatement [0..17] (1:1)
│   └── LocalDeclaration [0..17] (1:1)
│       ├── VariableDeclaration [0..17] (1:1)
│       │   ├── LetKeyword "let" [0..3] (1:1)
│       │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │   └── VariableDeclarator [4..17] (1:5)
│       │       ├── IdentifierName [4..6] (1:5)
│       │       │   └── IdentifierToken "x" [4..5] (1:5)
│       │       │   └── [Trailing Trivia] WhitespaceTrivia: " "
│       │       ├── TypeAnnotation [6..12] (1:7)
│       │       │   ├── ColonToken ":" [6..7] (1:7)
│       │       │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │       │   └── IdentifierName [8..12] (1:9)
│       │       │       └── IntKeyword "int" [8..11] (1:9)
│       │       │       └── [Trailing Trivia] WhitespaceTrivia: " "
│       │       └── EqualsValueClause [12..17] (1:13)
│       │           ├── EqualsToken "=" [12..13] (1:13)
│       │           │   [Trailing Trivia] WhitespaceTrivia: " "
│       │           └── NumericLiteralExpression [14..17] (1:15)
│       │               └── NumericLiteralToken "2" [14..15] (1:15)
│       │               └── [Trailing Trivia] EndOfLineTrivia: "\n"
│       │               └── [Trailing Trivia] EndOfLineTrivia: "\n"
│       └── SemicolonToken (Missing) "" [17..17] (3:1)
├── GlobalStatement [17..115] (3:1)
│   └── IfStatement [17..115] (3:1)
│       ├── IfKeyword "if" [17..19] (3:1)
│       │   [Trailing Trivia] WhitespaceTrivia: " "
│       ├── OpenParenToken "(" [20..21] (3:4)
│       ├── GreaterThanExpression [21..26] (3:5)
│       │   ├── IdentifierName [21..23] (3:5)
│       │   │   └── IdentifierToken "x" [21..22] (3:5)
│       │   │   └── [Trailing Trivia] WhitespaceTrivia: " "
│       │   ├── GreaterThanToken ">" [23..24] (3:7)
│       │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │   └── NumericLiteralExpression [25..26] (3:9)
│       │       └── NumericLiteralToken "2" [25..26] (3:9)
│       ├── CloseParenToken ")" [26..27] (3:10)
│       │   [Trailing Trivia] WhitespaceTrivia: " "
│       ├── Block [28..56] (3:12)
│       │   ├── OpenBraceToken "{" [28..29] (3:12)
│       │   │   [Trailing Trivia] EndOfLineTrivia: "\n"
│       │   │   [Trailing Trivia] WhitespaceTrivia: "    "
│       │   ├── ReturnStatement [34..54] (4:5)
│       │   │   ├── ReturnKeyword "return" [34..40] (4:5)
│       │   │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │   │   ├── MultiplyExpression [41..52] (4:12)
│       │   │   │   ├── ParenthesizedExpression [41..49] (4:12)
│       │   │   │   │   ├── OpenParenToken "(" [41..42] (4:12)
│       │   │   │   │   ├── AddExpression [42..47] (4:13)
│       │   │   │   │   │   ├── NumericLiteralExpression [42..44] (4:13)
│       │   │   │   │   │   │   └── NumericLiteralToken "6" [42..43] (4:13)
│       │   │   │   │   │   │   └── [Trailing Trivia] WhitespaceTrivia: " "
│       │   │   │   │   │   ├── PlusToken "+" [44..45] (4:15)
│       │   │   │   │   │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │   │   │   │   │   └── NumericLiteralExpression [46..47] (4:17)
│       │   │   │   │   │       └── NumericLiteralToken "2" [46..47] (4:17)
│       │   │   │   │   └── CloseParenToken ")" [47..48] (4:18)
│       │   │   │   │   └── [Trailing Trivia] WhitespaceTrivia: " "
│       │   │   │   ├── StarToken "*" [49..50] (4:20)
│       │   │   │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │   │   │   └── NumericLiteralExpression [51..52] (4:22)
│       │   │   │       └── NumericLiteralToken "2" [51..52] (4:22)
│       │   │   └── SemicolonToken ";" [52..53] (4:23)
│       │   │   └── [Trailing Trivia] EndOfLineTrivia: "\n"
│       │   └── CloseBraceToken "}" [54..55] (5:1)
│       │   └── [Trailing Trivia] WhitespaceTrivia: " "
│       └── ElseClause [56..115] (5:3)
│           ├── ElseKeyword "else" [56..60] (5:3)
│           │   [Trailing Trivia] EndOfLineTrivia: "\n"
│           │   [Trailing Trivia] WhitespaceTrivia: "    "
│           └── ReturnStatement [65..115] (6:5)
│               ├── ReturnKeyword "return" [65..71] (6:5)
│               │   [Trailing Trivia] WhitespaceTrivia: " "
│               ├── InvocationExpression [72..114] (6:12)
│               │   ├── SimpleMemberAccessExpression [72..97] (6:12)
│               │   │   ├── InvocationExpression [72..91] (6:12)
│               │   │   │   ├── SimpleMemberAccessExpression [72..79] (6:12)
│               │   │   │   │   ├── IdentifierName [72..75] (6:12)
│               │   │   │   │   │   └── IdentifierToken "foo" [72..75] (6:12)
│               │   │   │   │   ├── DotToken "." [75..76] (6:15)
│               │   │   │   │   └── IdentifierName [76..79] (6:16)
│               │   │   │   │       └── IdentifierToken "bar" [76..79] (6:16)
│               │   │   │   └── ArgumentList [79..91] (6:19)
│               │   │   │       ├── OpenParenToken "(" [79..80] (6:19)
│               │   │   │       ├── Argument [80..81] (6:20)
│               │   │   │       │   └── NumericLiteralExpression [80..81] (6:20)
│               │   │   │       │       └── NumericLiteralToken "2" [80..81] (6:20)
│               │   │   │       └── CloseParenToken ")" [81..82] (6:21)
│               │   │   │       └── [Trailing Trivia] EndOfLineTrivia: "\n"
│               │   │   │       └── [Trailing Trivia] WhitespaceTrivia: "        "
│               │   │   ├── DotToken "." [91..92] (7:9)
│               │   │   └── IdentifierName [92..97] (7:10)
│               │   │       └── IdentifierToken "GetId" [92..97] (7:10)
│               │   └── ArgumentList [97..114] (7:15)
│               │       ├── OpenParenToken "(" [97..98] (7:15)
│               │       ├── Argument [98..99] (7:16)
│               │       │   └── NumericLiteralExpression [98..99] (7:16)
│               │       │       └── NumericLiteralToken "1" [98..99] (7:16)
│               │       ├── CommaToken "," [99..100] (7:17)
│               │       │   [Trailing Trivia] WhitespaceTrivia: " "
│               │       ├── Argument [101..106] (7:19)
│               │       │   └── AddExpression [101..106] (7:19)
│               │       │       ├── IdentifierName [101..103] (7:19)
│               │       │       │   └── IdentifierToken "z" [101..102] (7:19)
│               │       │       │   └── [Trailing Trivia] WhitespaceTrivia: " "
│               │       │       ├── PlusToken "+" [103..104] (7:21)
│               │       │       │   [Trailing Trivia] WhitespaceTrivia: " "
│               │       │       └── NumericLiteralExpression [105..106] (7:23)
│               │       │           └── NumericLiteralToken "2" [105..106] (7:23)
│               │       ├── CommaToken "," [106..107] (7:24)
│               │       │   [Trailing Trivia] WhitespaceTrivia: " "
│               │       ├── Argument [108..113] (7:26)
│               │       │   └── StringLiteralExpression [108..113] (7:26)
│               │       │       └── StringLiteralToken ""Foo"" [108..113] (7:26)
│               │       └── CloseParenToken ")" [113..114] (7:31)
│               └── SemicolonToken ";" [114..115] (7:32)
└── EndOfFileToken "" [115..115] (7:33)
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