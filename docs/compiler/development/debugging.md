# Debugging

## Readable representation of the Syntax tree

You can obtain a readable representation of the syntax tree hierarchy that can be printed to the console.

Using the `PrintSyntaxTree` and `GetSyntaxTreeRepresentation` extension methods. 

### Example

Assuming you have a syntax node that represent this code:

```csharp
  var sourceCode = 
            """
            import System

            let no = 2;

            if no == 42 {
                Console.WriteLine(no);
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
root.PrintSyntaxTree(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = true, IncludeLocations = true, Colorize = true });
````

Will give you this:

```
CompilationUnit [0..70] (1,1) - (7,2)
├── ImportDirective [0..14] (1,1) - (2,1)
│   ├── ImportKeyword: import ImportKeyword [0..6] (1,1) - (1,7)
│   │   └── ␣ WhitespaceTrivia [6..7] (1,7) - (1,8)
│   ├── NamespaceOrType: IdentifierName [7..13] (1,8) - (1,14)
│   │   └── Identifier: System IdentifierToken [7..13] (1,8) - (1,14)
│   └── TerminatorToken: \n NewLineToken [13..14] (1,14) - (2,1)
├── GlobalStatement [15..26] (3,1) - (3,12)
│   └── Statement: LocalDeclaration [15..26] (3,1) - (3,12)
│       ├── Declaration: VariableDeclaration [15..25] (3,1) - (3,11)
│       │   │   ┌── \n EndOfLineTrivia [14..15] (2,1) - (3,1)
│       │   ├── LetOrVarKeyword: let LetKeyword [15..18] (3,1) - (3,4)
│       │   │   └── ␣ WhitespaceTrivia [17..18] (3,3) - (3,4)
│       │   └── VariableDeclarator [19..25] (3,5) - (3,11)
│       │       ├── Identifier: no IdentifierToken [19..21] (3,5) - (3,7)
│       │       │   └── ␣ WhitespaceTrivia [21..22] (3,7) - (3,8)
│       │       └── Initializer: EqualsValueClause [22..25] (3,8) - (3,11)
│       │           ├── EqualsToken: = EqualsToken [22..23] (3,8) - (3,9)
│       │           │   └── ␣ WhitespaceTrivia [23..24] (3,9) - (3,10)
│       │           └── Value: NumericLiteralExpression [24..25] (3,10) - (3,11)
│       │               └── Token: 2 NumericLiteralToken [24..25] (3,10) - (3,11)
│       └── TerminatorToken: ; SemicolonToken [25..26] (3,11) - (3,12)
└── GlobalStatement [28..70] (5,1) - (7,2)
    └── Statement: ExpressionStatement [28..70] (5,1) - (7,2)
        ├── Expression: IfExpression [28..70] (5,1) - (7,2)
        │   │   ┌── \n EndOfLineTrivia [26..27] (3,12) - (4,1)
        │   │   ├── \n EndOfLineTrivia [27..28] (4,1) - (5,1)
        │   ├── IfKeyword: if IfKeyword [28..30] (5,1) - (5,3)
        │   │   └── ␣ WhitespaceTrivia [28..29] (5,1) - (5,2)
        │   ├── Condition: EqualsExpression [31..39] (5,4) - (5,12)
        │   │   ├── LeftHandSide: IdentifierName [31..33] (5,4) - (5,6)
        │   │   │   └── Identifier: no IdentifierToken [31..33] (5,4) - (5,6)
        │   │   │       └── ␣ WhitespaceTrivia [33..34] (5,6) - (5,7)
        │   │   ├── OperatorToken: == EqualsEqualsToken [34..36] (5,7) - (5,9)
        │   │   │   └── ␣ WhitespaceTrivia [36..37] (5,9) - (5,10)
        │   │   └── RightHandSide: NumericLiteralExpression [37..39] (5,10) - (5,12)
        │   │       └── Token: 42 NumericLiteralToken [37..39] (5,10) - (5,12)
        │   │           └── ␣ WhitespaceTrivia [39..40] (5,12) - (5,13)
        │   └── Expression: Block [40..70] (5,13) - (7,2)
        │       ├── OpenBraceToken: { OpenBraceToken [40..41] (5,13) - (5,14)
        │       ├── ExpressionStatement [46..68] (6,5) - (6,27)
        │       │   ├── Expression: InvocationExpression [46..67] (6,5) - (6,26)
        │       │   │   ├── Expression: SimpleMemberAccessExpression [46..63] (6,5) - (6,22)
        │       │   │   │   ├── Expression: IdentifierName [46..53] (6,5) - (6,12)
        │       │   │   │   │   │   ┌── \n EndOfLineTrivia [41..42] (5,14) - (6,1)
        │       │   │   │   │   │   ├── ␣␣␣␣ WhitespaceTrivia [42..46] (6,1) - (6,5)
        │       │   │   │   │   └── Identifier: Console IdentifierToken [46..53] (6,5) - (6,12)
        │       │   │   │   ├── OperatorToken: . DotToken [53..54] (6,12) - (6,13)
        │       │   │   │   └── Name: IdentifierName [54..63] (6,13) - (6,22)
        │       │   │   │       └── Identifier: WriteLine IdentifierToken [54..63] (6,13) - (6,22)
        │       │   │   └── ArgumentList: ArgumentList [63..67] (6,22) - (6,26)
        │       │   │       ├── OpenParenToken: ( OpenParenToken [63..64] (6,22) - (6,23)
        │       │   │       ├── Argument [64..66] (6,23) - (6,25)
        │       │   │       │   └── Expression: IdentifierName [64..66] (6,23) - (6,25)
        │       │   │       │       └── Identifier: no IdentifierToken [64..66] (6,23) - (6,25)
        │       │   │       └── CloseParenToken: ) CloseParenToken [66..67] (6,25) - (6,26)
        │       │   └── TerminatorToken: ; SemicolonToken [67..68] (6,26) - (6,27)
        │       │   ┌── \n EndOfLineTrivia [68..69] (6,27) - (7,1)
        │       └── CloseBraceToken: } CloseBraceToken [69..70] (7,1) - (7,2)
        └── TerminatorToken:  EndOfFileToken [70..70] (7,2) - (7,2)
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