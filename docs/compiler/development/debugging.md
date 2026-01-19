# Debugging

## Readable representation of the Syntax tree

You can obtain a readable representation of the syntax tree hierarchy that can be printed to the console.

Using the `PrintSyntaxTree` and `GetSyntaxTreeRepresentation` extension methods. 

### Example

Assuming you have a syntax node that represent this code:

```csharp
  var sourceCode = 
            """
            import System.*

            let no = 2

            if no == 42 {
                Console.WriteLine(no)
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

> **Tip:**
This can also be printed by adding `-s` as an argument to the Raven.Compiler (ravc) command.

Will give you this:

```
CompilationUnit [0..70] (1,1) - (7,2)
├── ImportDirective [0..16] (1,1) - (1,16)
│   ├── ImportKeyword: import ImportKeyword [0..6] (1,1) - (1,7)
│   │   └── ␣ WhitespaceTrivia [6..7] (1,7) - (1,8)
│   ├── Name: QualifiedName [7..15] (1,8) - (1,16)
│   │   ├── Left: IdentifierName [7..13] (1,8) - (1,14)
│   │   │   └── Identifier: System IdentifierToken [7..13] (1,8) - (1,14)
│   │   ├── DotToken: . DotToken [13..14] (1,14) - (1,15)
│   │   └── Right: WildcardName [14..15] (1,15) - (1,16)
│   │       └── StartToken: * StarToken [14..15] (1,15) - (1,16)
│   └── TerminatorToken: \n NewLineToken [15..16] (1,16) - (2,1)
├── GlobalStatement [17..28] (3,1) - (4,1)
│   └── Statement: LocalDeclarationStatement [17..28] (3,1) - (3,11)
│       ├── Declaration: VariableDeclaration [17..27] (3,1) - (3,11)
│       │   │   ┌── \n EndOfLineTrivia [16..17] (2,1) - (3,1)
│       │   ├── BindingKeyword: let LetKeyword [17..20] (3,1) - (3,4)
│       │   │   └── ␣ WhitespaceTrivia [19..20] (3,3) - (3,4)
│       │   └── VariableDeclarator [21..27] (3,5) - (3,11)
│       │       ├── Identifier: no IdentifierToken [21..23] (3,5) - (3,7)
│       │       │   └── ␣ WhitespaceTrivia [23..24] (3,7) - (3,8)
│       │       └── Initializer: EqualsValueClause [24..27] (3,8) - (3,11)
│       │           ├── EqualsToken: = EqualsToken [24..25] (3,8) - (3,9)
│       │           │   └── ␣ WhitespaceTrivia [25..26] (3,9) - (3,10)
│       │           └── Value: NumericLiteralExpression [26..27] (3,10) - (3,11)
│       │               └── Token: 2 NumericLiteralToken [26..27] (3,10) - (3,11)
│       └── TerminatorToken: \n NewLineToken [27..28] (3,11) - (4,1)
├── GlobalStatement [29..70] (5,1) - (7,2)
│   └── Statement: IfStatement [29..70] (5,1) - (7,2)
│       │   ┌── \n EndOfLineTrivia [28..29] (4,1) - (5,1)
│       ├── IfKeyword: if IfKeyword [29..31] (5,1) - (5,3)
│       │   └── ␣ WhitespaceTrivia [30..31] (5,2) - (5,3)
│       ├── Condition: EqualsExpression [32..40] (5,4) - (5,12)
│       │   ├── Left: IdentifierName [32..34] (5,4) - (5,6)
│       │   │   └── Identifier: no IdentifierToken [32..34] (5,4) - (5,6)
│       │   │       └── ␣ WhitespaceTrivia [34..35] (5,6) - (5,7)
│       │   ├── OperatorToken: == EqualsEqualsToken [35..37] (5,7) - (5,9)
│       │   │   └── ␣ WhitespaceTrivia [37..38] (5,9) - (5,10)
│       │   └── Right: NumericLiteralExpression [38..40] (5,10) - (5,12)
│       │       └── Token: 42 NumericLiteralToken [38..40] (5,10) - (5,12)
│       │           └── ␣ WhitespaceTrivia [40..41] (5,12) - (5,13)
│       ├── ThenStatement: BlockStatement [41..70] (5,13) - (7,2)
│       │   ├── OpenBraceToken: { OpenBraceToken [41..42] (5,13) - (5,14)
│       │   ├── ExpressionStatement [47..69] (6,5) - (6,26)
│       │   │   ├── Expression: InvocationExpression [47..68] (6,5) - (6,26)
│       │   │   │   ├── Expression: SimpleMemberAccessExpression [47..64] (6,5) - (6,22)
│       │   │   │   │   ├── Expression: IdentifierName [47..54] (6,5) - (6,12)
│       │   │   │   │   │   │   ┌── \n EndOfLineTrivia [42..43] (5,14) - (6,1)
│       │   │   │   │   │   │   ├── ␣␣␣␣ WhitespaceTrivia [43..47] (6,1) - (6,5)
│       │   │   │   │   │   └── Identifier: Console IdentifierToken [47..54] (6,5) - (6,12)
│       │   │   │   │   ├── OperatorToken: . DotToken [54..55] (6,12) - (6,13)
│       │   │   │   │   └── Name: IdentifierName [55..64] (6,13) - (6,22)
│       │   │   │   │       └── Identifier: WriteLine IdentifierToken [55..64] (6,13) - (6,22)
│       │   │   │   └── ArgumentList: ArgumentList [64..68] (6,22) - (6,26)
│       │   │   │       ├── OpenParenToken: ( OpenParenToken [64..65] (6,22) - (6,23)
│       │   │   │       ├── Argument [65..67] (6,23) - (6,25)
│       │   │   │       │   └── Expression: IdentifierName [65..67] (6,23) - (6,25)
│       │   │   │       │       └── Identifier: no IdentifierToken [65..67] (6,23) - (6,25)
│       │   │   │       └── CloseParenToken: ) CloseParenToken [67..68] (6,25) - (6,26)
│       │   │   └── TerminatorToken: \n NewLineToken [68..69] (6,26) - (7,1)
│       │   └── CloseBraceToken: } CloseBraceToken [69..70] (7,1) - (7,2)
│       └── TerminatorToken:  None [70..70] (7,2) - (7,2)
└── EndOfFileToken:  EndOfFileToken [70..70] (7,2) - (7,2)
```

### Syntax tree to source code

You can output the syntax tree into source code [again]. This is useful for debugging when building your own trees programmatically.

This method is available on every `SyntaxNode`:

```csharp
var str = syntaxNode.ToFullString();
```

## Readable representation of Binder hierarchy

To visualize the binder hierarchy.

Simply invoke `PrintBinderTree` on the `SemanticModel`.

```csharp
semanticModel.PrintBinderTree();
```

> **Tip:**
This is also printed by adding `-bt` as an argument to the Raven.Compiler (ravc) command.

```
GlobalBinder
└── NamespaceBinder (<global>)
    └── ImportBinder
        └── TopLevelBinder (synthesized Main)
            └── LocalScopeBinder
                └── LocalScopeBinder
```

## Readable representation of Bound tree

To visualize the bound tree.

Simply invoke `PrintBoundTree` on the `SemanticModel`.

```csharp
semanticModel.PrintBoundTree();
```

> **Tip:**
This is also printed by adding `-bt` as an argument to the Raven.Compiler (ravc) command.

```
BlockStatement [Syntax=CompilationUnit]
├── LocalDeclarationStatement [Syntax=LocalDeclarationStatement, Symbol=val no: int, IsUsing=false]
│   └── VariableDeclarator [Syntax=VariableDeclarator, Local=val no: int]
│       └── LiteralExpression [Syntax=NumericLiteralExpression, Type=int, Value=2, Kind=NumericLiteral]
└── IfStatement [Syntax=IfStatement]
    ├── BinaryExpression [Type=bool]
    │   ├── LocalAccess [Type=int, Symbol=val no: int, Local=val no: int]
    │   └── LiteralExpression [Type=int, Value=42, Kind=NumericLiteral]
    └── BlockStatement [Syntax=BlockStatement]
        └── ExpressionStatement [Syntax=ExpressionStatement, Symbol=static Console.WriteLine(value: int) → ()]
            └── InvocationExpression [Type=(), Symbol=static Console.WriteLine(value: int) → (), Method=static Console.WriteLine(value: int) → (), RequiresReceiverAddress=false]
                ├── LocalAccess [Type=int, Symbol=val no: int, Local=val no: int]
                └── TypeExpression [Type=Console, Symbol=Console, TypeSymbol=Console]
```

## Source generator

### Debugging

Uncommenting this line should make the debugger launch in Visual Studio:

```csharp
Debugger.Launch();
```

This occurs when the generator is running, that is, when the ``Raven.CodeAnalysis`` re-compiles:

### Inspecting generated source code

The output of the source generator for syntax is found in `Raven.CodeAnalysis/Syntax/generated` and in `Raven.CodeAnalysis/Syntax/InternalSyntax/generated`.

The output of the source generator for bound nodes, is found in `Raven.CodeAnalysis/obj/Debug/net9.0/generated`

### Suppress warnings in .NET CLI

Sometimes errors don't show up in CLI, so try this:

`dotnet build --property WarningLevel=0`

When run triggers a build:

`dotnet run --property WarningLevel=0`