# Compiler API

This document outlines the API of the Raven compiler, providing guidance on syntax analysis, semantic analysis, and code generation.

---

## Syntax Analysis

This section covers the basics of syntax analysis, including parsing source code, transforming syntax trees, and visualizing syntax hierarchies.

### Parsing Source Code into a Syntax Tree

To parse source code into a syntax tree, use the following example:

```csharp
let x : int = 2;

if (x > 2) {
    return (6 + 2) * 2;
} else {
    return foo.bar(2)
        .GetId(1, z + 2, "Foo");
}
```

Here’s how to parse the code:

```csharp
var syntaxTree = SyntaxTree.ParseText("...");

// Retrieves the CompilationUnit (root syntax node)
var root = syntaxTree.GetRoot();
```

### Converting a Syntax Tree Back to Source Code

To convert a syntax tree back to its source code:

```csharp
var sourceCode = root.ToFullString();
```

The output will reproduce the original code:

```csharp
let x : int = 2;

if (x > 2) {
    return (6 + 2) * 2;
} else {
    return foo.bar(2)
        .GetId(1, z + 2, "Foo");
}
```

### Visualizing the Syntax Hierarchy

You can print the syntax tree’s hierarchical structure to the console:

```csharp
root.PrintSyntaxTree(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = true, IncludeLocations = true, Colorize = true });
```

#### Example Syntax Tree Output

For the following code:

```csharp
let x : int = 2;

if (x > 2) {
    return (6 + 2) * 2;
} else {
    return foo.bar(2)
        .GetId(1, z + 2, "Foo");
}
```

The syntax tree will look like:

```
CompilationUnit [0..120] (1,1) - (8,2)
├── GlobalStatement [0..16] (1,1) - (1,17)
│   └── Statement: LocalDeclaration [0..16] (1,1) - (1,17)
│       ├── Declaration: VariableDeclaration [0..15] (1,1) - (1,16)
│       │   ├── LetOrVarKeyword: let LetKeyword [0..3] (1,1) - (1,4)
│       │   │   └── ␣ WhitespaceTrivia [3..4] (1,4) - (1,5)
│       │   └── VariableDeclarator [4..15] (1,5) - (1,16)
│       │       ├── Identifier: x IdentifierToken [4..5] (1,5) - (1,6)
│       │       │   └── ␣ WhitespaceTrivia [5..6] (1,6) - (1,7)
│       │       ├── TypeAnnotation: TypeAnnotationClause [6..11] (1,7) - (1,12)
│       │       │   ├── ColonToken: : ColonToken [6..7] (1,7) - (1,8)
│       │       │   │   └── ␣ WhitespaceTrivia [7..8] (1,8) - (1,9)
│       │       │   └── Type: PredefinedType [8..11] (1,9) - (1,12)
│       │       │       └── Keyword: int IntKeyword [8..11] (1,9) - (1,12)
│       │       │           └── ␣ WhitespaceTrivia [11..12] (1,12) - (1,13)
│       │       └── Initializer: EqualsValueClause [12..15] (1,13) - (1,16)
│       │           ├── EqualsToken: = EqualsToken [12..13] (1,13) - (1,14)
│       │           │   └── ␣ WhitespaceTrivia [13..14] (1,14) - (1,15)
│       │           └── Value: NumericLiteralExpression [14..15] (1,15) - (1,16)
│       │               └── Token: 2 NumericLiteralToken [14..15] (1,15) - (1,16)
│       └── TerminatorToken: ; SemicolonToken [15..16] (1,16) - (1,17)
└── GlobalStatement [18..120] (3,1) - (8,2)
    └── Statement: ExpressionStatement [18..120] (3,1) - (8,2)
        ├── Expression: IfExpression [18..120] (3,1) - (8,2)
        │   │   ┌── \n EndOfLineTrivia [16..17] (1,17) - (2,1)
        │   │   ├── \n EndOfLineTrivia [17..18] (2,1) - (3,1)
        │   ├── IfKeyword: if IfKeyword [18..20] (3,1) - (3,3)
        │   │   └── ␣ WhitespaceTrivia [18..19] (3,1) - (3,2)
        │   ├── Condition: ParenthesizedExpression [21..28] (3,4) - (3,11)
        │   │   ├── OpenParenToken: ( OpenParenToken [21..22] (3,4) - (3,5)
        │   │   ├── Expression: GreaterThanExpression [22..27] (3,5) - (3,10)
        │   │   │   ├── LeftHandSide: IdentifierName [22..23] (3,5) - (3,6)
        │   │   │   │   └── Identifier: x IdentifierToken [22..23] (3,5) - (3,6)
        │   │   │   │       └── ␣ WhitespaceTrivia [23..24] (3,6) - (3,7)
        │   │   │   ├── OperatorToken: > GreaterThanToken [24..25] (3,7) - (3,8)
        │   │   │   │   └── ␣ WhitespaceTrivia [25..26] (3,8) - (3,9)
        │   │   │   └── RightHandSide: NumericLiteralExpression [26..27] (3,9) - (3,10)
        │   │   │       └── Token: 2 NumericLiteralToken [26..27] (3,9) - (3,10)
        │   │   └── CloseParenToken: ) CloseParenToken [27..28] (3,10) - (3,11)
        │   │       └── ␣ WhitespaceTrivia [28..29] (3,11) - (3,12)
        │   ├── Expression: Block [29..56] (3,12) - (5,2)
        │   │   ├── OpenBraceToken: { OpenBraceToken [29..30] (3,12) - (3,13)
        │   │   ├── ReturnStatement [35..54] (4,5) - (4,24)
        │   │   │   │   ┌── \n EndOfLineTrivia [30..31] (3,13) - (4,1)
        │   │   │   │   ├── ␣␣␣␣ WhitespaceTrivia [31..35] (4,1) - (4,5)
        │   │   │   ├── ReturnKeyword: return ReturnKeyword [35..41] (4,5) - (4,11)
        │   │   │   │   └── ␣ WhitespaceTrivia [36..37] (4,6) - (4,7)
        │   │   │   ├── Expression: MultiplyExpression [42..53] (4,12) - (4,23)
        │   │   │   │   ├── LeftHandSide: ParenthesizedExpression [42..49] (4,12) - (4,19)
        │   │   │   │   │   ├── OpenParenToken: ( OpenParenToken [42..43] (4,12) - (4,13)
        │   │   │   │   │   ├── Expression: AddExpression [43..48] (4,13) - (4,18)
        │   │   │   │   │   │   ├── LeftHandSide: NumericLiteralExpression [43..44] (4,13) - (4,14)
        │   │   │   │   │   │   │   └── Token: 6 NumericLiteralToken [43..44] (4,13) - (4,14)
        │   │   │   │   │   │   │       └── ␣ WhitespaceTrivia [44..45] (4,14) - (4,15)
        │   │   │   │   │   │   ├── OperatorToken: + PlusToken [45..46] (4,15) - (4,16)
        │   │   │   │   │   │   │   └── ␣ WhitespaceTrivia [46..47] (4,16) - (4,17)
        │   │   │   │   │   │   └── RightHandSide: NumericLiteralExpression [47..48] (4,17) - (4,18)
        │   │   │   │   │   │       └── Token: 2 NumericLiteralToken [47..48] (4,17) - (4,18)
        │   │   │   │   │   └── CloseParenToken: ) CloseParenToken [48..49] (4,18) - (4,19)
        │   │   │   │   │       └── ␣ WhitespaceTrivia [49..50] (4,19) - (4,20)
        │   │   │   │   ├── OperatorToken: * StarToken [50..51] (4,20) - (4,21)
        │   │   │   │   │   └── ␣ WhitespaceTrivia [51..52] (4,21) - (4,22)
        │   │   │   │   └── RightHandSide: NumericLiteralExpression [52..53] (4,22) - (4,23)
        │   │   │   │       └── Token: 2 NumericLiteralToken [52..53] (4,22) - (4,23)
        │   │   │   └── TerminatorToken: ; SemicolonToken [53..54] (4,23) - (4,24)
        │   │   │   ┌── \n EndOfLineTrivia [54..55] (4,24) - (5,1)
        │   │   └── CloseBraceToken: } CloseBraceToken [55..56] (5,1) - (5,2)
        │   │       └── ␣ WhitespaceTrivia [55..56] (5,1) - (5,2)
        │   └── ElseClause: ElseClause [57..120] (5,3) - (8,2)
        │       ├── ElseKeyword: else ElseKeyword [57..61] (5,3) - (5,7)
        │       │   └── ␣ WhitespaceTrivia [61..62] (5,7) - (5,8)
        │       └── Expression: Block [62..120] (5,8) - (8,2)
        │           ├── OpenBraceToken: { OpenBraceToken [62..63] (5,8) - (5,9)
        │           ├── ReturnStatement [68..118] (6,5) - (7,33)
        │           │   │   ┌── \n EndOfLineTrivia [63..64] (5,9) - (6,1)
        │           │   │   ├── ␣␣␣␣ WhitespaceTrivia [64..68] (6,1) - (6,5)
        │           │   ├── ReturnKeyword: return ReturnKeyword [68..74] (6,5) - (6,11)
        │           │   │   └── ␣ WhitespaceTrivia [69..70] (6,6) - (6,7)
        │           │   ├── Expression: InvocationExpression [75..117] (6,12) - (7,32)
        │           │   │   ├── Expression: SimpleMemberAccessExpression [75..100] (6,12) - (7,15)
        │           │   │   │   ├── Expression: InvocationExpression [75..85] (6,12) - (6,22)
        │           │   │   │   │   ├── Expression: SimpleMemberAccessExpression [75..82] (6,12) - (6,19)
        │           │   │   │   │   │   ├── Expression: IdentifierName [75..78] (6,12) - (6,15)
        │           │   │   │   │   │   │   └── Identifier: foo IdentifierToken [75..78] (6,12) - (6,15)
        │           │   │   │   │   │   ├── OperatorToken: . DotToken [78..79] (6,15) - (6,16)
        │           │   │   │   │   │   └── Name: IdentifierName [79..82] (6,16) - (6,19)
        │           │   │   │   │   │       └── Identifier: bar IdentifierToken [79..82] (6,16) - (6,19)
        │           │   │   │   │   └── ArgumentList: ArgumentList [82..85] (6,19) - (6,22)
        │           │   │   │   │       ├── OpenParenToken: ( OpenParenToken [82..83] (6,19) - (6,20)
        │           │   │   │   │       ├── Argument [83..84] (6,20) - (6,21)
        │           │   │   │   │       │   └── Expression: NumericLiteralExpression [83..84] (6,20) - (6,21)
        │           │   │   │   │       │       └── Token: 2 NumericLiteralToken [83..84] (6,20) - (6,21)
        │           │   │   │   │       └── CloseParenToken: ) CloseParenToken [84..85] (6,21) - (6,22)
        │           │   │   │   │   ┌── \n EndOfLineTrivia [85..86] (6,22) - (7,1)
        │           │   │   │   │   ├── ␣␣␣␣␣␣␣␣ WhitespaceTrivia [86..94] (7,1) - (7,9)
        │           │   │   │   ├── OperatorToken: . DotToken [94..95] (7,9) - (7,10)
        │           │   │   │   └── Name: IdentifierName [95..100] (7,10) - (7,15)
        │           │   │   │       └── Identifier: GetId IdentifierToken [95..100] (7,10) - (7,15)
        │           │   │   └── ArgumentList: ArgumentList [100..117] (7,15) - (7,32)
        │           │   │       ├── OpenParenToken: ( OpenParenToken [100..101] (7,15) - (7,16)
        │           │   │       ├── Argument [101..102] (7,16) - (7,17)
        │           │   │       │   └── Expression: NumericLiteralExpression [101..102] (7,16) - (7,17)
        │           │   │       │       └── Token: 1 NumericLiteralToken [101..102] (7,16) - (7,17)
        │           │   │       ├── , CommaToken [102..103] (7,17) - (7,18)
        │           │   │       │   └── ␣ WhitespaceTrivia [103..104] (7,18) - (7,19)
        │           │   │       ├── Argument [104..109] (7,19) - (7,24)
        │           │   │       │   └── Expression: AddExpression [104..109] (7,19) - (7,24)
        │           │   │       │       ├── LeftHandSide: IdentifierName [104..105] (7,19) - (7,20)
        │           │   │       │       │   └── Identifier: z IdentifierToken [104..105] (7,19) - (7,20)
        │           │   │       │       │       └── ␣ WhitespaceTrivia [105..106] (7,20) - (7,21)
        │           │   │       │       ├── OperatorToken: + PlusToken [106..107] (7,21) - (7,22)
        │           │   │       │       │   └── ␣ WhitespaceTrivia [107..108] (7,22) - (7,23)
        │           │   │       │       └── RightHandSide: NumericLiteralExpression [108..109] (7,23) - (7,24)
        │           │   │       │           └── Token: 2 NumericLiteralToken [108..109] (7,23) - (7,24)
        │           │   │       ├── , CommaToken [109..110] (7,24) - (7,25)
        │           │   │       │   └── ␣ WhitespaceTrivia [110..111] (7,25) - (7,26)
        │           │   │       ├── Argument [111..116] (7,26) - (7,31)
        │           │   │       │   └── Expression: StringLiteralExpression [111..116] (7,26) - (7,31)
        │           │   │       │       └── Token: "Foo" StringLiteralToken [111..116] (7,26) - (7,31)
        │           │   │       └── CloseParenToken: ) CloseParenToken [116..117] (7,31) - (7,32)
        │           │   └── TerminatorToken: ; SemicolonToken [117..118] (7,32) - (7,33)
        │           │   ┌── \n EndOfLineTrivia [118..119] (7,33) - (8,1)
        │           └── CloseBraceToken: } CloseBraceToken [119..120] (8,1) - (8,2)
        └── TerminatorToken:  EndOfFileToken [120..120] (8,2) - (8,2)
```

### Modifying a Syntax Tree

Syntax trees are immutable, so updating a `SyntaxNode` creates a new, detached node:

```csharp
var sourceCode = 
    """
    let x : int = "foo";
    """;

var syntaxTree = SyntaxTree.ParseText(sourceCode);
var root = syntaxTree.GetRoot();

// Modify a node
var oldNode = root.DescendantNodes().OfType<VariableDeclaratorSyntax>().First();
var newNode = oldNode.WithName(SyntaxFactory.IdentifierName("Foo"));

// Replace the node in the tree
var newCompilationUnit = root.ReplaceNode(oldNode, newNode);

// Create a new tree
var newTree = SyntaxTree.Create(newCompilationUnit);
```

### Applying Changes from Source Text

You can update a tree directly from modified source text. The compiler efficiently reuses syntax nodes:

```csharp
var sourceText = SourceText.From(
    """
    if (foo)  {
        return 0;
    }
    """);

var syntaxTree = SyntaxTree.ParseText(sourceText);

var changedSourceText = SourceText.From(
    """
    if (foo)  {
        return 0;
    } else if (bar ) {
        return 1;
    }
    """);

var updatedTree = syntaxTree.WithChangedText(changedSourceText);
var newRoot = updatedTree.GetRoot();
Console.WriteLine(newRoot.ToFullString());
```

---

## Compilation

A `Compilation` encapsulates all components required for compiling, including syntax trees, nodes, semantic models, and symbols.

### Semantic Analysis

Retrieve symbol information using the `Compilation` and `SemanticModel` classes.

#### Example

For the code:

```csharp
let x : int = 2;

if (x > 2) {
    return (6 + 2) * 2;
} else {
    return foo.bar(2)
        .GetId(1, z + 2, "Foo");
}
```

Use the following example:

```csharp
var syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceText);

var compilation = Compilation.Create("MyAssembly", new CompilationOptions(OutputKind.ConsoleApplication))
    .AddSyntaxTrees(syntaxTree)
    .AddReferences([
        // The path to the reference assembly for System.Runtime. Will determine what version of .NET you compile against.
        // On Windows the path is different.Despite pointing at a file in Mac. The app will run on other platforms.
        MetadataReference.CreateFromFile("/usr/local/share/dotnet/packs/Microsoft.NETCore.App.Ref/9.0.0/ref/net9.0/System.Runtime.dll"),
        MetadataReference.CreateFromFile(typeof(Console).Assembly.Location)
    ]);

syntaxTree = compilation.SyntaxTrees.First();

var semanticModel = compilation.GetSemanticModel(syntaxTree);
var variableDeclarator = syntaxTree.GetRoot()
    .DescendantNodes()
    .OfType<VariableDeclaratorSyntax>()
    .First();

var symbol = semanticModel.GetDeclaredSymbol(variableDeclarator) as ILocalSymbol;
Console.WriteLine(symbol.Name);
```

---

## Code Generation

Once you have a valid compilation, you can emit code using the `Emit` method:

```csharp
var syntaxTree = SyntaxFactory.ParseSyntaxTree(sourceText);

var compilation = Compilation.Create("MyAssembly", new CompilationOptions(OutputKind.ConsoleApplication))
    .AddSyntaxTrees(syntaxTree)
    .AddReferences([
        MetadataReference.CreateFromFile(("/usr/local/share/dotnet/packs/Microsoft.NETCore.App.Ref/9.0.0/ref/net9.0/System.Runtime.dll"),
        MetadataReference.CreateFromFile(typeof(Console).Assembly.Location)
    ]);

using var stream = File.OpenWrite("MyAssembly.exe");
var result = compilation.Emit(stream);

if(result.Success) 
{
    Console.WriteLine("Build succeeded");
}

// Get diagnostics
var diagnostics = result.Diagnostics;

```

This will generate an executable (`MyAssembly.exe`) from your source code.