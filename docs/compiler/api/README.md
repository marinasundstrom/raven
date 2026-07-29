# Compiler API

This document outlines the API of the Raven compiler, providing guidance on syntax analysis, semantic analysis, and code generation.

For analyzer, source-generator, refactoring, and language-server hot paths, see
the [Compiler API performance guidance](performance.md).

The examples are written in Raven and use `Raven.CodeAnalysis` directly. This
is the preferred form for Raven tools and macros. A C# host can call the same
.NET APIs; C# is shown only where a section specifically discusses that
integration boundary.

---

## Syntax Analysis

This section covers the basics of syntax analysis, including parsing source code, transforming syntax trees, and visualizing syntax hierarchies. For a structured tour of the public surface, see the [Syntax Tree API](syntax-tree.md).

### Parsing Source Code into a Syntax Tree

To parse source code into a syntax tree, use the following example:

```raven
let x: int = 2

if (x > 2) {
    return (6 + 2) * 2
} else {
    return foo.bar(2)
        .GetId(1, z + 2, "Foo")
}
```

Here’s how to parse the code:

```raven
import Raven.CodeAnalysis.Syntax.*

let syntaxTree = SyntaxTree.ParseText("...")
let root = syntaxTree.GetRoot() // CompilationUnitSyntax
```

### Converting a Syntax Tree Back to Source Code

To convert a syntax tree back to its source code:

```raven
let sourceCode = root.ToFullString()
```

The output will reproduce the original code:

```raven
let x: int = 2

if (x > 2) {
    return (6 + 2) * 2
} else {
    return foo.bar(2)
        .GetId(1, z + 2, "Foo")
}
```

### Visualizing the Syntax Hierarchy

You can print the syntax tree’s hierarchical structure to the console:

```raven
root.PrintSyntaxTree(PrinterOptions {
    IncludeNames = true
    IncludeTokens = true
    IncludeTrivia = true
    IncludeSpans = true
    IncludeLocations = true
    Colorize = true
})
```

#### Example Syntax Tree Output

For the following code:

```raven
import System.*

sayHello("Bob")

func sayHello(name: string?) -> () {
    if name is not null {
        Console.WriteLine("Hello, $name!")
    }
}
```

The syntax tree will look like:

```
CompilationUnit [0..147] (1,1) - (9,2)
├── ImportDirective [0..16] (1,1) - (1,16)
│   ├── ImportKeyword: import ImportKeyword [0..6] (1,1) - (1,7)
│   │   └── ␣ WhitespaceTrivia [6..7] (1,7) - (1,8)
│   ├── Name: QualifiedName [7..15] (1,8) - (1,16)
│   │   ├── Left: IdentifierName [7..13] (1,8) - (1,14)
│   │   │   └── Identifier: System IdentifierToken [7..13] (1,8) - (1,14)
│   │   ├── DotToken: . DotToken [13..14] (1,14) - (1,15)
│   │   └── Right: WildcardName [14..15] (1,15) - (1,16)
│   │       └── StartToken: * StarToken [14..15] (1,15) - (1,16)
│   └── TerminatorToken:  None [15..15] (1,16) - (1,16)
├── GlobalStatement [17..33] (3,1) - (4,1)
│   └── Statement: ExpressionStatement [17..33] (3,1) - (3,16)
│       ├── Expression: InvocationExpression [17..32] (3,1) - (3,16)
│       │   ├── Expression: IdentifierName [17..25] (3,1) - (3,9)
│       │   │   │   ┌── \n EndOfLineTrivia [16..17] (2,1) - (3,1)
│       │   │   └── Identifier: sayHello IdentifierToken [17..25] (3,1) - (3,9)
│       │   └── ArgumentList: ArgumentList [25..32] (3,9) - (3,16)
│       │       ├── OpenParenToken: ( OpenParenToken [25..26] (3,9) - (3,10)
│       │       ├── Argument [26..31] (3,10) - (3,15)
│       │       │   └── Expression: StringLiteralExpression [26..31] (3,10) - (3,15)
│       │       │       └── Token: "Bob" StringLiteralToken [26..31] (3,10) - (3,15)
│       │       └── CloseParenToken: ) CloseParenToken [31..32] (3,15) - (3,16)
│       └── TerminatorToken:  None [32..32] (3,16) - (3,16)
├── GlobalStatement [34..147] (5,1) - (9,2)
│   └── Statement: FunctionStatement [34..147] (5,1) - (9,2)
│       │   ┌── \n EndOfLineTrivia [33..34] (4,1) - (5,1)
│       ├── FuncKeyword: func FuncKeyword [34..38] (5,1) - (5,5)
│       │   └── ␣ WhitespaceTrivia [37..38] (5,4) - (5,5)
│       ├── Identifier: sayHello IdentifierToken [39..47] (5,6) - (5,14)
│       ├── ParameterList: ParameterList [47..62] (5,14) - (5,29)
│       │   ├── OpenParenToken: ( OpenParenToken [47..48] (5,14) - (5,15)
│       │   ├── Parameter [48..61] (5,15) - (5,28)
│       │   │   ├── Identifier: name IdentifierToken [48..52] (5,15) - (5,19)
│       │   │   └── TypeAnnotation: TypeAnnotationClause [52..61] (5,19) - (5,28)
│       │   │       ├── ColonToken: : ColonToken [52..53] (5,19) - (5,20)
│       │   │       │   └── ␣ WhitespaceTrivia [53..54] (5,20) - (5,21)
│       │   │       └── Type: NullableType [54..61] (5,21) - (5,28)
│       │   │           ├── ElementType: PredefinedType [54..60] (5,21) - (5,27)
│       │   │           │   └── Keyword: string StringKeyword [54..60] (5,21) - (5,27)
│       │   │           └── QuestionToken: ? QuestionToken [60..61] (5,27) - (5,28)
│       │   └── CloseParenToken: ) CloseParenToken [61..62] (5,28) - (5,29)
│       │       └── ␣ WhitespaceTrivia [62..63] (5,29) - (5,30)
│       ├── ReturnType: ArrowTypeClause [63..68] (5,30) - (5,35)
│       │   ├── ArrowToken: -> ArrowToken [63..65] (5,30) - (5,32)
│       │   │   └── ␣ WhitespaceTrivia [65..66] (5,32) - (5,33)
│       │   └── Type: UnitType [66..68] (5,33) - (5,35)
│       │       ├── OpenParenToken: ( OpenParenToken [66..67] (5,33) - (5,34)
│       │       └── CloseParenToken: ) CloseParenToken [67..68] (5,34) - (5,35)
│       │           └── ␣ WhitespaceTrivia [68..69] (5,35) - (5,36)
│       ├── Body: BlockStatement [69..147] (5,36) - (9,2)
│       │   ├── OpenBraceToken: { OpenBraceToken [69..70] (5,36) - (5,37)
│       │   ├── IfStatement [75..146] (6,5) - (8,6)
│       │   │   │   ┌── \n EndOfLineTrivia [70..71] (5,37) - (6,1)
│       │   │   │   ├── ␣␣␣␣ WhitespaceTrivia [71..75] (6,1) - (6,5)
│       │   │   ├── IfKeyword: if IfKeyword [75..77] (6,5) - (6,7)
│       │   │   │   └── ␣ WhitespaceTrivia [72..73] (6,2) - (6,3)
│       │   │   ├── Condition: IsPatternExpression [78..95] (6,8) - (6,25)
│       │   │   │   ├── Expression: IdentifierName [78..82] (6,8) - (6,12)
│       │   │   │   │   └── Identifier: name IdentifierToken [78..82] (6,8) - (6,12)
│       │   │   │   │       └── ␣ WhitespaceTrivia [82..83] (6,12) - (6,13)
│       │   │   │   ├── IsKeyword: is IsKeyword [83..85] (6,13) - (6,15)
│       │   │   │   │   └── ␣ WhitespaceTrivia [85..86] (6,15) - (6,16)
│       │   │   │   └── Pattern: NotPattern [86..95] (6,16) - (6,25)
│       │   │   │       ├── OperatorToken: not NotKeyword [86..89] (6,16) - (6,19)
│       │   │   │       │   └── ␣ WhitespaceTrivia [89..90] (6,19) - (6,20)
│       │   │   │       └── Pattern: DeclarationPattern [90..95] (6,20) - (6,25)
│       │   │   │           ├── Type: NullType [90..94] (6,20) - (6,24)
│       │   │   │           │   └── NullKeyword: null NullKeyword [90..94] (6,20) - (6,24)
│       │   │   │           │       └── ␣ WhitespaceTrivia [94..95] (6,24) - (6,25)
│       │   │   │           └── Designation: SingleVariableDesignation (Missing) [95..95] (6,25) - (6,25)
│       │   │   │               ├── BindingKeyword:  None [95..95] (6,25) - (6,25)
│       │   │   │               └── Identifier:  None (Missing) [95..95] (6,25) - (6,25)
│       │   │   ├── ThenStatement: BlockStatement [95..145] (6,25) - (8,6)
│       │   │   │   ├── OpenBraceToken: { OpenBraceToken [95..96] (6,25) - (6,26)
│       │   │   │   ├── ExpressionStatement [105..140] (7,9) - (7,43)
│       │   │   │   │   ├── Expression: InvocationExpression [105..139] (7,9) - (7,43)
│       │   │   │   │   │   ├── Expression: SimpleMemberAccessExpression [105..122] (7,9) - (7,26)
│       │   │   │   │   │   │   ├── Expression: IdentifierName [105..112] (7,9) - (7,16)
│       │   │   │   │   │   │   │   │   ┌── \n EndOfLineTrivia [96..97] (6,26) - (7,1)
│       │   │   │   │   │   │   │   │   ├── ␣␣␣␣␣␣␣␣ WhitespaceTrivia [97..105] (7,1) - (7,9)
│       │   │   │   │   │   │   │   └── Identifier: Console IdentifierToken [105..112] (7,9) - (7,16)
│       │   │   │   │   │   │   ├── OperatorToken: . DotToken [112..113] (7,16) - (7,17)
│       │   │   │   │   │   │   └── Name: IdentifierName [113..122] (7,17) - (7,26)
│       │   │   │   │   │   │       └── Identifier: WriteLine IdentifierToken [113..122] (7,17) - (7,26)
│       │   │   │   │   │   └── ArgumentList: ArgumentList [122..139] (7,26) - (7,43)
│       │   │   │   │   │       ├── OpenParenToken: ( OpenParenToken [122..123] (7,26) - (7,27)
│       │   │   │   │   │       ├── Argument [123..138] (7,27) - (7,42)
│       │   │   │   │   │       │   └── Expression: InterpolatedStringExpression [123..138] (7,27) - (7,42)
│       │   │   │   │   │       │       ├── StringStartToken: " StringStartToken [123..124] (7,27) - (7,28)
│       │   │   │   │   │       │       ├── InterpolatedStringText [124..131] (7,28) - (7,35)
│       │   │   │   │   │       │       │   └── Token: Hello,  StringLiteralToken [124..131] (7,28) - (7,35)
│       │   │   │   │   │       │       ├── Interpolation [131..136] (7,35) - (7,40)
│       │   │   │   │   │       │       │   ├── DollarToken: $ DollarToken [131..132] (7,35) - (7,36)
│       │   │   │   │   │       │       │   ├── OpenBraceToken:  OpenBraceToken (Missing) [132..132] (7,36) - (7,36)
│       │   │   │   │   │       │       │   ├── Expression: IdentifierName [132..136] (7,36) - (7,40)
│       │   │   │   │   │       │       │   │   └── Identifier: name IdentifierToken [132..136] (7,36) - (7,40)
│       │   │   │   │   │       │       │   └── CloseBraceToken:  CloseBraceToken (Missing) [136..136] (7,40) - (7,40)
│       │   │   │   │   │       │       ├── InterpolatedStringText [136..137] (7,40) - (7,41)
│       │   │   │   │   │       │       │   └── Token: ! StringLiteralToken [136..137] (7,40) - (7,41)
│       │   │   │   │   │       │       └── StringEndToken: " StringEndToken [137..138] (7,41) - (7,42)
│       │   │   │   │   │       └── CloseParenToken: ) CloseParenToken [138..139] (7,42) - (7,43)
│       │   │   │   │   └── TerminatorToken:  None [139..139] (7,43) - (7,43)
│       │   │   │   │   ┌── ␣␣␣␣ WhitespaceTrivia [140..144] (8,1) - (8,5)
│       │   │   │   └── CloseBraceToken: } CloseBraceToken [144..145] (8,5) - (8,6)
│       │   │   └── TerminatorToken:  None [145..145] (8,6) - (8,6)
│       │   └── CloseBraceToken: } CloseBraceToken [146..147] (9,1) - (9,2)
│       └── TerminatorToken:  None [147..147] (9,2) - (9,2)
└── EndOfFileToken:  EndOfFileToken [147..147] (9,2) - (9,2)
```

### Modifying a Syntax Tree

Syntax trees are immutable, so updating a `SyntaxNode` creates a new, detached node:

```raven
import System.Linq.*
import Raven.CodeAnalysis.Syntax.*
import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

let sourceCode = """
    let x: int = "foo"
    """

let syntaxTree = SyntaxTree.ParseText(sourceCode)
let root = syntaxTree.GetRoot()
let oldNode = root.DescendantNodes()
    .OfType<VariableDeclaratorSyntax>()
    .First()
let newIdentifier = Identifier("Foo") {
    LeadingTrivia = oldNode.Identifier.LeadingTrivia
    TrailingTrivia = oldNode.Identifier.TrailingTrivia
}
let newNode = oldNode with { Identifier = newIdentifier }

let newCompilationUnit = root.ReplaceNode(oldNode, newNode)

let newTree = SyntaxTree.Create(newCompilationUnit)
```

### Formatting factory-built syntax

`SyntaxFactory` produces raw structured syntax. That is useful for analyzers,
rewriters, and code generation, but it also means the resulting text usually
needs explicit trivia or a formatting pass before it is readable.

For full normalization:

```raven
let prettyNode = rawNode.NormalizeWhitespace()
```

For targeted formatting, annotate the nodes you want formatted and use elastic
trivia where spacing/newlines should be computed by the formatter:

```raven
let updatedNode = rawNode.WithAdditionalAnnotations(Formatter.Annotation)
let prettyNode = Formatter.Format(updatedNode)
```

Use `SyntaxFactory.ElasticSpace`, `SyntaxFactory.ElasticLineFeed`, and related
helpers when constructing tokens or lists that should pick up formatter-managed
whitespace.

### Applying Changes from Source Text

You can update a tree directly from modified source text. The compiler efficiently reuses syntax nodes:

```raven
import System.Console.*
import Raven.CodeAnalysis.Syntax.*
import Raven.CodeAnalysis.Text.*

let sourceText = SourceText.From("""
    if foo {
        return 0
    }
    """)

let syntaxTree = SyntaxTree.ParseText(sourceText)

let changedSourceText = SourceText.From("""
    if foo {
        return 0
    } else if bar {
        return 1
    }
    """)

let updatedTree = syntaxTree.WithChangedText(changedSourceText)
let newRoot = updatedTree.GetRoot()
WriteLine(newRoot.ToFullString())
```

---

## Compilation

A `Compilation` encapsulates all components required for compiling, including syntax trees, nodes, semantic models, and symbols.

### Semantic Analysis

Retrieve symbol information using the `Compilation` and `SemanticModel` classes. The [Semantic Analysis API](semantic-analysis.md) dives deeper into compilation construction, diagnostics, symbol binding, and the operations surface.

#### Example

For the code:

```raven
import System.*

let x: int = 2

Console.WriteLine(x)
```

Use the following example:

```raven
import System.Console.*
import System.Linq.*
import Raven.CodeAnalysis.*
import Raven.CodeAnalysis.Syntax.*

let syntaxTree = SyntaxTree.ParseText(sourceText)
let compilation = Compilation.Create(
    "MyAssembly",
    [syntaxTree],
    references,
    CompilationOptions(OutputKind.ConsoleApplication))

let semanticModel = compilation.GetSemanticModel(syntaxTree)
let variableDeclarator = syntaxTree.GetRoot()
    .DescendantNodes()
    .OfType<VariableDeclaratorSyntax>()
    .First()

let symbol = semanticModel.GetDeclaredSymbol(variableDeclarator) as ILocalSymbol
WriteLine(symbol.Name)
```

Here `references` is the host's reference-assembly set for the selected target
framework. Normal `.rvnproj` builds obtain it from MSBuild; a direct
`Compilation` host supplies it explicitly.

### Completion API

The completion surface is available on both `Compilation` and `SemanticModel`:

```raven
let fromCompilation = compilation.GetCompletions(syntaxTree, position)
let fromSemanticModel = semanticModel.GetCompletions(position)
```

Each `CompletionItem` carries the display text, insertion text, and replacement span.

---

## Operations

The operations API provides a stable, syntax-agnostic view of semantics that mirrors Roslyn's `IOperation` abstraction. See
[Operations API](operations.md) for an overview of the surface and how to retrieve operations from a `SemanticModel`.

---

## Code Generation

Once you have a valid compilation, you can emit code using the `Emit` method:

```raven
import System.Console.*
import System.IO.*
import Raven.CodeAnalysis.*
import Raven.CodeAnalysis.Syntax.*

let syntaxTree = SyntaxTree.ParseText(sourceText)
let compilation = Compilation.Create(
    "MyAssembly",
    [syntaxTree],
    references,
    CompilationOptions(OutputKind.ConsoleApplication))

use stream = File.OpenWrite("MyAssembly.dll")
let result = compilation.Emit(stream)

if result.Success {
    WriteLine("Build succeeded")
}

let diagnostics = result.Diagnostics
```

This emits the managed assembly to `MyAssembly.dll`. An application host also
needs the matching runtime configuration and dependencies; prefer the Raven
project system when producing a runnable application.
