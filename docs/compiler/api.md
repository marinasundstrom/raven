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
root.PrintSyntaxTree(includeTrivia: true, includeSpans: true, includeLocation: true);
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
CompilationUnit [0..116] (1:1)
├── GlobalStatement [0..17] (1:1)
│   └── LocalDeclaration [0..17] (1:1)
│       ├── VariableDeclaration [0..17] (1:1)
│       │   ├── LetKeyword "let" [0..3] (1:1)
│       │   │   [Trailing Trivia] WhitespaceTrivia: " "
│       │   └── VariableDeclarator [4..17] (1:5)
... (truncated for brevity)
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
    ])
    .AnalyzeCodeTemp(); // Temporary

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
    ])
    .AnalyzeCodeTemp(); // Temporary

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