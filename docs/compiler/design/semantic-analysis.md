# Semantic Analysis

Semantic analysis determines the meaning behind syntax. This task is shared between a `Compilation` and various `SemanticModel` objects.

The semantic APIs come into play during the code generation stage.

## Symbol

A symbol represents a unit of meaning in a program, indicating an abstract element of the code. Often, it denotes what a particular syntax node means.  

When a syntax node in the code corresponds to a specific symbol, we call this connection a "binding."

For example, an `IMethodSymbol` may be declared by a `MethodDeclarationSyntax`. Meanwhile, an `ExpressionSyntax` might bind to a particular type or method, revealing how that expression should be interpreted and ultimately compiled.

## Compilation

A `Compilation` encapsulates the overall context of a codebase being compiled, including shared references, namespaces, and types.

## Semantic Model

A `SemanticModel` manages statement- and expression-level context for a given `SyntaxTree`. It determines how certain nodes within the tree map to specific symbols. This model is created on demand for each syntax tree.

### Symbol Resolution and Binding

During semantic analysis, various syntax nodes—particularly expressions—are associated with the symbols that represent their meaning.

#### Example: Simple Resolution

Below is a simplified illustration of how a method might resolve symbols:

```csharp
void AnalyzeExpression(ExpressionSyntax expression, out ImmutableArray<ISymbol> symbols) 
{
    ISymbol resolvedSymbol = ...;

    symbols = [resolvedSymbol];

    Bind(expressionSyntax, [resolvedSymbol]);
}
```

Here, the `symbols` array conveys the type or symbol that the expression resolves to. Using an `ImmutableArray<ISymbol>` allows for multiple symbols to be returned in cases of ambiguity.

#### Example: More complex scenario

Consider a scenario involving a binary expression, where the symbol resolution and the final set of symbols differ:

```csharp
void AnalyzeBinaryExpression(BinaryExpressionSyntax binaryExpression, out ImmutableArray<ISymbol> symbols) 
{
    // Suppose the binary expression, combining two operands, maps to a method call:

    // SyntaxKind: AddExpression

    // Example: "Hello, " + "World!"
    // becomes string.Concat("Hello, " + "World!")

    // Identify the appropriate method overload for "string.Concat"
    IMethodSymbol resolvedMethod = /* Omitted */;

    // Return only the symbol corresponding to the result type of this binary operation
    symbols = [resolvedMethod.ReturnType];

    // Use Bind to link the expression with the resolved method
    Bind(expressionSyntax, [resolvedMethod]);
}
```

### Querying Symbols for Bindings

You can query a `SemanticModel` for the symbol associated with a particular syntax node. Below is a quick example, assuming some familiarity with the structure of the parsed code:

```csharp
SyntaxTree syntaxTree = SyntaxFactory.ParseSyntaxTree(
    """
    Console.WriteLine("x" + "2");
    """);

Compilation compilation = /* Omitted */;

SemanticModel semanticModel = compilation.GetSemanticModel(syntaxTree);

var tree = compilation.SyntaxTrees.First();

// Retrieve the BinaryExpression node
var binaryExpression = tree.GetRoot()
    .DescendantNodes()
    .OfType<BinaryExpression>()
    .First();

// Obtain symbol information
var symbolInfo = semanticModel.GetSymbol(binaryExpression);

// Extract the IMethodSymbol that represents the underlying "Concat"
var symbol = symbolInfo.Symbol as IMethodSymbol;
```

In this example, `GetSymbol(binaryExpression)` retrieves the symbol (or symbols) describing the meaning of the `BinaryExpression` node—here, specifically, the overload of `string.Concat`.