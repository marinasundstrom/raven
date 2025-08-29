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

During semantic analysis, various syntax nodes — particularly expressions — are associated, or bound, with the symbols that represent their meaning.

The actual semantic binding is performed by Binders. They are specialized classes that traverse the syntax tree and resolve symbols based on the context of the code. Each binder is responsible for a specific scope, such as a method or class, and it uses the `SemanticModel` to resolve symbols. If a particular symbol is not found in the current scope, the binder will look up the symbol in its parent scopes until it finds a match or determines that the symbol is not defined.

The BlockBinder, for example, is responsible for binding local variables and parameters within a method or block of code. It uses the `SemanticModel` to resolve symbols and check for any potential conflicts or errors.

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

### Expression vs. imperative contexts

Raven is an expression-oriented language where constructs like `if`, `while`, and
`for` normally yield values. In statement position, these control-flow forms have
their own statement syntax (e.g. `IfStatementSyntax`). When an expression form
appears in a statement position—such as a block used directly—the binder rewrites
it into a dedicated statement node (`BoundIfStatement`, `BoundWhileStatement`,
`BoundForStatement`). These nodes capture the imperative intent and ensure any
produced values are discarded.

`BoundIfStatement` is flexible in its branches: each branch may be either a
`BoundStatement` (such as a `return` or another `if`) or a `BoundExpression`. If an
expression branch appears, its value is ignored. Block expressions may also appear
directly as branches; they permit nested statements but do not allow imperative
statement forms on their own.

This distinction between expression and imperative contexts allows the semantic
analysis phase to reason precisely about side effects and control flow while
preserving Raven's expression-first design.