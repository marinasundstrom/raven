using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Binding;

public sealed class NodeInterestBindingTests
{
    [Fact]
    public void GetSymbolInfo_ResolvesLocalFromSyntaxNodeWithoutPrecomputingDiagnostics()
    {
        const string source = """
class C {
    func Run() -> int {
        val count = 42
        return count
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var identifier = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "count");

        var symbol = model.GetSymbolInfo(identifier).Symbol;

        var local = Assert.IsAssignableFrom<ILocalSymbol>(symbol);
        Assert.Equal("count", local.Name);
    }

    [Fact]
    public void GetTypeInfo_ResolvesLocalReferenceTypeWithoutPrecomputingDiagnostics()
    {
        const string source = """
class C {
    func Run() -> int {
        val count = 42
        return count
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var identifier = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "count");

        var typeInfo = model.GetTypeInfo(identifier);

        Assert.Equal(SpecialType.System_Int32, typeInfo.Type?.SpecialType);
        Assert.Equal(SpecialType.System_Int32, typeInfo.ConvertedType?.SpecialType);
    }

    [Fact]
    public void GetSymbolInfo_ResolvesParameterReceiverFromNodeInterestPath()
    {
        const string source = """
class Counter {
    func Increment() -> () { }
}

class C {
    func Run(counter: Counter) -> () {
        counter.Increment()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var identifier = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "counter");

        var symbol = model.GetSymbolInfo(identifier).Symbol;

        var parameter = Assert.IsAssignableFrom<IParameterSymbol>(symbol);
        Assert.Equal("counter", parameter.Name);
    }

    [Fact]
    public void GetTypeInfo_OnFunctionExpression_DoesNotUseNodeInterestBinderFallback()
    {
        const string source = """
class C {
    func Run() -> () {
        val project = (x: int) => x
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var functionExpression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();

        var typeInfo = model.GetTypeInfo(functionExpression);

        Assert.NotNull(typeInfo.Type ?? typeInfo.ConvertedType);
    }

    [Fact]
    public void TryGetFunctionExpressionDelegateType_ResolvesContextualDelegateWithoutFullDiagnostics()
    {
        const string source = """
class C {
    func Run() -> () {
        val project: (int) -> int = (x) => x
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var functionExpression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();

        var resolved = model.TryGetFunctionExpressionDelegateType(functionExpression, out var delegateType);

        Assert.True(resolved);
        Assert.NotNull(delegateType);
        Assert.Equal(TypeKind.Delegate, delegateType!.TypeKind);
        Assert.Equal(delegateType, model.GetTypeInfo(functionExpression).Type);
    }

    [Fact]
    public void GetSymbolInfo_OnNamedFunctionExpression_ResolvesLambdaMethodFromNodeInterestPath()
    {
        const string source = """
class C {
    func Run() -> () {
        val project = func Step(n: int) -> int => n
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var functionExpression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();

        var symbol = model.GetSymbolInfo(functionExpression).Symbol;

        var method = Assert.IsAssignableFrom<IMethodSymbol>(symbol);
        Assert.Equal(MethodKind.LambdaMethod, method.MethodKind);
        Assert.Same(functionExpression.SyntaxTree, method.DeclaringSyntaxReferences.Single().SyntaxTree);
    }

    [Fact]
    public void GetDeclaredSymbol_OnNamedRecursiveFunctionExpression_DoesNotRecurseThroughContainingOwner()
    {
        const string source = """
class C {
    func Run() -> int {
        val compute = func Step(n: int) -> int {
            if n < 1
                0
            else
                Step(n - 1)
        }

        compute(3)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "node-interest-binding-tests",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);
        var functionExpression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedFunctionExpressionSyntax>()
            .Single();

        var symbol = model.GetDeclaredSymbol(functionExpression);

        var method = Assert.IsAssignableFrom<IMethodSymbol>(symbol);
        Assert.Equal(MethodKind.LambdaMethod, method.MethodKind);
        Assert.Equal("Step", functionExpression.Identifier.ValueText);
    }
}
