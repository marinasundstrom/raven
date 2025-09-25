using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class GenericMethodTests : CompilationTestBase
{
    [Fact]
    public void GenericMethod_ExposesTypeParametersAndArguments()
    {
        var source = """
            class Container
            {
                static identity<T>(value: T) -> T
                {
                    return value;
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var methodDeclaration = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodDeclaration)!;

        Assert.True(methodSymbol.IsGenericMethod);
        Assert.Single(methodSymbol.TypeParameters);
        Assert.Equal("T", methodSymbol.TypeParameters[0].Name);
        Assert.Same(methodSymbol.TypeParameters[0], methodSymbol.TypeArguments[0]);
        Assert.Same(methodSymbol.TypeParameters[0], methodSymbol.ReturnType);
        Assert.Same(methodSymbol.TypeParameters[0], methodSymbol.Parameters[0].Type);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void GenericMethodInvocation_WithExplicitTypeArguments_BindsConstructedMethod()
    {
        var source = """
            class Container
            {
                static identity<T>(value: T) -> T
                {
                    return value;
                }

                static call() -> int
                {
                    return identity<int>(1);
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        Assert.True(method.IsGenericMethod);
        Assert.Equal("identity", method.Name);
        Assert.Single(method.TypeArguments);
        Assert.Same(intType, method.TypeArguments[0]);
        Assert.Same(intType, method.ReturnType);
        Assert.Same(intType, method.Parameters[0].Type);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void GenericMethodInvocation_WithImplicitTypeArguments_BindsConstructedMethod()
    {
        var source = """
            class Container
            {
                static identity<T>(value: T) -> T
                {
                    return value;
                }

                static call() -> int
                {
                    return identity(1);
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        Assert.True(method.IsGenericMethod);
        Assert.Equal("identity", method.Name);
        Assert.Single(method.TypeArguments);
        Assert.Same(intType, method.TypeArguments[0]);
        Assert.Same(intType, method.ReturnType);
        Assert.Same(intType, method.Parameters[0].Type);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void GenericMethodInvocation_InfersFromConstructedGenericParameter()
    {
        var source = """
            class Box<T>
            {
                public Value: T { get; }

                public init(value: T)
                {
                    self.Value = value;
                }
            }

            class Container
            {
                static unwrap<T>(box: Box<T>) -> T
                {
                    return box.Value;
                }

                static call() -> int
                {
                    let box = Box<int>(1);
                    return unwrap(box);
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var method = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        Assert.True(method.IsGenericMethod);
        Assert.Equal("unwrap", method.Name);
        Assert.Single(method.TypeArguments);
        Assert.Same(intType, method.TypeArguments[0]);
        Assert.Same(intType, method.ReturnType);
        Assert.Same(intType, ((INamedTypeSymbol)method.Parameters[0].Type).TypeArguments[0]);
        Assert.Empty(compilation.GetDiagnostics());
    }
}
