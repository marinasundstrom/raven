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
        Assert.Equal(TypeParameterOwnerKind.Method, methodSymbol.TypeParameters[0].OwnerKind);
        Assert.Same(methodSymbol, methodSymbol.TypeParameters[0].DeclaringMethodParameterOwner);
        Assert.Null(methodSymbol.TypeParameters[0].DeclaringTypeParameterOwner);
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
    public void GenericMethodInvocation_NullArgument_DoesNotInferTypeArgument()
    {
        var source = """
            class C
            {
                static f<T>(value: T) -> ()
                {
                }

                static test() -> ()
                {
                    f(null);
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.NoOverloadForMethod);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);
    }

    [Fact]
    public void GenericMethodInvocation_ConstraintFailure_DoesNotReportNameMissing()
    {
        var source = """
            import System.*

            f2<bool?>(null)

            func f2<T>(t: T) -> ()
                where T: notnull {
                Console.WriteLine(t)
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.NoOverloadForMethod);
    }
}
