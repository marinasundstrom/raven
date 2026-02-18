using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NamedArgumentTests : CompilationTestBase
{
    [Fact]
    public void Invocation_WithNamedArguments_BindsToMatchingParameters()
    {
        var source = """
        class C {
            static sum(x: int, y: int) -> int { x + y }

            static test() -> int {
                return sum(y: 2, x: 1);
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var invocation = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var arguments = boundInvocation.Arguments.ToArray();

        Assert.Equal(2, arguments.Length);
        var first = Assert.IsType<BoundLiteralExpression>(arguments[0]);
        var second = Assert.IsType<BoundLiteralExpression>(arguments[1]);
        Assert.Equal(1, first.Value);
        Assert.Equal(2, second.Value);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void Invocation_WithMixedNamedAndPositionalArguments_Binds()
    {
        var source = """
        class C {
            static combine(a: int, b: int, c: int) -> int { a + b + c }

            static test() -> int {
                return combine(a: 1, 2, c: 3);
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var invocation = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var arguments = boundInvocation.Arguments.ToArray();

        Assert.Equal(3, arguments.Length);
        var values = arguments
            .Select(a => Assert.IsType<BoundLiteralExpression>(a).Value)
            .ToArray();
        Assert.Equal(new object[] { 1, 2, 3 }, values);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void Invocation_WithOutOfOrderPositionalAfterNamed_ReportsDiagnostic()
    {
        var source = """
        class C {
            static combine(a: int, b: int, c: int) -> int { a + b + c }

            static test() -> int {
                return combine(c: 3, 1, b: 2);
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.NoOverloadForMethod, diagnostic.Descriptor);
    }
}
