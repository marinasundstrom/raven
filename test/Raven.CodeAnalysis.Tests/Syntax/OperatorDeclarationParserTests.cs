using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class OperatorDeclarationParserTests : DiagnosticTestBase
{
    [Fact]
    public void OperatorDeclaration_InClass_ParsesSignature()
    {
        var source = """
            class NumberBox {
                public static operator +(left: NumberBox, right: NumberBox) -> NumberBox {
                    return left;
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var declaration = tree.GetRoot().DescendantNodes().OfType<OperatorDeclarationSyntax>().Single();

        Assert.Equal(SyntaxKind.OperatorKeyword, declaration.OperatorKeyword.Kind);
        Assert.Equal(SyntaxKind.PlusToken, declaration.OperatorToken.Kind);
        Assert.Equal(2, declaration.ParameterList.Parameters.Count);
        Assert.NotNull(declaration.ReturnType);
        Assert.NotNull(declaration.Body);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void OperatorDeclaration_InExtension_ParsesExpressionBody()
    {
        var source = """
            extension BoxOps for Box {
                public static operator -(value: Box) -> Box => value
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var declaration = tree.GetRoot().DescendantNodes().OfType<OperatorDeclarationSyntax>().Single();

        Assert.Equal(SyntaxKind.OperatorKeyword, declaration.OperatorKeyword.Kind);
        Assert.Equal(SyntaxKind.MinusToken, declaration.OperatorToken.Kind);
        Assert.Single(declaration.ParameterList.Parameters);
        Assert.NotNull(declaration.ExpressionBody);
        Assert.Empty(tree.GetDiagnostics());
    }
}
