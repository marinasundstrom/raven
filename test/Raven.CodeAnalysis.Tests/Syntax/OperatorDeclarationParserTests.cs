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
                static func +(left: NumberBox, right: NumberBox) -> NumberBox {
                    return left;
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var declaration = tree.GetRoot().DescendantNodes().OfType<OperatorDeclarationSyntax>().Single();

        Assert.Equal(SyntaxKind.FuncKeyword, declaration.FuncKeyword.Kind);
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
                static func -(value: Box) -> Box => value
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var declaration = tree.GetRoot().DescendantNodes().OfType<OperatorDeclarationSyntax>().Single();

        Assert.Equal(SyntaxKind.FuncKeyword, declaration.FuncKeyword.Kind);
        Assert.Equal(SyntaxKind.MinusToken, declaration.OperatorToken.Kind);
        Assert.Single(declaration.ParameterList.Parameters);
        Assert.NotNull(declaration.ExpressionBody);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void ConversionOperatorDeclaration_InClass_ParsesSignature()
    {
        var source = """
            class NumberBox {
                static func explicit(value: NumberBox) -> int => 42
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ConversionOperatorDeclarationSyntax>().Single();

        Assert.Equal(SyntaxKind.ExplicitKeyword, declaration.ConversionKindKeyword.Kind);
        Assert.Equal(SyntaxKind.FuncKeyword, declaration.FuncKeyword.Kind);
        Assert.Single(declaration.ParameterList.Parameters);
        Assert.NotNull(declaration.ReturnType);
        Assert.NotNull(declaration.ExpressionBody);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FuncStyleOperatorDeclaration_InClass_ParsesAsOperatorDeclaration()
    {
        var source = """
            class NumberBox {
                static func ==(left: NumberBox, right: NumberBox) -> bool => true
                static func !=(left: NumberBox, right: NumberBox) -> bool => false
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var declarations = tree.GetRoot().DescendantNodes().OfType<OperatorDeclarationSyntax>().ToArray();

        Assert.Equal(2, declarations.Length);
        Assert.Equal(SyntaxKind.EqualsEqualsToken, declarations[0].OperatorToken.Kind);
        Assert.Equal(SyntaxKind.NotEqualsToken, declarations[1].OperatorToken.Kind);
        Assert.All(declarations, declaration => Assert.NotNull(declaration.ExpressionBody));
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FuncStyleOperatorDeclaration_InExtension_ParsesAsOperatorDeclaration()
    {
        var source = """
            extension NumberBoxOps for NumberBox {
                static func +(left: NumberBox, right: NumberBox) -> NumberBox => left
            }

            class NumberBox {}
            """;

        var tree = SyntaxTree.ParseText(source);
        var declaration = tree.GetRoot().DescendantNodes().OfType<OperatorDeclarationSyntax>().Single();

        Assert.Equal(SyntaxKind.PlusToken, declaration.OperatorToken.Kind);
        Assert.NotNull(declaration.ExpressionBody);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FuncStyleConversionOperatorDeclaration_InClass_ParsesAsConversionOperatorDeclaration()
    {
        var source = """
            class NumberBox {
                static func implicit(value: int) -> NumberBox => NumberBox()
                static func explicit(value: NumberBox) -> int => 42
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var declarations = tree.GetRoot().DescendantNodes().OfType<ConversionOperatorDeclarationSyntax>().ToArray();

        Assert.Equal(2, declarations.Length);
        Assert.Equal(SyntaxKind.ImplicitKeyword, declarations[0].ConversionKindKeyword.Kind);
        Assert.Equal(SyntaxKind.ExplicitKeyword, declarations[1].ConversionKindKeyword.Kind);
        Assert.All(declarations, declaration => Assert.NotNull(declaration.ReturnType));
        Assert.All(declarations, declaration => Assert.NotNull(declaration.ExpressionBody));
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FuncStyleConversionOperatorDeclaration_InExtension_ParsesAsConversionOperatorDeclaration()
    {
        var source = """
            extension NumberBoxConversions for NumberBox {
                static func implicit(value: int) -> NumberBox => NumberBox()
            }

            class NumberBox {}
            """;

        var tree = SyntaxTree.ParseText(source);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ConversionOperatorDeclarationSyntax>().Single();

        Assert.Equal(SyntaxKind.ImplicitKeyword, declaration.ConversionKindKeyword.Kind);
        Assert.NotNull(declaration.ReturnType);
        Assert.NotNull(declaration.ExpressionBody);
        Assert.Empty(tree.GetDiagnostics());
    }
}
