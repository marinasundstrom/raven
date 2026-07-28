using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Syntax;

public sealed class MacroFunctionDeclarationParsingTests
{
    [Fact]
    public void MacroFunctionDeclaration_ParsesAsTopLevelMember()
    {
        var tree = SyntaxTree.ParseText("""
            macro func Compile<TDelegate>(body: Expression) -> TDelegate
                where TDelegate: Delegate
            {
                return body
            }
            """);

        var declaration = Assert.IsType<MacroFunctionDeclarationSyntax>(
            Assert.Single(tree.GetRoot().Members));

        Assert.Equal(SyntaxKind.IdentifierToken, declaration.MacroKeyword.Kind);
        Assert.Equal("macro", declaration.MacroKeyword.ValueText);
        Assert.Equal(SyntaxKind.FuncKeyword, declaration.FuncKeyword.Kind);
        Assert.Equal("Compile", declaration.Identifier.ValueText);
        Assert.Equal("TDelegate", Assert.Single(declaration.TypeParameterList!.Parameters).Identifier.ValueText);
        Assert.Equal("body", Assert.Single(declaration.ParameterList.Parameters).Identifier.ValueText);
        Assert.Equal("TDelegate", declaration.ReturnType!.Type.ToString());
        Assert.Single(declaration.ConstraintClauses);
        Assert.NotNull(declaration.Body);
        Assert.Null(declaration.ExpressionBody);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MacroFunctionDeclaration_ParsesInsideNamespace()
    {
        var tree = SyntaxTree.ParseText("""
            namespace Tools {
                macro func Quote(body: Expression) -> Expression => body
            }
            """);

        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroFunctionDeclarationSyntax>()
            .Single();

        Assert.Equal("Quote", declaration.Identifier.ValueText);
        Assert.Null(declaration.Body);
        Assert.NotNull(declaration.ExpressionBody);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MacroFunctionDeclaration_PreservesAttributesAndModifiers()
    {
        var tree = SyntaxTree.ParseText("""
            [Obsolete]
            public macro func Legacy(body: Expression) -> Expression {
                return body
            }
            """);

        var declaration = Assert.IsType<MacroFunctionDeclarationSyntax>(
            Assert.Single(tree.GetRoot().Members));

        Assert.Single(declaration.AttributeLists);
        Assert.Contains(declaration.Modifiers, static modifier => modifier.Kind == SyntaxKind.PublicKeyword);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MacroIdentifier_RemainsAvailableOutsideDeclarationLookahead()
    {
        var tree = SyntaxTree.ParseText("""
            let macro = 42
            """);

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<MacroFunctionDeclarationSyntax>());
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MacroFunctionDeclaration_MissingNameProducesRecoveredNode()
    {
        var tree = SyntaxTree.ParseText("""
            macro func () -> Expression {
            }
            """);

        var declaration = Assert.IsType<MacroFunctionDeclarationSyntax>(
            Assert.Single(tree.GetRoot().Members));

        Assert.True(declaration.Identifier.IsMissing);
        Assert.Contains(tree.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAV1001");
    }

    [Fact]
    public void MacroFunctionDeclaration_ClassifiesContextualKeywordAndName()
    {
        var tree = SyntaxTree.ParseText("""
            macro func Compile(body: Expression) -> Expression {
                return body
            }
            """);
        var declaration = Assert.IsType<MacroFunctionDeclarationSyntax>(
            Assert.Single(tree.GetRoot().Members));

        var classifications = SemanticClassifier.Classify(tree.GetRoot());

        Assert.Equal(
            SemanticClassification.Keyword,
            classifications.Tokens[declaration.MacroKeyword]);
        Assert.Equal(
            SemanticClassification.Method,
            classifications.Tokens[declaration.Identifier]);
    }
}
