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

    [Fact]
    public void MacroFunctionDeclaration_ParsesNamedTargetClause()
    {
        var tree = SyntaxTree.ParseText("""
            macro func Observable(enabled: bool) on property: Property {
                replace property
                if enabled {
                    introduce CreateBackingField(property)
                }
            }
            """);

        var declaration = Assert.IsType<MacroFunctionDeclarationSyntax>(
            Assert.Single(tree.GetRoot().Members));
        var target = Assert.IsType<MacroTargetClauseSyntax>(declaration.TargetClause);

        Assert.Equal("on", target.OnKeyword.ValueText);
        Assert.Equal("property", target.Identifier.ValueText);
        Assert.Equal(SyntaxKind.ColonToken, target.ColonToken.Kind);
        Assert.Equal("Property", target.Target.ToString());

        var contributions = declaration.Body!
            .DescendantNodes()
            .OfType<MacroExpansionStatementSyntax>()
            .ToArray();
        Assert.Equal(["replace", "introduce"], contributions.Select(static x => x.Keyword.ValueText));

        var classifications = SemanticClassifier.Classify(declaration);
        Assert.Equal(SemanticClassification.Keyword, classifications.Tokens[target.OnKeyword]);
        Assert.Equal(SemanticClassification.Parameter, classifications.Tokens[target.Identifier]);
        Assert.All(
            contributions,
            contribution => Assert.Equal(
                SemanticClassification.Keyword,
                classifications.Tokens[contribution.Keyword]));
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MacroFunctionDeclaration_ParsesShorthandTargetClause()
    {
        var tree = SyntaxTree.ParseText("""
            macro func AddEquatable() on Type {
                introduce CreateMembers()
            }
            """);

        var declaration = Assert.IsType<MacroFunctionDeclarationSyntax>(
            Assert.Single(tree.GetRoot().Members));
        var target = Assert.IsType<MacroTargetClauseSyntax>(declaration.TargetClause);

        Assert.Equal(SyntaxKind.None, target.Identifier.Kind);
        Assert.Equal(SyntaxKind.None, target.ColonToken.Kind);
        Assert.Equal("Type", target.Target.ToString());
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void ExpansionWords_RemainIdentifiersOutsideMacroFunctions()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() {
                expand(value)
                replace(value)
                introduce(value)
            }
            """);

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<MacroExpansionStatementSyntax>());
        Assert.Empty(tree.GetDiagnostics());
    }
}
