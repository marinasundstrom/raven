using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Syntax;

public sealed class MacroDeclarationParsingTests
{
    [Fact]
    public void MacroDeclaration_ParsesAsTopLevelMember()
    {
        var tree = SyntaxTree.ParseText("""
            macro Compile<TDelegate>(body: ExpressionSyntax) -> TDelegate
                where TDelegate: Delegate
            {
                return body
            }
            """);

        var declaration = Assert.Single(
            tree.GetRoot().Members.OfType<MacroDeclarationSyntax>());

        Assert.Equal(SyntaxKind.IdentifierToken, declaration.MacroKeyword.Kind);
        Assert.Equal("macro", declaration.MacroKeyword.ValueText);
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
    public void MacroDeclaration_ParsesInsideNamespace()
    {
        var tree = SyntaxTree.ParseText("""
            namespace Tools {
                macro Quote(body: ExpressionSyntax) -> ExpressionSyntax => body
            }
            """);

        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .Single();

        Assert.Equal("Quote", declaration.Identifier.ValueText);
        Assert.Null(declaration.Body);
        Assert.NotNull(declaration.ExpressionBody);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MacroDeclaration_PreservesAttributesAndModifiers()
    {
        var tree = SyntaxTree.ParseText("""
            [Obsolete]
            public macro Legacy(body: ExpressionSyntax) -> ExpressionSyntax {
                return body
            }
            """);

        var declaration = Assert.IsType<MacroDeclarationSyntax>(
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
            macro + 1
            """);

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<MacroDeclarationSyntax>());
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MacroDeclaration_MissingNameProducesRecoveredNode()
    {
        var tree = SyntaxTree.ParseText("""
            macro () -> Expression {
            }
            """);

        var declaration = Assert.Single(
            tree.GetRoot().Members.OfType<MacroDeclarationSyntax>());

        Assert.True(declaration.Identifier.IsMissing);
        Assert.Contains(tree.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAV1001");
    }

    [Fact]
    public void LegacyMacroFuncSpelling_IsRejected()
    {
        var tree = SyntaxTree.ParseText("macro func Legacy() {}");

        Assert.Empty(tree.GetRoot().Members.OfType<MacroDeclarationSyntax>());
        Assert.NotEmpty(tree.GetDiagnostics());
    }

    [Fact]
    public void MacroDeclaration_ClassifiesContextualKeywordAndName()
    {
        var tree = SyntaxTree.ParseText("""
            macro Compile(body: ExpressionSyntax) -> ExpressionSyntax {
                return body
            }
            """);
        var declaration = Assert.IsType<MacroDeclarationSyntax>(
            Assert.Single(tree.GetRoot().Members));

        var classifications = SemanticClassifier.Classify(tree.GetRoot());

        Assert.Equal(
            SemanticClassification.Keyword,
            classifications.Tokens[declaration.MacroKeyword]);
        Assert.Equal(
            SemanticClassification.Macro,
            classifications.Tokens[declaration.Identifier]);
    }

    [Fact]
    public void MacroDeclaration_ParsesTypedTargetParameter()
    {
        var tree = SyntaxTree.ParseText("""
            macro Observable(enabled: bool, on property: PropertyDeclarationSyntax) {
                replace property
                if enabled {
                    introduce CreateBackingField(property)
                }
            }
            """);

        var declaration = Assert.IsType<MacroDeclarationSyntax>(
            Assert.Single(tree.GetRoot().Members));
        var target = declaration.ParameterList.Parameters[1];

        Assert.Equal("on", target.OnKeyword.ValueText);
        Assert.Equal("property", target.Identifier.ValueText);
        Assert.Equal(SyntaxKind.ColonToken, target.TypeAnnotation!.ColonToken.Kind);
        Assert.Equal("PropertyDeclarationSyntax", target.TypeAnnotation.Type.ToString());

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
    public void MacroDeclaration_ParsesTargetAsOnlyParameter()
    {
        var tree = SyntaxTree.ParseText("""
            macro AddEquatable(on target: BaseTypeDeclarationSyntax) {
                introduce CreateMembers()
            }
            """);

        var declaration = Assert.IsType<MacroDeclarationSyntax>(
            Assert.Single(tree.GetRoot().Members));
        var target = Assert.Single(declaration.ParameterList.Parameters);

        Assert.Equal("on", target.OnKeyword.ValueText);
        Assert.Equal("target", target.Identifier.ValueText);
        Assert.Equal("BaseTypeDeclarationSyntax", target.TypeAnnotation!.Type.ToString());
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MacroDeclaration_TrailingTargetClauseIsNoLongerAccepted()
    {
        var tree = SyntaxTree.ParseText("""
            macro Legacy() on Type {
                introduce target
            }
            """);

        var declaration = Assert.IsType<MacroDeclarationSyntax>(
            Assert.Single(tree.GetRoot().Members.OfType<MacroDeclarationSyntax>()));

        Assert.Empty(declaration.ParameterList.Parameters);
        Assert.NotEmpty(tree.GetDiagnostics());
    }

    [Fact]
    public void MacroDeclaration_ParsesTokenStreamInputAsParameter()
    {
        var tree = SyntaxTree.ParseText("""
            macro FirstTokenLength(offset: int, tokens: IMacroTokenStream) {
                let token = tokens.ReadToken()
                expand ParseExpression((token.Text.Length + offset).ToString())
            }
            """);

        var declaration = Assert.IsType<MacroDeclarationSyntax>(
            Assert.Single(tree.GetRoot().Members));
        var parameter = declaration.ParameterList.Parameters[1];

        var classifications = SemanticClassifier.Classify(declaration);
        Assert.Equal("tokens", parameter.Identifier.ValueText);
        Assert.Equal("IMacroTokenStream", parameter.TypeAnnotation!.Type.ToString());
        Assert.Equal(SemanticClassification.Parameter, classifications.Tokens[parameter.Identifier]);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MacroDeclaration_ParsesFragmentContribution()
    {
        var tree = SyntaxTree.ParseText("""
            macro Template(context: TokenTreeMacroContext) {
                token context.CreateTokenInfo(next)
                fragment context.CreateFragmentRegion(kind, span)
                expand context.ParseExpression(span)
            }
            """);

        var declaration = Assert.IsType<MacroDeclarationSyntax>(
            Assert.Single(tree.GetRoot().Members));
        var contributions = declaration.Body!
            .DescendantNodes()
            .OfType<MacroExpansionStatementSyntax>()
            .ToArray();

        Assert.Equal(["token", "fragment", "expand"], contributions.Select(static contribution => contribution.Keyword.ValueText));
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void ExpansionWords_RemainIdentifiersOutsideMacros()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() {
                expand(value)
                replace(value)
                introduce(value)
                fragment(value)
                token(value)
            }
            """);

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<MacroExpansionStatementSyntax>());
        Assert.Empty(tree.GetDiagnostics());
    }
}
