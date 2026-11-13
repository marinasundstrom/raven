using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics.Macros;

[Trait("Target", "Shared")]
public class MacroExpansionResultBuilderTests
{
    [Fact]
    public void ExpandCompleteFreestandingResult_MergesContributionsAndUsesLatestExpression()
    {
        var builder = new MacroExpansionResultBuilder();
        var diagnostic = new MacroExpansionDiagnostic(
            DiagnosticSeverity.Warning,
            "warning",
            Location.None);
        builder.Expand(new FreestandingMacroExpansionResult
        {
            Expression = SyntaxFactory.ParseExpression("1"),
            MacroDiagnostics = [diagnostic]
        });
        builder.Expand(SyntaxFactory.ParseExpression("2"));

        var result = builder.BuildFreestanding();

        Assert.Equal("2", result.Expression!.ToString());
        Assert.Same(diagnostic, Assert.Single(result.MacroDiagnostics));
    }

    [Fact]
    public void ExpandCompleteAttachedResult_MergesAllContributionKinds()
    {
        var builder = new MacroExpansionResultBuilder();
        var replacement = SyntaxFactory.ParseSyntaxTree("class Replacement {}").GetRoot()
            .DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var introduced = SyntaxFactory.ParseSyntaxTree("class Introduced {}").GetRoot()
            .DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var peer = SyntaxFactory.ParseSyntaxTree("class Peer {}").GetRoot()
            .DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var diagnostic = new MacroExpansionDiagnostic(
            DiagnosticSeverity.Error,
            "error",
            Location.None);
        builder.Expand(new MacroExpansionResult
        {
            ReplacementDeclaration = replacement,
            IntroducedMembers = [introduced],
            PeerDeclarations = [peer],
            MacroDiagnostics = [diagnostic]
        });

        var result = builder.BuildAttached();

        Assert.Same(replacement, result.ReplacementDeclaration);
        Assert.Same(introduced, Assert.Single(result.IntroducedMembers));
        Assert.Same(peer, Assert.Single(result.PeerDeclarations));
        Assert.Same(diagnostic, Assert.Single(result.MacroDiagnostics));
    }

    [Fact]
    public void LatestFreestandingExpansionSelectsOneCardinality()
    {
        var builder = new MacroExpansionResultBuilder();
        var member = SyntaxFactory.ParseSyntaxTree("class Generated {}").GetRoot().Members.Single();

        builder.Expand(SyntaxFactory.List([member]));
        builder.Expand(SyntaxFactory.ParseExpression("42"));
        var expressionResult = builder.BuildFreestanding();

        Assert.False(expressionResult.HasMemberExpansion);
        Assert.NotNull(expressionResult.Expression);

        builder.Expand(SyntaxFactory.List([member]));
        var memberResult = builder.BuildFreestanding();

        Assert.True(memberResult.HasMemberExpansion);
        Assert.Null(memberResult.Node);
        Assert.Equal(member.ToString(), Assert.Single(memberResult.Members).ToString());
    }


    [Fact]
    public void BuildAttached_SnapshotsContributionsWithoutFreezingBuilder()
    {
        var first = SyntaxFactory.ParseSyntaxTree("class First {}").GetRoot().Members.Single();
        var second = SyntaxFactory.ParseSyntaxTree("class Second {}").GetRoot().Members.Single();
        var builder = new MacroExpansionResultBuilder();
        builder.Introduce(first);
        var earlier = builder.BuildAttached();
        builder.Introduce(second);
        var later = builder.BuildAttached();

        Assert.Same(first, Assert.Single(earlier.IntroducedMembers));
        Assert.Equal(new[] { first, second }, later.IntroducedMembers);
        Assert.NotSame(earlier, later);
    }

    [Fact]
    public void ExpandNode_ReplacesPreviouslySelectedMemberList()
    {
        var builder = new MacroExpansionResultBuilder();
        builder.Expand(SyntaxFactory.List<MemberDeclarationSyntax>());
        var expression = SyntaxFactory.ParseExpression("42");
        builder.Expand(expression);
        var result = builder.BuildFreestanding();

        Assert.Same(expression, result.Node);
        Assert.False(result.HasMemberExpansion);
        Assert.Empty(result.Members);
    }
}
