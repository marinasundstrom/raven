using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics.Macros;

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

}
