using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroExpansionResultTests
{
    [Fact]
    public void FromReplacement_CreatesReplacementWithIntroducedAndPeerDeclarations()
    {
        var replacement = ParseMember("func Replacement() -> unit { }");
        var introduced = ParseMember("func Introduced() -> unit { }");
        var peer = ParseMember("func Peer() -> unit { }");

        var result = MacroExpansionResult.FromReplacement(
            replacement,
            [introduced],
            [peer]);

        result.ReplacementDeclaration.ShouldBeSameAs(replacement);
        result.IntroducedMembers.Length.ShouldBe(1);
        result.IntroducedMembers[0].ShouldBeSameAs(introduced);
        result.PeerDeclarations.Length.ShouldBe(1);
        result.PeerDeclarations[0].ShouldBeSameAs(peer);
        result.Diagnostics.ShouldBe(ImmutableArray<Diagnostic>.Empty);
        result.MacroDiagnostics.ShouldBe(ImmutableArray<MacroExpansionDiagnostic>.Empty);
    }

    [Fact]
    public void FromDiagnostic_CreatesDiagnosticOnlyResult()
    {
        var diagnostic = MacroExpansionDiagnostic.Error("Expansion failed.");

        var result = MacroExpansionResult.FromDiagnostic(diagnostic);

        result.ReplacementDeclaration.ShouldBeNull();
        result.IntroducedMembers.ShouldBeEmpty();
        result.PeerDeclarations.ShouldBeEmpty();
        result.Diagnostics.ShouldBeEmpty();
        result.MacroDiagnostics.Length.ShouldBe(1);
        result.MacroDiagnostics[0].ShouldBeSameAs(diagnostic);
    }

    private static MemberDeclarationSyntax ParseMember(string source)
        => SyntaxFactory.ParseSyntaxTree(source).GetRoot().Members.Single();
}
