using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public sealed class MacroExpansionResult
{
    public static MacroExpansionResult Empty { get; } = new();

    public static MacroExpansionResult FromReplacement(SyntaxNode replacementDeclaration)
    {
        ArgumentNullException.ThrowIfNull(replacementDeclaration);
        return new MacroExpansionResult
        {
            ReplacementDeclaration = replacementDeclaration
        };
    }

    public static MacroExpansionResult FromReplacement(
        SyntaxNode replacementDeclaration,
        ImmutableArray<MemberDeclarationSyntax> introducedMembers)
    {
        ArgumentNullException.ThrowIfNull(replacementDeclaration);
        return new MacroExpansionResult
        {
            ReplacementDeclaration = replacementDeclaration,
            IntroducedMembers = Normalize(introducedMembers)
        };
    }

    public static MacroExpansionResult FromReplacement(
        SyntaxNode replacementDeclaration,
        ImmutableArray<MemberDeclarationSyntax> introducedMembers,
        ImmutableArray<MemberDeclarationSyntax> peerDeclarations)
    {
        ArgumentNullException.ThrowIfNull(replacementDeclaration);
        return new MacroExpansionResult
        {
            ReplacementDeclaration = replacementDeclaration,
            IntroducedMembers = Normalize(introducedMembers),
            PeerDeclarations = Normalize(peerDeclarations)
        };
    }

    public static MacroExpansionResult FromIntroducedMembers(
        ImmutableArray<MemberDeclarationSyntax> introducedMembers)
        => new()
        {
            IntroducedMembers = Normalize(introducedMembers)
        };

    public static MacroExpansionResult FromPeerDeclarations(
        ImmutableArray<MemberDeclarationSyntax> peerDeclarations)
        => new()
        {
            PeerDeclarations = Normalize(peerDeclarations)
        };

    public static MacroExpansionResult FromDiagnostic(MacroExpansionDiagnostic diagnostic)
    {
        ArgumentNullException.ThrowIfNull(diagnostic);
        return new MacroExpansionResult
        {
            MacroDiagnostics = [diagnostic]
        };
    }

    public static MacroExpansionResult FromDiagnostics(
        ImmutableArray<Diagnostic> diagnostics)
        => new()
        {
            Diagnostics = Normalize(diagnostics)
        };

    public static MacroExpansionResult FromDiagnostics(
        ImmutableArray<MacroExpansionDiagnostic> macroDiagnostics)
        => new()
        {
            MacroDiagnostics = Normalize(macroDiagnostics)
        };

    public static MacroExpansionResult FromDiagnostics(
        ImmutableArray<Diagnostic> diagnostics,
        ImmutableArray<MacroExpansionDiagnostic> macroDiagnostics)
        => new()
        {
            Diagnostics = Normalize(diagnostics),
            MacroDiagnostics = Normalize(macroDiagnostics)
        };

    public SyntaxNode? ReplacementDeclaration { get; set; }

    public ImmutableArray<MemberDeclarationSyntax> IntroducedMembers { get; set; } = ImmutableArray<MemberDeclarationSyntax>.Empty;

    public ImmutableArray<MemberDeclarationSyntax> PeerDeclarations { get; set; } = ImmutableArray<MemberDeclarationSyntax>.Empty;

    public ImmutableArray<MacroExpansionDiagnostic> MacroDiagnostics { get; set; } = ImmutableArray<MacroExpansionDiagnostic>.Empty;

    public ImmutableArray<Diagnostic> Diagnostics { get; set; } = ImmutableArray<Diagnostic>.Empty;

    private static ImmutableArray<T> Normalize<T>(ImmutableArray<T> values)
        => values.IsDefault ? ImmutableArray<T>.Empty : values;
}
