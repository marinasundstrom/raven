using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public sealed class MacroExpansionResult
{
    public static MacroExpansionResult Empty { get; } = new();

    public SyntaxNode? ReplacementDeclaration { get; set; }

    public ImmutableArray<MemberDeclarationSyntax> IntroducedMembers { get; set; } = ImmutableArray<MemberDeclarationSyntax>.Empty;

    public ImmutableArray<MemberDeclarationSyntax> PeerDeclarations { get; set; } = ImmutableArray<MemberDeclarationSyntax>.Empty;

    public ImmutableArray<Diagnostic> Diagnostics { get; set; } = ImmutableArray<Diagnostic>.Empty;
}
