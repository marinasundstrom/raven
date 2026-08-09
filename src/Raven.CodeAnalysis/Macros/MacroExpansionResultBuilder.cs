using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Collects expansion contributions produced while a macro function executes.
/// </summary>
/// <remarks>
/// Contribution order follows execution order. Replacements supersede earlier
/// replacements, while introduced members and editor metadata append in order.
/// In source macro functions, <c>expand</c> finalizes the accumulated result and
/// returns it from the current execution path.
/// </remarks>
public sealed class MacroExpansionResultBuilder
{
    private readonly ImmutableArray<Diagnostic>.Builder _diagnostics =
        ImmutableArray.CreateBuilder<Diagnostic>();
    private readonly ImmutableArray<MacroExpansionDiagnostic>.Builder _macroDiagnostics =
        ImmutableArray.CreateBuilder<MacroExpansionDiagnostic>();
    private readonly ImmutableArray<MemberDeclarationSyntax>.Builder _introducedMembers =
        ImmutableArray.CreateBuilder<MemberDeclarationSyntax>();
    private readonly ImmutableArray<MemberDeclarationSyntax>.Builder _peerDeclarations =
        ImmutableArray.CreateBuilder<MemberDeclarationSyntax>();
    private readonly ImmutableArray<MacroFragmentRegion>.Builder _fragmentRegions =
        ImmutableArray.CreateBuilder<MacroFragmentRegion>();
    private readonly ImmutableArray<MacroTokenInfo>.Builder _tokenInfos =
        ImmutableArray.CreateBuilder<MacroTokenInfo>();
    private ExpressionSyntax? _expression;
    private SyntaxNode? _replacement;

    public void Expand(ExpressionSyntax expression)
    {
        ArgumentNullException.ThrowIfNull(expression);
        _expression = expression;
    }

    /// <summary>
    /// Applies a complete expansion result produced by a lower-level macro API.
    /// </summary>
    public void Expand(FreestandingMacroExpansionResult result)
    {
        ArgumentNullException.ThrowIfNull(result);
        if (result.Expression is not null)
            _expression = result.Expression;

        AddRange(_diagnostics, result.Diagnostics);
        AddRange(_macroDiagnostics, result.MacroDiagnostics);
        AddRange(_fragmentRegions, result.FragmentRegions);
        AddRange(_tokenInfos, result.TokenInfos);
    }

    /// <summary>
    /// Applies a complete attached expansion result produced by a lower-level
    /// macro API.
    /// </summary>
    public void Expand(MacroExpansionResult result)
    {
        ArgumentNullException.ThrowIfNull(result);
        if (result.ReplacementDeclaration is not null)
            _replacement = result.ReplacementDeclaration;

        AddRange(_introducedMembers, result.IntroducedMembers);
        AddRange(_peerDeclarations, result.PeerDeclarations);
        AddRange(_diagnostics, result.Diagnostics);
        AddRange(_macroDiagnostics, result.MacroDiagnostics);
    }

    public void Replace(SyntaxNode declaration)
    {
        ArgumentNullException.ThrowIfNull(declaration);
        _replacement = declaration;
    }

    public void Introduce(MemberDeclarationSyntax member)
    {
        ArgumentNullException.ThrowIfNull(member);
        _introducedMembers.Add(member);
    }

    public void Introduce(IEnumerable<MemberDeclarationSyntax> members)
    {
        ArgumentNullException.ThrowIfNull(members);
        _introducedMembers.AddRange(members);
    }

    /// <summary>
    /// Contributes an ordinary Raven fragment for editor tooling.
    /// </summary>
    public void Fragment(MacroFragmentRegion region)
    {
        ArgumentNullException.ThrowIfNull(region);
        _fragmentRegions.Add(region);
    }

    /// <summary>
    /// Contributes ordinary Raven fragments for editor tooling.
    /// </summary>
    public void Fragment(IEnumerable<MacroFragmentRegion> regions)
    {
        ArgumentNullException.ThrowIfNull(regions);
        _fragmentRegions.AddRange(regions);
    }

    /// <summary>
    /// Contributes metadata for a token in the macro body.
    /// </summary>
    public void Token(MacroTokenInfo tokenInfo)
    {
        ArgumentNullException.ThrowIfNull(tokenInfo);
        _tokenInfos.Add(tokenInfo);
    }

    /// <summary>
    /// Contributes metadata for tokens in the macro body.
    /// </summary>
    public void Token(IEnumerable<MacroTokenInfo> tokenInfos)
    {
        ArgumentNullException.ThrowIfNull(tokenInfos);
        _tokenInfos.AddRange(tokenInfos);
    }

    public FreestandingMacroExpansionResult BuildFreestanding()
    {
        if (_expression is null &&
            _diagnostics.Count == 0 &&
            _macroDiagnostics.Count == 0 &&
            _fragmentRegions.Count == 0 &&
            _tokenInfos.Count == 0)
        {
            return FreestandingMacroExpansionResult.Empty;
        }

        return new FreestandingMacroExpansionResult
        {
            Expression = _expression,
            Diagnostics = _diagnostics.ToImmutable(),
            MacroDiagnostics = _macroDiagnostics.ToImmutable(),
            FragmentRegions = _fragmentRegions.ToImmutable(),
            TokenInfos = _tokenInfos.ToImmutable()
        };
    }

    public MacroExpansionResult BuildAttached()
    {
        var introducedMembers = _introducedMembers.ToImmutable();
        var peerDeclarations = _peerDeclarations.ToImmutable();
        var diagnostics = _diagnostics.ToImmutable();
        var macroDiagnostics = _macroDiagnostics.ToImmutable();
        if (_replacement is null &&
            introducedMembers.IsEmpty &&
            peerDeclarations.IsEmpty &&
            diagnostics.IsEmpty &&
            macroDiagnostics.IsEmpty)
        {
            return MacroExpansionResult.Empty;
        }

        return new MacroExpansionResult
        {
            ReplacementDeclaration = _replacement,
            IntroducedMembers = introducedMembers,
            PeerDeclarations = peerDeclarations,
            Diagnostics = diagnostics,
            MacroDiagnostics = macroDiagnostics
        };
    }

    private static void AddRange<T>(
        ImmutableArray<T>.Builder builder,
        ImmutableArray<T> values)
    {
        if (!values.IsDefaultOrEmpty)
            builder.AddRange(values);
    }

}
