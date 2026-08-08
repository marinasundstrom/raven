using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Collects expansion contributions produced while a macro function executes.
/// </summary>
/// <remarks>
/// Contribution order follows execution order. A later expression or
/// replacement supersedes an earlier one, while introduced members and editor
/// metadata append in order.
/// </remarks>
public sealed class MacroExpansionResultBuilder
{
    private readonly ImmutableArray<MemberDeclarationSyntax>.Builder _introducedMembers =
        ImmutableArray.CreateBuilder<MemberDeclarationSyntax>();
    private readonly ImmutableArray<MacroFragmentRegion>.Builder _fragmentRegions =
        ImmutableArray.CreateBuilder<MacroFragmentRegion>();
    private readonly ImmutableArray<MacroTokenInfo>.Builder _tokenInfos =
        ImmutableArray.CreateBuilder<MacroTokenInfo>();
    private ExpressionSyntax? _expression;
    private FreestandingMacroExpansionResult? _freestandingResult;
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
        _freestandingResult = result;
        _expression = result.Expression;
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
        var result = _freestandingResult is not null
            ? _freestandingResult
            : _expression is null
            ? FreestandingMacroExpansionResult.Empty
            : FreestandingMacroExpansionResult.FromExpression(_expression);

        if (_fragmentRegions.Count > 0 || _tokenInfos.Count > 0)
        {
            if (ReferenceEquals(result, FreestandingMacroExpansionResult.Empty))
                result = new FreestandingMacroExpansionResult();

            if (_fragmentRegions.Count > 0)
            {
                result.FragmentRegions = result.FragmentRegions.IsDefault
                    ? _fragmentRegions.ToImmutable()
                    : result.FragmentRegions.AddRange(_fragmentRegions);
            }
            if (_tokenInfos.Count > 0)
            {
                result.TokenInfos = result.TokenInfos.IsDefault
                    ? _tokenInfos.ToImmutable()
                    : result.TokenInfos.AddRange(_tokenInfos);
            }
        }

        return result;
    }

    public MacroExpansionResult BuildAttached()
    {
        var introducedMembers = _introducedMembers.ToImmutable();
        if (_replacement is not null)
            return MacroExpansionResult.FromReplacement(_replacement, introducedMembers);

        return introducedMembers.IsEmpty
            ? MacroExpansionResult.Empty
            : MacroExpansionResult.FromIntroducedMembers(introducedMembers);
    }
}
