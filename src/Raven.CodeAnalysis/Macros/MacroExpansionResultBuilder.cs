using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Collects expansion contributions produced while a macro function executes.
/// </summary>
/// <remarks>
/// Contribution order follows execution order. A later expression or
/// replacement supersedes an earlier one, while introduced members append in
/// order.
/// </remarks>
public sealed class MacroExpansionResultBuilder
{
    private readonly ImmutableArray<MemberDeclarationSyntax>.Builder _introducedMembers =
        ImmutableArray.CreateBuilder<MemberDeclarationSyntax>();
    private readonly ImmutableArray<MacroFragmentRegion>.Builder _fragmentRegions =
        ImmutableArray.CreateBuilder<MacroFragmentRegion>();
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

    public FreestandingMacroExpansionResult BuildFreestanding()
    {
        var result = _freestandingResult is not null
            ? _freestandingResult
            : _expression is null
            ? FreestandingMacroExpansionResult.Empty
            : FreestandingMacroExpansionResult.FromExpression(_expression);

        if (_fragmentRegions.Count > 0)
        {
            if (ReferenceEquals(result, FreestandingMacroExpansionResult.Empty))
                result = new FreestandingMacroExpansionResult();

            result.FragmentRegions = result.FragmentRegions.IsDefault
                ? _fragmentRegions.ToImmutable()
                : result.FragmentRegions.AddRange(_fragmentRegions);
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
