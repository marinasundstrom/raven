using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static partial class SymbolResolver
{
    private static bool TryResolveExactIdentifierSymbol(
        SemanticModel semanticModel,
        SyntaxNode root,
        int offset,
        [NotNullWhen(true)] out SymbolResolutionResult? resolution)
    {
        resolution = null;

        foreach (var normalizedOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            if (!TryGetIdentifierTokenAtOffset(root, normalizedOffset, out var token, out var identifier))
                continue;

            if (!TryResolveIdentifierByIntent(semanticModel, identifier, token, out var symbol) ||
                symbol is null)
            {
                continue;
            }

            var kind = TryResolveIdentifierInvocationOrMemberTarget(semanticModel, identifier, token, out _)
                ? SymbolResolutionKind.InvocationTarget
                : TryResolveExplicitTypeIdentifierSymbol(semanticModel, identifier, token, out _)
                    ? SymbolResolutionKind.TypePosition
                    : identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
                      HaveEquivalentSpan(memberAccess.Expression, identifier)
                        ? SymbolResolutionKind.MemberReceiver
                    : SymbolResolutionKind.Identifier;

            resolution = new SymbolResolutionResult(kind, symbol.UnderlyingSymbol, identifier);
            return true;
        }

        return false;
    }

    // Exact identifier hovers split by semantic intent:
    // - explicit type identifiers
    // - invocation/member targets
    // - ordinary referenced identifiers
    private static bool TryResolveIdentifierByIntent(
        SemanticModel semanticModel,
        IdentifierNameSyntax identifier,
        SyntaxToken token,
        [NotNullWhen(true)] out ISymbol? symbol)
    {
        symbol = null;

        if (TryResolveIdentifierInvocationOrMemberTarget(semanticModel, identifier, token, out var invocationOrMemberSymbol) &&
            invocationOrMemberSymbol is not null)
        {
            symbol = invocationOrMemberSymbol;
            return true;
        }

        if (TryResolveExplicitTypeIdentifierSymbol(semanticModel, identifier, token, out var explicitTypeSymbol) &&
            explicitTypeSymbol is not null)
        {
            symbol = explicitTypeSymbol;
            return true;
        }

        if (TryResolveReferencedIdentifierSymbol(semanticModel, identifier, token, out var referencedSymbol) &&
            referencedSymbol is not null)
        {
            symbol = referencedSymbol;
            return true;
        }

        return false;
    }

    private static bool TryResolveIdentifierInvocationOrMemberTarget(
        SemanticModel semanticModel,
        IdentifierNameSyntax identifier,
        SyntaxToken token,
        [NotNullWhen(true)] out ISymbol? symbol)
    {
        symbol = null;

        if (TryResolveInvocationTargetSymbol(semanticModel, identifier, token, out var invocationSymbol) &&
            invocationSymbol is not null)
        {
            symbol = invocationSymbol;
            return true;
        }

        if (identifier.Parent is not MemberAccessExpressionSyntax memberAccess ||
            !HaveEquivalentSpan(memberAccess.Name, identifier))
        {
            return false;
        }

        symbol = ResolveExplicitMemberAccessSymbol(semanticModel, memberAccess, token.Span);
        return symbol is not null;
    }

    private static bool TryResolveReferencedIdentifierSymbol(
        SemanticModel semanticModel,
        IdentifierNameSyntax identifier,
        SyntaxToken token,
        [NotNullWhen(true)] out ISymbol? symbol)
    {
        symbol = TryGetSymbolInfo(semanticModel, identifier, out var symbolInfo)
            ? ProjectSymbolForDisplay(ChoosePreferredSymbol(symbolInfo.Symbol, symbolInfo.CandidateSymbols, identifier))
            : null;
        if (symbol is not null)
            return true;

        var operationSymbol = FindReferencedSymbolAtToken(TryGetOperation(semanticModel, identifier), token.Span);
        symbol = ProjectSymbolForDisplay(operationSymbol);
        return symbol is not null;
    }

    private static bool TryResolveExplicitTypeIdentifierSymbol(
        SemanticModel semanticModel,
        IdentifierNameSyntax identifier,
        SyntaxToken token,
        [NotNullWhen(true)] out ISymbol? symbol)
    {
        symbol = null;

        if (TryGetSymbolInfo(semanticModel, identifier, out var identifierSymbolInfo))
        {
            var resolvedIdentifierSymbol = ChoosePreferredSymbol(
                identifierSymbolInfo.Symbol,
                identifierSymbolInfo.CandidateSymbols,
                identifier);
            resolvedIdentifierSymbol = ProjectTypeContextSymbol(resolvedIdentifierSymbol!);
            if (resolvedIdentifierSymbol is ITypeSymbol or IAliasSymbol)
            {
                symbol = resolvedIdentifierSymbol;
                return true;
            }
        }

        var typeSyntax = identifier.AncestorsAndSelf()
            .OfType<TypeSyntax>()
            .FirstOrDefault(candidate =>
                candidate.Span.Contains(token.Span) &&
                IsExplicitTypeSyntaxContext(candidate));
        if (typeSyntax is null)
            return false;

        if (!TryResolveTypeSymbolFromSyntax(semanticModel, typeSyntax, out var resolvedType) ||
            resolvedType is null)
        {
            return false;
        }

        symbol = resolvedType.UnderlyingUnionType ?? resolvedType;
        return true;
    }

    private static bool TryGetIdentifierTokenAtOffset(
        SyntaxNode root,
        int offset,
        [NotNullWhen(true)] out SyntaxToken token,
        [NotNullWhen(true)] out IdentifierNameSyntax? identifier)
    {
        try
        {
            token = root.FindToken(offset);
        }
        catch
        {
            token = default;
            identifier = null;
            return false;
        }

        identifier = token.Parent as IdentifierNameSyntax;
        return identifier is not null && identifier.Identifier.Span.Contains(token.Span);
    }
}
