using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroEmbeddedLanguageProjectionService
{
    public static bool TryFindInvocationAtPosition(
        SemanticModel semanticModel,
        int position,
        CancellationToken cancellationToken,
        out SyntaxNode? syntax,
        out SyntaxNode? resolutionContext)
    {
        syntax = null;
        resolutionContext = null;
        cancellationToken.ThrowIfCancellationRequested();

        var root = semanticModel.SyntaxTree.GetRoot(cancellationToken);
        var searchPosition = Math.Clamp(position - 1, 0, root.FullSpan.End);
        var token = root.FindToken(searchPosition);
        var invocation = token.Parent?.AncestorsAndSelf()
            .FirstOrDefault(static node =>
                node is FreestandingMacroExpressionSyntax { TokenTree: not null } or
                    FreestandingMacroDeclarationSyntax { TokenTree: not null } or
                    FreestandingMacroMemberDeclarationSyntax { TokenTree: not null });
        if (invocation is null)
            return false;

        resolutionContext = invocation;
        for (var nestingDepth = 0; nestingDepth < 16; nestingDepth++)
        {
            var regions = ReferenceEquals(invocation.SyntaxTree, semanticModel.SyntaxTree)
                ? semanticModel.GetMacroInputSnapshotCore(invocation, cancellationToken).FragmentRegions
                : MacroFragmentRegionService.GetFragmentRegions(
                    semanticModel,
                    invocation,
                    resolutionContext,
                    cancellationToken);
            var region = regions
                .Where(candidate => candidate.Span.Length == 0
                    ? position == candidate.Span.Start
                    : position >= candidate.Span.Start && position <= candidate.Span.End)
                .OrderBy(static candidate => candidate.Span.Length)
                .FirstOrDefault();
            if (region is null)
            {
                if (CanProvideProjectionAtPosition(semanticModel, invocation, resolutionContext, position, cancellationToken))
                {
                    syntax = invocation;
                    return true;
                }

                return false;
            }

            var context = CreateContext(semanticModel, invocation, cancellationToken);
            var fragment = ParseFragment(context, region);
            var nestedSearchPosition = Math.Clamp(position - 1, fragment.FullSpan.Start, fragment.FullSpan.End);
            var nestedToken = fragment.FindToken(nestedSearchPosition);
            var nestedInvocation = nestedToken.Parent?.AncestorsAndSelf()
                .OfType<FreestandingMacroExpressionSyntax>()
                .FirstOrDefault(candidate => candidate.TokenTree?.Span.Contains(nestedSearchPosition) == true);
            if (nestedInvocation is null)
                return false;

            invocation = nestedInvocation;
        }

        return false;
    }

    public static MacroEmbeddedLanguageProjection? GetProjection(
        SemanticModel semanticModel,
        SyntaxNode syntax,
        CancellationToken cancellationToken)
        => GetProjection(semanticModel, syntax, syntax, cancellationToken);

    public static MacroEmbeddedLanguageProjection? GetProjection(
        SemanticModel semanticModel,
        SyntaxNode syntax,
        SyntaxNode resolutionContext,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (!FreestandingMacroInvocation.TryCreate(syntax, out var invocation) ||
            invocation.TokenTree is null ||
            !invocation.TryGetMacroName(out var name) ||
            !semanticModel.Compilation.GetMacroRegistry().TryResolveFreestandingMacro(
                semanticModel.Compilation,
                resolutionContext,
                name,
                out var loaded,
                out _) ||
            !loaded.Descriptor.HasTokenBody ||
            loaded.Macro is not IMacroEmbeddedLanguageProvider provider)
        {
            return null;
        }

        try
        {
            var context = new TokenTreeMacroContext(
                semanticModel.Compilation,
                semanticModel,
                invocation,
                loaded.Macro,
                cancellationToken);
            var projection = provider.GetEmbeddedLanguageProjection(context);
            cancellationToken.ThrowIfCancellationRequested();

            if (projection is null ||
                string.IsNullOrWhiteSpace(projection.LanguageId) ||
                projection.Span != context.BodySpan ||
                projection.Text.Length != context.BodySpan.Length)
            {
                return null;
            }

            return projection;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            throw;
        }
        catch
        {
            // Optional macro tooling must not break ordinary editor requests.
            return null;
        }
    }

    private static bool CanProvideProjectionAtPosition(
        SemanticModel semanticModel,
        SyntaxNode syntax,
        SyntaxNode resolutionContext,
        int position,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();
        if (!FreestandingMacroInvocation.TryCreate(syntax, out var invocation) ||
            invocation.TokenTree is null ||
            !invocation.TryGetMacroName(out var name) ||
            !semanticModel.Compilation.GetMacroRegistry().TryResolveFreestandingMacro(
                semanticModel.Compilation,
                resolutionContext,
                name,
                out var loaded,
                out _) ||
            !loaded.Descriptor.HasTokenBody ||
            loaded.Macro is not IMacroEmbeddedLanguageProvider)
        {
            return false;
        }

        var context = new TokenTreeMacroContext(
            semanticModel.Compilation,
            semanticModel,
            invocation,
            loaded.Macro,
            cancellationToken);
        return context.BodySpan.Contains(position) || context.BodySpan.End == position;
    }

    private static TokenTreeMacroContext CreateContext(
        SemanticModel semanticModel,
        SyntaxNode syntax,
        CancellationToken cancellationToken)
        => syntax switch
        {
            FreestandingMacroExpressionSyntax expression => new TokenTreeMacroContext(
                semanticModel.Compilation,
                semanticModel,
                expression,
                cancellationToken),
            FreestandingMacroDeclarationSyntax declaration => new TokenTreeMacroContext(
                semanticModel.Compilation,
                semanticModel,
                declaration,
                cancellationToken),
            FreestandingMacroMemberDeclarationSyntax member => new TokenTreeMacroContext(
                semanticModel.Compilation,
                semanticModel,
                member,
                cancellationToken),
            _ => throw new InvalidOperationException("Unsupported macro fragment carrier.")
        };

    private static SyntaxNode ParseFragment(
        TokenTreeMacroContext context,
        MacroFragmentRegion region)
        => region.Kind switch
        {
            MacroFragmentKind.Expression => context.ParseExpression(region.BodyRelativeSpan),
            MacroFragmentKind.Statement => context.ParseStatement(region.BodyRelativeSpan),
            MacroFragmentKind.Type => context.ParseType(region.BodyRelativeSpan),
            MacroFragmentKind.Pattern => context.ParsePattern(region.BodyRelativeSpan),
            MacroFragmentKind.MemberDeclaration => context.ParseMemberDeclaration(region.BodyRelativeSpan),
            MacroFragmentKind.Block => context.ParseBlock(region.BodyRelativeSpan),
            _ => throw new InvalidOperationException($"Unsupported macro fragment kind '{region.Kind}'.")
        };
}
