using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroSignatureHelpService
{
    public static MacroSignatureHelp? GetSignatureHelp(SemanticModel semanticModel, int position)
    {
        var root = semanticModel.SyntaxTree.GetRoot();
        var argumentList = FindArgumentList(root, position);
        if (argumentList is null)
            return null;

        string name;
        MacroDefinitionDescriptor descriptor;
        MacroKind kind;
        bool hasTokenTreeBody;
        switch (argumentList.Parent)
        {
            case AttributeSyntax attribute
                when attribute.TryGetMacroName(out name) &&
                     semanticModel.Compilation.GetMacroRegistry()
                         .TryResolveAttachedMacro(
                             semanticModel.Compilation,
                             attribute,
                             name,
                             out var attached,
                             out _):
                descriptor = attached.Descriptor;
                kind = MacroKind.AttachedDeclaration;
                hasTokenTreeBody = false;
                break;

            case FreestandingMacroExpressionSyntax expression
                when expression.TryGetMacroName(out name) &&
                     semanticModel.Compilation.GetMacroRegistry()
                         .TryResolveFreestandingMacro(
                             semanticModel.Compilation,
                             expression,
                             name,
                             out var freestanding,
                             out _):
                descriptor = freestanding.Descriptor;
                kind = MacroKind.FreestandingExpression;
                hasTokenTreeBody = descriptor.HasTokenBody;
                break;

            default:
                return null;
        }

        var parameters = descriptor.Parameters;
        var activeParameter = GetActiveParameter(argumentList, parameters, position);
        return new MacroSignatureHelp(name, kind, parameters, activeParameter, hasTokenTreeBody);
    }

    private static int GetActiveParameter(
        ArgumentListSyntax argumentList,
        ImmutableArray<MacroParameterDescriptor> parameters,
        int position)
    {
        if (parameters.IsDefaultOrEmpty)
            return 0;

        var argument = argumentList.Arguments
            .FirstOrDefault(candidate =>
                position >= candidate.FullSpan.Start &&
                position <= candidate.FullSpan.End);
        var namedArgument = argument?.NameColon?.Name.Identifier.ValueText;
        if (!string.IsNullOrEmpty(namedArgument))
        {
            for (var index = 0; index < parameters.Length; index++)
            {
                if (string.Equals(parameters[index].Name, namedArgument, StringComparison.Ordinal))
                    return index;
            }
        }

        var commaCount = argumentList.Arguments
            .GetSeparators()
            .Count(separator => separator.Span.Start < position);
        return Math.Clamp(commaCount, 0, parameters.Length - 1);
    }

    private static ArgumentListSyntax? FindArgumentList(SyntaxNode root, int position)
    {
        foreach (var candidatePosition in GetCandidatePositions(position, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(candidatePosition);
            }
            catch (ArgumentOutOfRangeException)
            {
                continue;
            }

            var argumentList = token.Parent?.AncestorsAndSelf()
                .OfType<ArgumentListSyntax>()
                .FirstOrDefault(candidate =>
                    candidatePosition >= candidate.OpenParenToken.Span.Start &&
                    candidatePosition <= candidate.CloseParenToken.Span.End);
            if (argumentList is not null)
                return argumentList;
        }

        return null;
    }

    private static IEnumerable<int> GetCandidatePositions(int position, int end)
    {
        var clamped = Math.Clamp(position, 0, end);
        yield return clamped;
        if (clamped > 0)
            yield return clamped - 1;
    }
}
