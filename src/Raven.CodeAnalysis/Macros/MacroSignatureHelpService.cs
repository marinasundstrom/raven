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
        TypeSyntax nameSyntax;
        SyntaxNode invocation;
        MacroKind expectedKind;
        switch (argumentList.Parent)
        {
            case AttributeSyntax attribute when attribute.TryGetMacroName(out name):
                nameSyntax = attribute.Name;
                invocation = attribute;
                expectedKind = MacroKind.AttachedDeclaration;
                break;

            case FreestandingMacroExpressionSyntax expression when expression.TryGetMacroName(out name):
                nameSyntax = expression.Name;
                invocation = expression;
                expectedKind = MacroKind.Freestanding;
                break;

            default:
                return null;
        }

        if (semanticModel.Compilation.TryResolveLocalMacroDeclarationSymbol(
                invocation,
                name,
                nameSyntax.GetMacroArity(),
                out var localDefinition,
                out _) &&
            localDefinition.MacroKind == expectedKind)
        {
            var localMacro = semanticModel.ConstructLocalMacroSymbol(nameSyntax, localDefinition);
            var localParameters = localMacro.ParameterBindings
                .Where(static binding => binding.InvocationArgumentOrdinal is not null)
                .OrderBy(static binding => binding.InvocationArgumentOrdinal)
                .Select(static binding => CreateParameter(binding))
                .ToImmutableArray();
            var localActiveParameter = GetActiveParameter(argumentList, localParameters, position);
            var hasLocalTokenTreeBody = localMacro.ParameterBindings.Any(
                static binding => binding.Source == MacroParameterSource.TokenBody);
            return new MacroSignatureHelp(
                nameSyntax.ToString(),
                localMacro.MacroKind,
                localParameters,
                localActiveParameter,
                hasLocalTokenTreeBody);
        }

        if (!TryResolveLoadedMacro(
                semanticModel,
                invocation,
                name,
                expectedKind,
                out var descriptor))
        {
            return null;
        }

        var parameters = descriptor.Parameters.Select(parameter => CreateParameter(
                parameter,
                descriptor.Definition as IMacroExecutor,
                nameSyntax))
            .ToImmutableArray();
        var activeParameter = GetActiveParameter(argumentList, parameters, position);
        return new MacroSignatureHelp(
            nameSyntax.ToString(),
            expectedKind,
            parameters,
            activeParameter,
            descriptor.HasTokenBody);
    }

    private static bool TryResolveLoadedMacro(
        SemanticModel semanticModel,
        SyntaxNode invocation,
        string name,
        MacroKind expectedKind,
        out MacroDefinitionDescriptor descriptor)
    {
        var registry = semanticModel.Compilation.GetMacroRegistry();
        if (expectedKind == MacroKind.AttachedDeclaration &&
            invocation is AttributeSyntax attribute &&
            registry.TryResolveAttachedMacro(
                semanticModel.Compilation,
                attribute,
                name,
                out var attached,
                out _))
        {
            descriptor = attached.Descriptor;
            return true;
        }

        if (expectedKind == MacroKind.Freestanding &&
            invocation is FreestandingMacroExpressionSyntax expression &&
            registry.TryResolveFreestandingMacro(
                semanticModel.Compilation,
                expression,
                name,
                out var freestanding,
                out _))
        {
            descriptor = freestanding.Descriptor;
            return true;
        }

        descriptor = null!;
        return false;
    }

    private static MacroSignatureParameter CreateParameter(MacroParameterBinding binding)
    {
        var parameter = binding.Parameter;
        return new MacroSignatureParameter(
            parameter.Name,
            parameter.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            parameter.MacroRole,
            binding.Source,
            binding.InvocationArgumentOrdinal!.Value,
            !parameter.IsOptional,
            parameter.ExplicitDefaultValue);
    }

    private static MacroSignatureParameter CreateParameter(
        MacroParameterDescriptor parameter,
        IMacroExecutor? executor,
        TypeSyntax nameSyntax)
        => new(
            parameter.Name,
            GetConstructedTypeDisplay(parameter.TypeDisplayName, executor, nameSyntax),
            parameter.Role,
            GetSource(parameter.Role),
            parameter.Ordinal,
            parameter.IsRequired,
            parameter.DefaultValue,
            parameter.DefaultValueDisplay);

    private static string GetConstructedTypeDisplay(
        string typeDisplayName,
        IMacroExecutor? executor,
        TypeSyntax nameSyntax)
    {
        if (executor is null ||
            executor.TypeParameters.IsDefaultOrEmpty ||
            !nameSyntax.TryGetMacroTypeArgumentList(out var typeArgumentList) ||
            typeArgumentList.Arguments.Count != executor.TypeParameters.Length)
        {
            return typeDisplayName;
        }

        for (var index = 0; index < executor.TypeParameters.Length; index++)
        {
            var parameterName = executor.TypeParameters[index];
            var argumentDisplay = typeArgumentList.Arguments[index].Type.ToString();
            if (string.Equals(typeDisplayName, parameterName, StringComparison.Ordinal))
                return argumentDisplay;

            typeDisplayName = typeDisplayName.Replace(
                $"<{parameterName}>",
                $"<{argumentDisplay}>",
                StringComparison.Ordinal);
        }

        return typeDisplayName;
    }

    private static MacroParameterSource GetSource(MacroParameterRole role)
        => role switch
        {
            MacroParameterRole.SyntaxInput => MacroParameterSource.SyntaxInput,
            MacroParameterRole.Context => MacroParameterSource.Context,
            MacroParameterRole.TokenBody => MacroParameterSource.TokenBody,
            MacroParameterRole.AttachedTarget => MacroParameterSource.AttachedTarget,
            _ => MacroParameterSource.Value,
        };

    private static int GetActiveParameter(
        ArgumentListSyntax argumentList,
        ImmutableArray<MacroSignatureParameter> parameters,
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
