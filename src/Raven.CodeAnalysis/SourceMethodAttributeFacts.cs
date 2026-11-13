using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class SourceMethodAttributeFacts
{
    public static ImmutableArray<AttributeData> BindReturnTypeAttributes(
        Symbol owner, ImmutableArray<SyntaxReference> declarations, SourceDeclarationServices? services)
    {
        if (declarations.IsDefaultOrEmpty)
            return ImmutableArray<AttributeData>.Empty;

        if (services is null)
            return ImmutableArray<AttributeData>.Empty;

        var builder = ImmutableArray.CreateBuilder<AttributeData>();
        var seenAttributes = new Dictionary<AttributeTargets, HashSet<NamedTypeSymbol>>();
        const AttributeTargets defaultTarget = AttributeTargets.ReturnValue;

        foreach (var syntaxReference in declarations)
        {
            if (!services.TryGetCurrentDeclaringSyntax(syntaxReference, out var syntax))
                continue;

            var declarationReturnTargetLists = syntax switch
            {
                MethodDeclarationSyntax method => method.AttributeLists
                    .Where(static list => string.Equals(list.Target?.Identifier.ValueText, "return", StringComparison.OrdinalIgnoreCase)),
                FunctionStatementSyntax function => function.AttributeLists
                    .Where(static list => string.Equals(list.Target?.Identifier.ValueText, "return", StringComparison.OrdinalIgnoreCase)),
                _ => Enumerable.Empty<AttributeListSyntax>()
            };

            var returnClause = syntax switch
            {
                MethodDeclarationSyntax method => method.ReturnType,
                FunctionStatementSyntax function => function.ReturnType,
                ParenthesizedFunctionExpressionSyntax lambda => lambda.ReturnType,
                _ => null
            };

            IEnumerable<AttributeSyntax> returnAttributes = declarationReturnTargetLists.SelectMany(static list => list.Attributes);
            if (returnClause is not null && returnClause.AttributeLists.Count > 0)
                returnAttributes = returnAttributes.Concat(returnClause.AttributeLists.SelectMany(static list => list.Attributes));

            foreach (var attribute in returnAttributes)
            {
                if (attribute.IsMacroAttribute())
                    continue;

                if (services.BindAttribute(owner, attribute) is not { } boundAttribute)
                    continue;

                if (AttributeUsageHelper.TryValidateAttribute(
                        services,
                        boundAttribute.Diagnostics,
                        owner,
                        attribute,
                        boundAttribute.Data,
                        defaultTarget,
                        seenAttributes,
                        out var appliedTarget) &&
                    appliedTarget == AttributeTargets.ReturnValue)
                {
                    builder.Add(boundAttribute.Data);
                }
            }
        }

        return builder.ToImmutable();
    }
}
