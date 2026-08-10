using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroParameterRoleFacts
{
    public static MacroParameterRole GetRole(ITypeSymbol parameterType)
    {
        if (IsOrDerivesFrom(
            parameterType,
            "Raven.CodeAnalysis.Macros",
            nameof(AttachedMacroContext)))
        {
            return MacroParameterRole.AttachedContext;
        }

        if (IsOrDerivesFrom(
            parameterType,
            "Raven.CodeAnalysis.Macros",
            nameof(FreestandingMacroContext)))
        {
            return MacroParameterRole.FreestandingContext;
        }

        if (IsOrDerivesFrom(
            parameterType,
            "Raven.CodeAnalysis.Macros",
            nameof(TokenTreeMacroContext)))
        {
            return MacroParameterRole.Context;
        }

        if (IsOrDerivesFrom(
            parameterType,
            "Raven.CodeAnalysis.Syntax",
            nameof(ExpressionSyntax)))
        {
            return MacroParameterRole.SyntaxInput;
        }

        if (IsNamedType(
                parameterType,
                "Raven.CodeAnalysis.Macros",
                nameof(IMacroTokenStream)) ||
            parameterType is INamedTypeSymbol namedType &&
            namedType.AllInterfaces.Any(static @interface =>
                IsNamedType(
                    @interface,
                    "Raven.CodeAnalysis.Macros",
                    nameof(IMacroTokenStream))))
        {
            return MacroParameterRole.TokenStream;
        }

        return MacroParameterRole.Value;
    }

    public static MacroParameterRole GetRole(Type parameterType)
    {
        if (typeof(AttachedMacroContext).IsAssignableFrom(parameterType))
            return MacroParameterRole.AttachedContext;

        if (typeof(FreestandingMacroContext).IsAssignableFrom(parameterType))
            return MacroParameterRole.FreestandingContext;

        if (typeof(TokenTreeMacroContext).IsAssignableFrom(parameterType))
            return MacroParameterRole.Context;

        if (typeof(ExpressionSyntax).IsAssignableFrom(parameterType))
            return MacroParameterRole.SyntaxInput;

        if (typeof(IMacroTokenStream).IsAssignableFrom(parameterType))
            return MacroParameterRole.TokenStream;

        return MacroParameterRole.Value;
    }

    public static bool TryResolveKnownType(
        Compilation compilation,
        TypeSyntax typeSyntax,
        out ITypeSymbol type)
    {
        var sourceName = typeSyntax.ToString();
        var metadataName = sourceName switch
        {
            nameof(ExpressionSyntax) or "Raven.CodeAnalysis.Syntax.ExpressionSyntax" =>
                "Raven.CodeAnalysis.Syntax.ExpressionSyntax",
            nameof(IMacroTokenStream) or "Raven.CodeAnalysis.Macros.IMacroTokenStream" =>
                "Raven.CodeAnalysis.Macros.IMacroTokenStream",
            nameof(TokenTreeMacroContext) or "Raven.CodeAnalysis.Macros.TokenTreeMacroContext" =>
                "Raven.CodeAnalysis.Macros.TokenTreeMacroContext",
            nameof(FreestandingMacroContext) or "Raven.CodeAnalysis.Macros.FreestandingMacroContext" =>
                "Raven.CodeAnalysis.Macros.FreestandingMacroContext",
            nameof(AttachedMacroContext) or "Raven.CodeAnalysis.Macros.AttachedMacroContext" =>
                "Raven.CodeAnalysis.Macros.AttachedMacroContext",
            _ => null
        };

        if (metadataName is not null &&
            compilation.GetTypeByMetadataName(metadataName) is { } resolved)
        {
            type = resolved;
            return true;
        }

        type = null!;
        return false;
    }

    public static string GetLoweredTypeName(
        ParameterSyntax parameter,
        MacroParameterRole role)
        => role switch
        {
            MacroParameterRole.SyntaxInput =>
                parameter.TypeAnnotation?.Type.ToString() ??
                "Raven.CodeAnalysis.Syntax.ExpressionSyntax",
            MacroParameterRole.TokenStream => "Raven.CodeAnalysis.Macros.IMacroTokenStream",
            MacroParameterRole.Context => "Raven.CodeAnalysis.Macros.TokenTreeMacroContext",
            MacroParameterRole.FreestandingContext => "Raven.CodeAnalysis.Macros.FreestandingMacroContext",
            MacroParameterRole.AttachedContext => "Raven.CodeAnalysis.Macros.AttachedMacroContext",
            _ => parameter.TypeAnnotation?.Type.ToString() ?? "object"
        };

    private static bool IsOrDerivesFrom(
        ITypeSymbol type,
        string namespaceName,
        string typeName)
    {
        for (var current = type as INamedTypeSymbol;
             current is not null;
             current = current.BaseType)
        {
            if (IsNamedType(current, namespaceName, typeName))
                return true;
        }

        return false;
    }

    private static bool IsNamedType(
        ITypeSymbol type,
        string namespaceName,
        string typeName)
        => type.Name == typeName &&
           IsNamespace(type.ContainingNamespace, namespaceName);

    private static bool IsNamespace(
        INamespaceSymbol? namespaceSymbol,
        string qualifiedNamespace)
    {
        if (namespaceSymbol is null)
            return false;

        var remaining = qualifiedNamespace;

        while (!namespaceSymbol.IsGlobalNamespace)
        {
            var lastDot = remaining.LastIndexOf('.');
            var segment = lastDot >= 0 ? remaining[(lastDot + 1)..] : remaining;

            if (!string.Equals(namespaceSymbol.Name, segment, StringComparison.Ordinal))
                return false;

            if (lastDot < 0)
                return namespaceSymbol.ContainingNamespace.IsGlobalNamespace;

            remaining = remaining[..lastDot];
            namespaceSymbol = namespaceSymbol.ContainingNamespace;
        }

        return false;
    }
}
