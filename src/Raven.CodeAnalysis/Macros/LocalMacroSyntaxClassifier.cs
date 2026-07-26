using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static class LocalMacroSyntaxClassifier
{
    private const string MarkerName = "LocalMacroPlugin";
    private const string MarkerAttributeName = "LocalMacroPluginAttribute";

    public static bool IsLocalMacroTree(SyntaxTree syntaxTree)
    {
        ArgumentNullException.ThrowIfNull(syntaxTree);

        return syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<TypeDeclarationSyntax>()
            .SelectMany(static declaration => declaration.AttributeLists)
            .SelectMany(static list => list.Attributes)
            .Any(IsMarkerAttribute);
    }

    private static bool IsMarkerAttribute(AttributeSyntax attribute)
    {
        if (attribute.HashToken.Kind != SyntaxKind.None)
            return false;

        var identifier = attribute.Name
            .DescendantTokens()
            .LastOrDefault(static token => token.Kind == SyntaxKind.IdentifierToken);
        return string.Equals(identifier.ValueText, MarkerName, StringComparison.Ordinal) ||
               string.Equals(identifier.ValueText, MarkerAttributeName, StringComparison.Ordinal);
    }
}
