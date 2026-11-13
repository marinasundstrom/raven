using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static partial class MacroParameterRoleFacts
{
    public static bool TryResolveKnownType(
        Compilation compilation,
        TypeSyntax typeSyntax,
        [NotNullWhen(true)] out TypeSymbol? type)
    {
        var sourceName = typeSyntax.ToString();
        var metadataName = sourceName switch
        {
            nameof(ExpressionSyntax) or "Raven.CodeAnalysis.Syntax.ExpressionSyntax" =>
                "Raven.CodeAnalysis.Syntax.ExpressionSyntax",
            nameof(FreestandingMacroDeclarationSyntax) or
                "Raven.CodeAnalysis.Syntax.FreestandingMacroDeclarationSyntax" =>
                "Raven.CodeAnalysis.Syntax.FreestandingMacroDeclarationSyntax",
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

        type = null;
        return false;
    }
}
