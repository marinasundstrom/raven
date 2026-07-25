using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class ParameterSyntaxUtilities
{
    public static RefKind GetRefKind(ParameterSyntax parameter)
    {
        var typeSyntax = parameter.TypeAnnotation?.Type;
        var refKindTokenKind = parameter.RefKindKeyword.Kind;

        return typeSyntax is ByRefTypeSyntax
            ? refKindTokenKind switch
            {
                SyntaxKind.OutKeyword => RefKind.Out,
                SyntaxKind.InKeyword => RefKind.In,
                SyntaxKind.RefKeyword => RefKind.Ref,
                _ => RefKind.Ref,
            }
            : refKindTokenKind switch
            {
                SyntaxKind.OutKeyword => RefKind.Out,
                SyntaxKind.InKeyword => RefKind.In,
                SyntaxKind.RefKeyword => RefKind.Ref,
                _ => RefKind.None,
            };
    }

    public static ScopedKind GetScopedKind(ParameterSyntax parameter)
    {
        if (!parameter.ScopedKeyword.IsKind(SyntaxKind.ScopedKeyword))
            return ScopedKind.None;

        return GetRefKind(parameter).IsByRef
            ? ScopedKind.ScopedRef
            : ScopedKind.ScopedValue;
    }

    public static ScopedKind GetScopedKind(
        ParameterSyntax parameter,
        ITypeSymbol parameterType,
        DiagnosticBag diagnostics)
    {
        var scopedKind = GetScopedKind(parameter);
        if (scopedKind == ScopedKind.ScopedValue &&
            parameterType.TypeKind != TypeKind.Error &&
            !SemanticFacts.MayBeRefLike(parameterType))
        {
            diagnostics.ReportScopedModifierRequiresRefLikeTypeOrReference(
                parameter.ScopedKeyword.GetLocation());
        }

        return scopedKind;
    }
}
