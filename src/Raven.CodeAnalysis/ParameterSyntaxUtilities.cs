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
}
