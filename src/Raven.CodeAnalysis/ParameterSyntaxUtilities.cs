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
}
