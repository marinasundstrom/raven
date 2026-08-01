using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal static class TypeParameterConstraintAnalyzer
{
    public static (TypeParameterConstraintKind kind, ImmutableArray<SyntaxReference> typeRefs)
        AnalyzeInline(TypeParameterSyntax parameter)
    {
        // Decide one consistent policy:
        // If constraints.Count == 0 => None (no need to check ColonToken at all)
        var constraints = parameter.Constraints;
        if (constraints.Count == 0)
            return (TypeParameterConstraintKind.None, ImmutableArray<SyntaxReference>.Empty);

        return AnalyzeConstraintList(constraints);
    }

    public static (TypeParameterConstraintKind kind, ImmutableArray<SyntaxReference> typeRefs)
        AnalyzeClause(TypeParameterConstraintClauseSyntax clause)
    {
        var constraints = clause.Constraints;
        if (constraints.Count == 0)
            return (TypeParameterConstraintKind.None, ImmutableArray<SyntaxReference>.Empty);

        return AnalyzeConstraintList(constraints);
    }

    private static (TypeParameterConstraintKind kind, ImmutableArray<SyntaxReference> typeRefs)
        AnalyzeConstraintList(SeparatedSyntaxList<TypeParameterConstraintSyntax> constraints)
    {
        var kind = TypeParameterConstraintKind.None;
        var typeRefs = ImmutableArray.CreateBuilder<SyntaxReference>();

        foreach (var constraint in constraints)
        {
            switch (constraint)
            {
                case ClassConstraintSyntax:
                    kind |= TypeParameterConstraintKind.ReferenceType;
                    break;

                case StructConstraintSyntax:
                    kind |= TypeParameterConstraintKind.ValueType;
                    break;

                case TypeConstraintSyntax typeConstraint:
                    if (IsNotNullConstraint(typeConstraint))
                    {
                        kind |= TypeParameterConstraintKind.NotNull;
                        break;
                    }

                    kind |= TypeParameterConstraintKind.TypeConstraint;
                    typeRefs.Add(typeConstraint.GetReference());
                    break;

                case ConstructorConstraintSyntax:
                    kind |= TypeParameterConstraintKind.Constructor;
                    break;

                case AllowsRefStructConstraintSyntax:
                    kind |= TypeParameterConstraintKind.AllowByRefLike;
                    break;
            }
        }

        return (kind, typeRefs.ToImmutable());
    }

    private static bool IsNotNullConstraint(TypeConstraintSyntax typeConstraint)
    {
        return typeConstraint.Type is IdentifierNameSyntax identifier &&
               string.Equals(identifier.Identifier.Text, "notnull", StringComparison.Ordinal);
    }
}
