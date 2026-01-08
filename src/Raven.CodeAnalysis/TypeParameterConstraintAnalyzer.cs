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

internal static class TypeParameterConstraintCollector
{
    public static (TypeParameterConstraintKind kind, ImmutableArray<SyntaxReference> typeRefs)
        CollectFor(
            TypeParameterSyntax parameterSyntax,
            SyntaxList<TypeParameterConstraintClauseSyntax> clauseList, // on declaration
            string typeParameterName)
    {
        // inline
        var (k1, r1) = TypeParameterConstraintAnalyzer.AnalyzeInline(parameterSyntax);

        // where clause(s) matching this name
        TypeParameterConstraintKind k2 = TypeParameterConstraintKind.None;
        var r2 = ImmutableArray.CreateBuilder<SyntaxReference>();

        foreach (var clause in clauseList)
        {
            // clause.Name is IdentifierNameSyntax in Roslyn model
            if (!IsMatchingClause(clause, typeParameterName))
                continue;

            var (ck, cr) = TypeParameterConstraintAnalyzer.AnalyzeClause(clause);
            k2 |= ck;
            r2.AddRange(cr);

            // Optional: if you want “at most one where per type parameter”, detect duplicates here
        }

        return (k1 | k2, r1.AddRange(r2.ToImmutable()));
    }

    private static bool IsMatchingClause(TypeParameterConstraintClauseSyntax clause, string name)
    {
        // implement based on your clause node shape
        throw new NotImplementedException();
    }
}
