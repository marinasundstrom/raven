using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

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
                    AddTypeReferences(typeConstraint.Type, typeRefs);
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

    private static void AddTypeReferences(TypeSyntax type, ImmutableArray<SyntaxReference>.Builder references)
    {
        switch (type)
        {
            case ParenthesizedTypeSyntax parenthesized:
                AddTypeReferences(parenthesized.Type, references);
                break;
            case IntersectionTypeSyntax intersection:
                foreach (var constituent in intersection.Types)
                    AddTypeReferences(constituent, references);
                break;
            default:
                references.Add(type.GetReference());
                break;
        }
    }

    internal static void ValidateIntersectionBounds(SourceTypeParameterSymbol parameter, DiagnosticBag diagnostics)
    {
        if (!parameter.ConstraintTypeReferences.Any(reference =>
                reference.GetSyntax().Ancestors().Any(node => node is IntersectionTypeSyntax)))
            return;

        ITypeSymbol? classBound = null;
        for (var i = 0; i < parameter.ConstraintTypes.Length; i++)
        {
            var type = parameter.ConstraintTypes[i];
            var syntax = parameter.ConstraintTypeReferences[i].GetSyntax();
            if (syntax is UnionTypeSyntax)
            {
                diagnostics.ReportInvalidIntersectionConstraint(syntax.GetLocation());
                continue;
            }

            if (type.TypeKind is TypeKind.Error or TypeKind.Interface)
                continue;

            if (type.TypeKind == TypeKind.Class &&
                (parameter.ConstraintKind & TypeParameterConstraintKind.ValueType) == 0 &&
                (classBound is null || SymbolEqualityComparer.Default.Equals(classBound, type)))
            {
                classBound = type;
                continue;
            }

            diagnostics.ReportInvalidIntersectionConstraint(
                syntax.GetLocation());
        }
    }
}
