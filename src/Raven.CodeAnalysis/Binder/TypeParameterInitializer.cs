using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class TypeParameterInitializer
{
    public static void InitializeMethodTypeParameters(
        SourceMethodSymbol methodSymbol,
        INamedTypeSymbol containingType, // container used as "declaring type context" in your symbols
        TypeParameterListSyntax? typeParameterList,
        SyntaxList<TypeParameterConstraintClauseSyntax> constraintClauses,
        SyntaxTree syntaxTree,
        DiagnosticBag? diagnostics = null)
    {
        if (typeParameterList is null || typeParameterList.Parameters.Count == 0)
            return;

        Dictionary<string, List<TypeParameterConstraintClauseSyntax>>? clausesByName = null;
        if (constraintClauses.Count > 0)
        {
            clausesByName = new(StringComparer.Ordinal);

            foreach (var clause in constraintClauses)
            {
                var name = clause.TypeParameter.Identifier.ValueText;

                if (!clausesByName.TryGetValue(name, out var list))
                    clausesByName[name] = list = new List<TypeParameterConstraintClauseSyntax>();

                list.Add(clause);
            }
        }

        // Optional validation: clause refers to an unknown type parameter / duplicates etc.
        if (diagnostics is not null && clausesByName is not null)
        {
            var declared = new HashSet<string>(
                typeParameterList.Parameters.Select(p => p.Identifier.ValueText),
                StringComparer.Ordinal);

            foreach (var (name, list) in clausesByName)
            {
                if (!declared.Contains(name))
                {
                    // TODO: diagnostics.Report... UnknownTypeParameterInConstraintClause(...)
                }

                // If you want to forbid multiple clauses per parameter:
                // if (list.Count > 1) diagnostics.Report... DuplicateConstraintClause(...)
            }
        }

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(typeParameterList.Parameters.Count);
        int ordinal = 0;

        foreach (var parameter in typeParameterList.Parameters)
        {
            var identifier = parameter.Identifier;
            var location = syntaxTree.GetLocation(identifier.Span);
            var reference = parameter.GetReference();

            var (inlineKind, inlineRefs) = TypeParameterConstraintAnalyzer.AnalyzeInline(parameter);

            var clauseKind = TypeParameterConstraintKind.None;
            var clauseRefsBuilder = ImmutableArray.CreateBuilder<SyntaxReference>();

            if (clausesByName is not null &&
                clausesByName.TryGetValue(identifier.ValueText, out var matchingClauses))
            {
                foreach (var clause in matchingClauses)
                {
                    var (k, refs) = TypeParameterConstraintAnalyzer.AnalyzeClause(clause);
                    clauseKind |= k;
                    clauseRefsBuilder.AddRange(refs);
                }
            }

            var mergedKind = inlineKind | clauseKind;
            var mergedRefs = inlineRefs.AddRange(clauseRefsBuilder.ToImmutable());

            var variance = GetDeclaredVariance(parameter);

            builder.Add(new SourceTypeParameterSymbol(
                identifier.ValueText,
                methodSymbol,
                containingType,
                methodSymbol.ContainingNamespace,
                [location],
                [reference],
                ordinal++,
                mergedKind,
                mergedRefs,
                variance));
        }

        methodSymbol.SetTypeParameters(builder);
    }

    private static VarianceKind GetDeclaredVariance(TypeParameterSyntax parameter)
        => parameter.VarianceKeyword?.Kind switch
        {
            SyntaxKind.OutKeyword => VarianceKind.Out,
            SyntaxKind.InKeyword => VarianceKind.In,
            _ => VarianceKind.None,
        };
}
