using System.Collections.Immutable;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Discover the entire bound declaration graph before target admission. This is not
// reachability analysis: an uncalled synthesized member is still part of the contract.
internal sealed record SourceUnionDeclarationPlan(
    SourceUnionSymbol Union, ImmutableArray<SourceUnionTypeDeclaration> Types)
{
    internal static SourceUnionDeclarationPlan Create(SourceUnionSymbol union)
    {
        var types = ImmutableArray.CreateBuilder<SourceUnionTypeDeclaration>();
        var seen = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
        Add(union);
        var caseOwner = union.GetMetadataCaseContainer();
        if (!SymbolEqualityComparer.Default.Equals(caseOwner, union)) Add(caseOwner);
        foreach (var @case in union.DeclaredCaseTypes) Add(@case);
        return new(union, types.ToImmutable());

        void Add(INamedTypeSymbol type)
        {
            if (!seen.Add(type)) return;
            var methods = ImmutableArray.CreateBuilder<IMethodSymbol>();
            var seenMethods = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
            var members = type.GetMembers()
                .Where(m => SymbolEqualityComparer.Default.Equals(m.ContainingType, type))
                .ToImmutableArray();
            foreach (var member in members)
            {
                if (member is IMethodSymbol method) AddMethod(method);
                if (member is IPropertySymbol property)
                {
                    AddMethod(property.GetMethod);
                    AddMethod(property.SetMethod);
                }
            }
            var owner = type is SourceUnionCaseTypeSymbol caseType ? caseType.MetadataContainingType : type.ContainingType;
            types.Add(new(type, owner, members, methods.ToImmutable()));
            foreach (var nested in members.OfType<INamedTypeSymbol>())
            {
                // Cases are added after their physical companion, not beneath a
                // semantic generic carrier that does not own their metadata.
                if (nested is not IUnionCaseTypeSymbol) Add(nested);
            }
            void AddMethod(IMethodSymbol? method)
            {
                if (method is not null && seenMethods.Add(method)) methods.Add(method);
            }
        }
    }
}

internal sealed record SourceUnionTypeDeclaration(
    INamedTypeSymbol Symbol, INamedTypeSymbol? MetadataOwner,
    ImmutableArray<ISymbol> Members, ImmutableArray<IMethodSymbol> Methods)
{
    internal IEnumerable<IFieldSymbol> Fields => Members.OfType<IFieldSymbol>();
    internal IEnumerable<IPropertySymbol> Properties => Members.OfType<IPropertySymbol>();
}
