using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Introspection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.NeoClr;

// Raven interprets its language-level attribute contract; the facade owns metadata
// decoding and identity resolution. No code is loaded or executed during this step.
internal sealed class NativeUnionContracts
{
    internal sealed record Case(uint Token, uint UnionToken, string Name, int Ordinal);
    internal Dictionary<uint, ImmutableArray<Case>> Unions { get; } = [];
    internal Dictionary<uint, Case> Cases { get; } = [];
    internal Dictionary<uint, uint> Companions { get; } = [];

    internal NativeUnionContracts(IEnumerable<NominalTypeInfo> types)
    {
        var declarations = types.ToArray();
        var byName = declarations.ToDictionary(t => t.FullName, StringComparer.Ordinal);
        foreach (var type in declarations)
        {
            var attributes = type.GetCustomAttributes();
            var marker = attributes.Where(a => Is(a, "System.Runtime.CompilerServices", "UnionAttribute")).ToArray();
            var companions = attributes.Where(a => Is(a, "Raven.Runtime.CompilerServices", "RavenUnionCompanionAttribute")).ToArray();
            if (marker.Length > 1 || companions.Length > 1 || marker.Length != 0 && companions.Length != 0)
                throw new InvalidDataException("conflicting union metadata markers");
            if (marker.Length == 0 && attributes.Any(a => Is(a, "Raven.Runtime.CompilerServices", "RavenUnionCaseAttribute")))
                throw new InvalidDataException("case relationships require a union marker");
            if (marker.Length == 1)
            {
                if (!type.IsValueType || marker[0].GetArguments().Count != 0)
                    throw new InvalidDataException("unsupported native union carrier contract");
                var cases = ImmutableArray.CreateBuilder<Case>();
                foreach (var attribute in attributes.Where(a => Is(a, "Raven.Runtime.CompilerServices", "RavenUnionCaseAttribute")))
                {
                    if (attribute.GetArguments() is not [{ Value: string caseName }, { Value: string name }, { Value: int ordinal }] ||
                        !byName.TryGetValue(caseName, out var caseType) || !caseType.IsValueType || string.IsNullOrWhiteSpace(name) || ordinal < 0)
                        throw new InvalidDataException("invalid native union case contract");
                    cases.Add(new(caseType.MetadataToken, type.MetadataToken, name, ordinal));
                }
                var ordered = cases.OrderBy(c => c.Ordinal).ToImmutableArray();
                if ((ordered.IsEmpty && !HasConstructorUnionContract(type)) || !ordered.Select(c => c.Ordinal).SequenceEqual(Enumerable.Range(0, ordered.Length)) ||
                    ordered.Select(c => c.Name).Distinct(StringComparer.Ordinal).Count() != ordered.Length)
                    throw new InvalidDataException("missing or conflicting native union cases");
                Unions.Add(type.MetadataToken, ordered);
                foreach (var item in ordered)
                    if (!Cases.TryAdd(item.Token, item)) throw new InvalidDataException("native case belongs to multiple unions");
            }
            if (companions.Length == 1)
            {
                if (companions[0].GetArguments() is not [{ Value: string name }] ||
                    !byName.TryGetValue(name, out var union) || type.GenericArity != 0 || !type.IsStatic || union.GenericArity == 0)
                    throw new InvalidDataException("invalid native union companion contract");
                Companions.Add(type.MetadataToken, union.MetadataToken);
            }
        }
        foreach (var companion in Companions)
            if (!Unions.ContainsKey(companion.Value) || Companions.Count(p => p.Value == companion.Value) != 1)
                throw new InvalidDataException("unresolved or duplicate native union companion");
        foreach (var type in declarations.Where(t => Cases.ContainsKey(t.MetadataToken)))
        {
            if (type.GetConstructors().Count(c => c.IsConstructor) != 1)
                throw new InvalidDataException("native union case requires exactly one instance constructor");
            var item = Cases[type.MetadataToken];
            if (type.DeclaringType is not { } parent ||
                parent.MetadataToken != item.UnionToken && (!Companions.TryGetValue(parent.MetadataToken, out var union) || union != item.UnionToken))
                throw new InvalidDataException("native case physical owner disagrees with union contract");
        }
    }

    // Constructor unions have no named case attributes. Require the public shape
    // used by Raven's CLI importer, rather than accepting an empty marker alone.
    private static bool HasConstructorUnionContract(NominalTypeInfo type)
        => type.GetConstructors().Any(c => c.IsConstructor && c.Accessibility == MetadataAccessibility.Public &&
            c.GetParameters() is [{ PassingMode: ParameterPassingMode.Value }]) &&
            type.GetProperties().Any(p => p.Name == "Value" && !p.IsStatic && p.IndexParameterTypes.Count == 0 &&
                p.GetMethod is { Accessibility: MetadataAccessibility.Public } && p.PropertyType is NominalTypeInfo { FullName: "System.Object" });

    private static bool Is(CustomAttributeInfo attribute, string ns, string name)
    {
        if (attribute.Namespace != ns || attribute.Name != name) return false;
        _ = attribute.GetAttributeType(); // Require its actual metadata dependency, not a name-only fallback.
        return true;
    }
}

internal sealed class NativeUnionSymbol(Compilation compilation, NominalTypeInfo view, NativeNamespaceSymbol owner, NativeNamedTypeSymbol? parent)
    : NativeNamedTypeSymbol(compilation, view, owner, parent), IUnionSymbol
{
    private ImmutableArray<IUnionCaseTypeSymbol> cases = [];
    internal void SetCases(IEnumerable<IUnionCaseTypeSymbol> values) => cases = [.. values];
    public ImmutableArray<IUnionCaseTypeSymbol> DeclaredCaseTypes => cases;
    private ImmutableArray<ITypeSymbol>? memberTypes;
    private bool contentMayBeNull;
    public ImmutableArray<ITypeSymbol> Variants => MemberTypes;
    public ImmutableArray<ITypeSymbol> MemberTypes
    {
        get
        {
            if (memberTypes is { } cached) return cached;
            if (!cases.IsEmpty) return (memberTypes = [.. cases]).Value;
            var members = ImmutableArray.CreateBuilder<ITypeSymbol>();
            foreach (var constructor in InstanceConstructors.Where(c => c.DeclaredAccessibility == Accessibility.Public &&
                c.Parameters is [{ RefKind: RefKind.None }]))
            {
                var type = UnionContentNullability.GetNonNullContentType(constructor.Parameters[0].Type, out var nullable);
                contentMayBeNull |= nullable;
                if (!members.Any(existing => SymbolEqualityComparer.Default.Equals(existing, type))) members.Add(type);
            }
            return (memberTypes = members.ToImmutable()).Value;
        }
    }
    public bool ContentMayBeNull { get { _ = MemberTypes; return contentMayBeNull; } }
    public IFieldSymbol DiscriminatorField => GetMembers().OfType<IFieldSymbol>().Single(f => UnionFieldUtilities.IsTagFieldName(f.Name));
    public IFieldSymbol PayloadField => GetMembers().OfType<IFieldSymbol>().First(f => UnionFieldUtilities.IsPayloadFieldName(f.Name));
}

internal sealed class NativeUnionCaseSymbol : NativeNamedTypeSymbol, IUnionCaseTypeSymbol
{
    internal NativeUnionCaseSymbol(Compilation compilation, NominalTypeInfo view, NativeNamespaceSymbol owner,
        NativeUnionSymbol union, NativeNamedTypeSymbol physicalOwner, NativeUnionContracts.Case contract)
        : base(compilation, view, owner, union)
    {
        Union = union;
        MetadataContainingType = physicalOwner;
        Name = contract.Name;
        Ordinal = contract.Ordinal;
    }
    public override string Name { get; }
    public IUnionSymbol Union { get; }
    public INamedTypeSymbol MetadataContainingType { get; }
    public int Ordinal { get; }
    public ImmutableArray<IParameterSymbol> ConstructorParameters => InstanceConstructors.Single().Parameters;
}

internal sealed class NativeUnionCompanionSymbol(Compilation compilation, NominalTypeInfo view, NativeNamespaceSymbol owner,
    NativeNamedTypeSymbol? parent, Func<IUnionSymbol> resolveUnion)
    : NativeNamedTypeSymbol(compilation, view, owner, parent), IUnionCompanionSymbol
{
    public bool TryGetAssociatedUnion(out IUnionSymbol union)
    {
        union = resolveUnion();
        return true;
    }
}
