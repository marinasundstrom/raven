using System.Collections.Immutable;
using System.Diagnostics;

namespace Raven.CodeAnalysis.Symbols;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
internal sealed class ConstructedNamedTypeSymbol : INamedTypeSymbol
{
    private readonly PENamedTypeSymbol _originalDefinition;
    private readonly Dictionary<ITypeParameterSymbol, ITypeSymbol> _substitutionMap;
    private ImmutableArray<ISymbol>? _members;

    public ImmutableArray<ITypeSymbol> TypeArguments { get; }

    public ConstructedNamedTypeSymbol(PENamedTypeSymbol originalDefinition, ImmutableArray<ITypeSymbol> typeArguments)
    {
        ConstructedFrom = originalDefinition;
        _originalDefinition = originalDefinition;
        TypeArguments = typeArguments;

        _substitutionMap = originalDefinition.TypeParameters
            .Zip(TypeArguments, (p, a) => (p, a))
            .ToDictionary(x => x.p, x => x.a);

        _substitutionMap = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(_substitutionMap, SymbolEqualityComparer.Default);
    }

    public ITypeSymbol Substitute(ITypeSymbol type)
    {
        if (type is ITypeParameterSymbol tp && _substitutionMap.TryGetValue(tp, out var concrete))
            return concrete;

        if (type is INamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
        {
            var args = named.TypeArguments.Select(Substitute).ToImmutableArray();
            return new ConstructedNamedTypeSymbol((PENamedTypeSymbol)named.ConstructedFrom!, args);
        }

        return type;
    }

    public ImmutableArray<ISymbol> GetMembers() =>
        _members ??= _originalDefinition.GetMembers().Select(SubstituteMember).ToImmutableArray();

    public ImmutableArray<ISymbol> GetMembers(string name) =>
        GetMembers().Where(m => m.Name == name).ToImmutableArray();

    private ISymbol SubstituteMember(ISymbol member) => member switch
    {
        IMethodSymbol m => new SubstitutedMethodSymbol(m, this),
        IFieldSymbol f => new SubstitutedFieldSymbol(f, this),
        IPropertySymbol p => new SubstitutedPropertySymbol(p, this),
        _ => member
    };

    // Symbol metadata forwarding
    public string Name => _originalDefinition.Name;
    public string MetadataName => _originalDefinition.MetadataName;
    public SymbolKind Kind => _originalDefinition.Kind;
    public TypeKind TypeKind => _originalDefinition.TypeKind;
    public SpecialType SpecialType => _originalDefinition.SpecialType;
    public bool IsNamespace => false;
    public bool IsType => true;
    public INamedTypeSymbol? ContainingType => _originalDefinition.ContainingType;
    public INamespaceSymbol? ContainingNamespace => _originalDefinition.ContainingNamespace;
    public ISymbol? ContainingSymbol => _originalDefinition.ContainingSymbol;
    public IAssemblySymbol? ContainingAssembly => _originalDefinition.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _originalDefinition.ContainingModule;
    public Accessibility DeclaredAccessibility => Accessibility.Public;
    public bool IsStatic => false;
    public bool IsImplicitlyDeclared => true;
    public bool CanBeReferencedByName => true;
    public ImmutableArray<Location> Locations => _originalDefinition.Locations;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => ImmutableArray<SyntaxReference>.Empty;
    public int Arity => TypeArguments.Length;
    public ImmutableArray<ITypeSymbol> GetTypeArguments() => TypeArguments;
    public ITypeSymbol? OriginalDefinition => _originalDefinition;
    public INamedTypeSymbol? BaseType => _originalDefinition.BaseType;
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _originalDefinition.TypeParameters;
    public ITypeSymbol ConstructedFrom { get; }
    public bool IsGenericType => true;
    public bool IsUnboundGenericType => false;
    public ImmutableArray<IMethodSymbol> Constructors => _originalDefinition.Constructors;
    public IMethodSymbol? StaticConstructor => _originalDefinition.StaticConstructor;

    public void Accept(SymbolVisitor visitor) => visitor.VisitNamedType(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamedType(this);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
    public ITypeSymbol Construct(ITypeSymbol[] typeArguments) => throw new NotSupportedException();

    public ITypeSymbol? LookupType(string name)
    {
        throw new NotImplementedException();
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        throw new NotImplementedException();
    }
}

internal sealed class SubstitutedMethodSymbol : IMethodSymbol
{
    private readonly IMethodSymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructed;
    private ImmutableArray<IParameterSymbol>? _parameters;

    public SubstitutedMethodSymbol(IMethodSymbol original, ConstructedNamedTypeSymbol constructed)
    {
        _original = original;
        _constructed = constructed;
    }

    public string Name => _original.Name;
    public ITypeSymbol ReturnType => _constructed.Substitute(_original.ReturnType);
    public ImmutableArray<IParameterSymbol> Parameters =>
        _parameters ??= _original.Parameters.Select(p => (IParameterSymbol)new SubstitutedParameterSymbol(p, _constructed)).ToImmutableArray();

    public ISymbol ContainingSymbol => _constructed;

    public MethodKind MethodKind => _original.MethodKind;
    public bool IsConstructor => _original.IsConstructor;
    public IMethodSymbol? OriginalDefinition => _original;
    public bool IsAbstract => _original.IsAbstract;
    public bool IsAsync => _original.IsAsync;
    public bool IsCheckedBuiltin => _original.IsCheckedBuiltin;
    public bool IsDefinition => _original.IsDefinition;
    public bool IsExtensionMethod => _original.IsExtensionMethod;
    public bool IsExtern => _original.IsExtern;
    public bool IsGenericMethod => _original.IsGenericMethod;
    public bool IsOverride => _original.IsOverride;
    public bool IsReadOnly => _original.IsReadOnly;
    public bool IsSealed => _original.IsSealed;
    public bool IsVirtual => _original.IsVirtual;
    public SymbolKind Kind => _original.Kind;
    public string MetadataName => _original.MetadataName;
    public IAssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _original.ContainingModule;
    public INamedTypeSymbol? ContainingType => _constructed;
    public INamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => _original.IsStatic;

    public void Accept(SymbolVisitor visitor)
    {
        visitor.VisitMethod(this);
    }

    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
    {
        return visitor.VisitMethod(this);
    }

    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) =>
               comparer.Equals(this, other);

    public bool Equals(ISymbol? other) =>
        SymbolEqualityComparer.Default.Equals(this, other);
}

internal sealed class SubstitutedFieldSymbol : IFieldSymbol
{
    private readonly IFieldSymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructed;

    public SubstitutedFieldSymbol(IFieldSymbol original, ConstructedNamedTypeSymbol constructed)
    {
        _original = original;
        _constructed = constructed;
    }

    public string Name => _original.Name;
    public ITypeSymbol Type => _constructed.Substitute(_original.Type);
    public ISymbol ContainingSymbol => _constructed;

    public bool IsLiteral => _original.IsLiteral;
    public SymbolKind Kind => _original.Kind;
    public string MetadataName => _original.MetadataName;
    public IAssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _original.ContainingModule;
    public INamedTypeSymbol? ContainingType => _constructed;
    public INamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => _original.IsStatic;

    public void Accept(SymbolVisitor visitor) => visitor.VisitField(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitField(this);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);

    public object? GetConstantValue() => _original.GetConstantValue();
}

internal sealed class SubstitutedPropertySymbol : IPropertySymbol
{
    private readonly IPropertySymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructed;

    public SubstitutedPropertySymbol(IPropertySymbol original, ConstructedNamedTypeSymbol constructed)
    {
        _original = original;
        _constructed = constructed;
    }

    public string Name => _original.Name;
    public ITypeSymbol Type => _constructed.Substitute(_original.Type);
    public ISymbol ContainingSymbol => _constructed;

    public IMethodSymbol? GetMethod => _original.GetMethod is null ? null : new SubstitutedMethodSymbol(_original.GetMethod, _constructed);
    public IMethodSymbol? SetMethod => _original.SetMethod is null ? null : new SubstitutedMethodSymbol(_original.SetMethod, _constructed);
    public bool IsIndexer => _original.IsIndexer;
    public SymbolKind Kind => _original.Kind;
    public string MetadataName => _original.MetadataName;
    public IAssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _original.ContainingModule;
    public INamedTypeSymbol? ContainingType => _constructed;
    public INamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => _original.IsStatic;

    public void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
}

internal sealed class SubstitutedParameterSymbol : IParameterSymbol
{
    private readonly IParameterSymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructed;

    public SubstitutedParameterSymbol(IParameterSymbol original, ConstructedNamedTypeSymbol constructed)
    {
        _original = original;
        _constructed = constructed;
    }

    public string Name => _original.Name;
    public ITypeSymbol Type => _constructed.Substitute(_original.Type);

    public SymbolKind Kind => _original.Kind;
    public string MetadataName => _original.MetadataName;
    public ISymbol? ContainingSymbol => _constructed; // Adjust depending on method/field owner if needed
    public IAssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _original.ContainingModule;
    public INamedTypeSymbol? ContainingType => _constructed;
    public INamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => false;

    public void Accept(SymbolVisitor visitor) => visitor.VisitParameter(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitParameter(this);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);

    private string GetDebuggerDisplay()
    {
        try
        {
            return $"{Kind}: {this.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}";
        }
        catch (Exception exc)
        {
            return $"{Kind}: <{exc.GetType().Name}>";
        }
    }

    public override string ToString()
    {
        return this.ToDisplayString();
    }
}