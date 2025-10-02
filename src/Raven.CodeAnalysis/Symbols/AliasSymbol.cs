using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class AliasSymbol : IAliasSymbol
{
    protected AliasSymbol(string name, ISymbol underlying)
    {
        Name = name;
        UnderlyingSymbol = underlying;
    }

    public SymbolKind Kind => UnderlyingSymbol.Kind;

    public string Name { get; }

    public string MetadataName => UnderlyingSymbol.MetadataName;

    public ISymbol? ContainingSymbol => UnderlyingSymbol.ContainingSymbol;

    public IAssemblySymbol? ContainingAssembly => UnderlyingSymbol.ContainingAssembly;

    public IModuleSymbol? ContainingModule => UnderlyingSymbol.ContainingModule;

    public INamedTypeSymbol? ContainingType => UnderlyingSymbol.ContainingType;

    public INamespaceSymbol? ContainingNamespace => UnderlyingSymbol.ContainingNamespace;

    public ImmutableArray<Location> Locations => UnderlyingSymbol.Locations;

    public Accessibility DeclaredAccessibility => UnderlyingSymbol.DeclaredAccessibility;

    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => UnderlyingSymbol.DeclaringSyntaxReferences;

    public bool IsImplicitlyDeclared => UnderlyingSymbol.IsImplicitlyDeclared;

    public bool IsStatic => UnderlyingSymbol.IsStatic;

    public ISymbol UnderlyingSymbol { get; }

    public bool IsAlias => true;

    public ImmutableArray<AttributeData> GetAttributes() => UnderlyingSymbol.GetAttributes();

    public bool Equals(ISymbol? other) =>
        UnderlyingSymbol.Equals(other is IAliasSymbol alias ? alias.UnderlyingSymbol : other);

    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) =>
        UnderlyingSymbol.Equals(other is IAliasSymbol alias ? alias.UnderlyingSymbol : other, comparer);

    public void Accept(SymbolVisitor visitor) => UnderlyingSymbol.Accept(visitor);

    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => UnderlyingSymbol.Accept(visitor);
}

internal sealed class AliasNamespaceSymbol : AliasSymbol, INamespaceSymbol
{
    private readonly INamespaceSymbol _namespace;

    public AliasNamespaceSymbol(string name, INamespaceSymbol underlying)
        : base(name, underlying)
    {
        _namespace = underlying;
    }

    public bool IsNamespace => _namespace.IsNamespace;

    public bool IsType => _namespace.IsType;

    public ImmutableArray<ISymbol> GetMembers() => _namespace.GetMembers();

    public ImmutableArray<ISymbol> GetMembers(string name) => _namespace.GetMembers(name);

    public ITypeSymbol? LookupType(string name) => _namespace.LookupType(name);

    public bool IsMemberDefined(string name, out ISymbol? symbol) => _namespace.IsMemberDefined(name, out symbol);

    public bool IsGlobalNamespace => _namespace.IsGlobalNamespace;

    public INamespaceSymbol? LookupNamespace(string name) => _namespace.LookupNamespace(name);

    public string? ToMetadataName() => _namespace.ToMetadataName();
}

internal sealed class AliasNamedTypeSymbol : AliasSymbol, INamedTypeSymbol
{
    private readonly INamedTypeSymbol _type;

    public AliasNamedTypeSymbol(string name, INamedTypeSymbol underlying)
        : base(name, underlying)
    {
        _type = underlying;
    }

    public bool IsNamespace => _type.IsNamespace;

    public bool IsType => _type.IsType;

    public ImmutableArray<ISymbol> GetMembers() => _type.GetMembers();

    public ImmutableArray<ISymbol> GetMembers(string name) => _type.GetMembers(name);

    public ITypeSymbol? LookupType(string name) => _type.LookupType(name);

    public bool IsMemberDefined(string name, out ISymbol? symbol) => _type.IsMemberDefined(name, out symbol);

    public INamedTypeSymbol? BaseType => _type.BaseType;

    public ITypeSymbol? OriginalDefinition => _type.OriginalDefinition;

    public SpecialType SpecialType => _type.SpecialType;

    public TypeKind TypeKind => _type.TypeKind;

    public bool IsReferenceType => _type.IsReferenceType;

    public bool IsValueType => _type.IsValueType;

    public bool IsTupleType => _type.IsTupleType;

    public bool IsUnion => _type.IsUnion;

    public ImmutableArray<INamedTypeSymbol> Interfaces => _type.Interfaces;

    public ImmutableArray<INamedTypeSymbol> AllInterfaces => _type.AllInterfaces;

    public int Arity => _type.Arity;

    public ImmutableArray<IMethodSymbol> Constructors => _type.Constructors;

    public IMethodSymbol? StaticConstructor => _type.StaticConstructor;

    public INamedTypeSymbol UnderlyingTupleType => _type.UnderlyingTupleType;

    public ImmutableArray<IFieldSymbol> TupleElements => _type.TupleElements;

    public ImmutableArray<ITypeSymbol> TypeArguments => _type.TypeArguments;

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _type.TypeParameters;

    public ITypeSymbol? ConstructedFrom => _type.ConstructedFrom;

    public bool IsAbstract => _type.IsAbstract;

    public bool IsSealed => _type.IsSealed;

    public bool IsGenericType => _type.IsGenericType;

    public bool IsUnboundGenericType => _type.IsUnboundGenericType;

    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments) => _type.Construct(typeArguments);
}

internal sealed class AliasUnionTypeSymbol : AliasSymbol, IUnionTypeSymbol
{
    private readonly IUnionTypeSymbol _type;

    public AliasUnionTypeSymbol(string name, IUnionTypeSymbol underlying)
        : base(name, underlying)
    {
        _type = underlying;
    }

    public bool IsNamespace => _type.IsNamespace;

    public bool IsType => _type.IsType;

    public ImmutableArray<ISymbol> GetMembers() => _type.GetMembers();

    public ImmutableArray<ISymbol> GetMembers(string name) => _type.GetMembers(name);

    public ITypeSymbol? LookupType(string name) => _type.LookupType(name);

    public bool IsMemberDefined(string name, out ISymbol? symbol) => _type.IsMemberDefined(name, out symbol);

    public INamedTypeSymbol? BaseType => _type.BaseType;

    public ITypeSymbol? OriginalDefinition => _type.OriginalDefinition;

    public SpecialType SpecialType => _type.SpecialType;

    public TypeKind TypeKind => _type.TypeKind;

    public bool IsReferenceType => _type.IsReferenceType;

    public bool IsValueType => _type.IsValueType;

    public IEnumerable<ITypeSymbol> Types => _type.Types;

    public ImmutableArray<INamedTypeSymbol> Interfaces => _type.Interfaces;

    public ImmutableArray<INamedTypeSymbol> AllInterfaces => _type.AllInterfaces;
}

internal sealed class AliasLiteralTypeSymbol : AliasSymbol, ITypeSymbol
{
    private readonly LiteralTypeSymbol _type;

    public AliasLiteralTypeSymbol(string name, LiteralTypeSymbol underlying)
        : base(name, underlying)
    {
        _type = underlying;
    }

    public bool IsNamespace => _type.IsNamespace;

    public bool IsType => _type.IsType;

    public ImmutableArray<ISymbol> GetMembers() => _type.GetMembers();

    public ImmutableArray<ISymbol> GetMembers(string name) => _type.GetMembers(name);

    public ITypeSymbol? LookupType(string name) => _type.LookupType(name);

    public bool IsMemberDefined(string name, out ISymbol? symbol) => _type.IsMemberDefined(name, out symbol);

    public INamedTypeSymbol? BaseType => _type.BaseType;

    public ITypeSymbol? OriginalDefinition => _type.OriginalDefinition;

    public SpecialType SpecialType => _type.SpecialType;

    public TypeKind TypeKind => _type.TypeKind;

    public bool IsReferenceType => _type.IsReferenceType;

    public bool IsValueType => _type.IsValueType;

    public ImmutableArray<INamedTypeSymbol> Interfaces => _type.Interfaces;

    public ImmutableArray<INamedTypeSymbol> AllInterfaces => _type.AllInterfaces;
}

internal sealed class AliasMethodSymbol : AliasSymbol, IMethodSymbol
{
    private readonly IMethodSymbol _method;

    public AliasMethodSymbol(string name, IMethodSymbol underlying)
        : base(name, underlying)
    {
        _method = underlying;
    }

    public MethodKind MethodKind => _method.MethodKind;

    public ITypeSymbol ReturnType => _method.ReturnType;

    public ImmutableArray<IParameterSymbol> Parameters => _method.Parameters;

    public IMethodSymbol? OriginalDefinition => _method.OriginalDefinition;

    public bool IsAbstract => _method.IsAbstract;

    public bool IsAsync => _method.IsAsync;

    public bool IsCheckedBuiltin => _method.IsCheckedBuiltin;

    public bool IsDefinition => _method.IsDefinition;

    public bool IsExtensionMethod => _method.IsExtensionMethod;

    public bool IsExtern => _method.IsExtern;

    public bool IsGenericMethod => _method.IsGenericMethod;

    public bool IsOverride => _method.IsOverride;

    public bool IsReadOnly => _method.IsReadOnly;

    public bool IsSealed => _method.IsSealed;

    public bool IsVirtual => _method.IsVirtual;

    public bool IsIterator => _method.IsIterator;

    public IteratorMethodKind IteratorKind => _method.IteratorKind;

    public ITypeSymbol? IteratorElementType => _method.IteratorElementType;

    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => _method.ExplicitInterfaceImplementations;

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _method.TypeParameters;

    public ImmutableArray<ITypeSymbol> TypeArguments => _method.TypeArguments;

    public IMethodSymbol? ConstructedFrom => _method.ConstructedFrom;

    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => _method.GetReturnTypeAttributes();

    public IMethodSymbol Construct(params ITypeSymbol[] typeArguments) => _method.Construct(typeArguments);
}

internal sealed class AliasPropertySymbol : AliasSymbol, IPropertySymbol
{
    private readonly IPropertySymbol _property;

    public AliasPropertySymbol(string name, IPropertySymbol underlying)
        : base(name, underlying)
    {
        _property = underlying;
    }

    public ITypeSymbol Type => _property.Type;

    public IMethodSymbol? GetMethod => _property.GetMethod;

    public IMethodSymbol? SetMethod => _property.SetMethod;

    public bool IsIndexer => _property.IsIndexer;
}

internal sealed class AliasFieldSymbol : AliasSymbol, IFieldSymbol
{
    private readonly IFieldSymbol _field;

    public AliasFieldSymbol(string name, IFieldSymbol underlying)
        : base(name, underlying)
    {
        _field = underlying;
    }

    public ITypeSymbol Type => _field.Type;

    public bool IsLiteral => _field.IsLiteral;

    public object? GetConstantValue() => _field.GetConstantValue();
}

internal static class AliasSymbolFactory
{
    public static IAliasSymbol Create(string name, ISymbol underlying) => underlying switch
    {
        INamespaceSymbol n => new AliasNamespaceSymbol(name, n),
        IUnionTypeSymbol u => new AliasUnionTypeSymbol(name, u),
        INamedTypeSymbol t => new AliasNamedTypeSymbol(name, t),
        LiteralTypeSymbol l => new AliasLiteralTypeSymbol(name, l),
        IMethodSymbol m => new AliasMethodSymbol(name, m),
        IPropertySymbol p => new AliasPropertySymbol(name, p),
        IFieldSymbol f => new AliasFieldSymbol(name, f),
        _ => throw new ArgumentException("Unsupported alias target", nameof(underlying))
    };
}
