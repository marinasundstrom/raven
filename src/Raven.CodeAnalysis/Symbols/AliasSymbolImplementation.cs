using System;
using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Documentation;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class AliasSymbolImplementation : AliasSymbol
{
    protected AliasSymbolImplementation(string name, Symbol underlying)
    {
        Name = name;
        UnderlyingSymbol = underlying;
    }

    public SymbolKind Kind => UnderlyingSymbol.Kind;

    public string Name { get; }

    public string MetadataName => UnderlyingSymbol.MetadataName;

    public Symbol? ContainingSymbol => UnderlyingSymbol.ContainingSymbol;

    public AssemblySymbol? ContainingAssembly => UnderlyingSymbol.ContainingAssembly;

    public ModuleSymbol? ContainingModule => UnderlyingSymbol.ContainingModule;

    public NamedTypeSymbol? ContainingType => UnderlyingSymbol.ContainingType;

    public NamespaceSymbol? ContainingNamespace => null; //UnderlyingSymbol.ContainingNamespace;

    public ImmutableArray<Location> Locations => UnderlyingSymbol.Locations;

    public Accessibility DeclaredAccessibility => UnderlyingSymbol.DeclaredAccessibility;

    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => UnderlyingSymbol.DeclaringSyntaxReferences;

    public bool IsImplicitlyDeclared => UnderlyingSymbol.IsImplicitlyDeclared;

    public bool IsStatic => UnderlyingSymbol.IsStatic;

    public Symbol UnderlyingSymbol { get; }

    public bool IsAlias => true;

    public ImmutableArray<AttributeData> GetAttributes() => UnderlyingSymbol.GetAttributes();
    public DocumentationComment? GetDocumentationComment() => UnderlyingSymbol.GetDocumentationComment();

    public bool Equals(Symbol? other) =>
        UnderlyingSymbol.Equals(other is AliasSymbol alias ? alias.UnderlyingSymbol : other);

    public bool Equals(Symbol? other, SymbolEqualityComparer comparer) =>
        UnderlyingSymbol.Equals(other is AliasSymbol alias ? alias.UnderlyingSymbol : other, comparer);

    public void Accept(SymbolVisitor visitor) => UnderlyingSymbol.Accept(visitor);

    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => UnderlyingSymbol.Accept(visitor);
}

internal sealed class AliasNamespaceSymbol : AliasSymbolImplementation, NamespaceSymbol
{
    private readonly NamespaceSymbol _namespace;

    public AliasNamespaceSymbol(string name, NamespaceSymbol underlying)
        : base(name, underlying)
    {
        _namespace = underlying;
    }

    public bool IsNamespace => _namespace.IsNamespace;

    public bool IsType => _namespace.IsType;

    public ImmutableArray<Symbol> GetMembers() => _namespace.GetMembers();

    public ImmutableArray<Symbol> GetMembers(string name) => _namespace.GetMembers(name);

    public TypeSymbol? LookupType(string name) => _namespace.LookupType(name);

    public bool IsMemberDefined(string name, out Symbol? symbol) => _namespace.IsMemberDefined(name, out symbol);

    public bool IsGlobalNamespace => _namespace.IsGlobalNamespace;

    public NamespaceSymbol? LookupNamespace(string name) => _namespace.LookupNamespace(name);

    public string? ToMetadataName() => _namespace.ToMetadataName();
}

internal sealed class AliasNamedTypeSymbol : AliasSymbolImplementation, NamedTypeSymbol
{
    private readonly NamedTypeSymbol _type;

    public AliasNamedTypeSymbol(string name, NamedTypeSymbol underlying)
        : base(name, underlying)
    {
        _type = underlying;
    }

    public bool IsNamespace => _type.IsNamespace;

    public bool IsType => _type.IsType;

    public ImmutableArray<Symbol> GetMembers() => _type.GetMembers();

    public ImmutableArray<Symbol> GetMembers(string name) => _type.GetMembers(name);

    public TypeSymbol? LookupType(string name) => _type.LookupType(name);

    public bool IsMemberDefined(string name, out Symbol? symbol) => _type.IsMemberDefined(name, out symbol);

    public NamedTypeSymbol? BaseType => _type.BaseType;

    public TypeSymbol? OriginalDefinition => _type.OriginalDefinition;

    public SpecialType SpecialType => _type.SpecialType;

    public TypeKind TypeKind => _type.TypeKind;

    public bool IsReferenceType => _type.IsReferenceType;

    public bool IsValueType => _type.IsValueType;

    public bool IsTupleType => _type.IsTupleType;

    public bool IsUnion => _type.IsUnion;

    public bool IsUnionCase => _type.IsUnionCase;

    public NamedTypeSymbol? UnderlyingUnionType => _type.UnderlyingUnionType;

    public ImmutableArray<NamedTypeSymbol> Interfaces => _type.Interfaces;

    public ImmutableArray<NamedTypeSymbol> AllInterfaces => _type.AllInterfaces;

    public int Arity => _type.Arity;

    public ImmutableArray<MethodSymbol> Constructors => _type.Constructors;

    public MethodSymbol? StaticConstructor => _type.StaticConstructor;

    public NamedTypeSymbol? UnderlyingTupleType => _type.UnderlyingTupleType;

    public ImmutableArray<FieldSymbol> TupleElements => _type.TupleElements;

    public ImmutableArray<TypeSymbol> TypeArguments => _type.TypeArguments;

    public ImmutableArray<TypeParameterSymbol> TypeParameters => _type.TypeParameters;

    public TypeSymbol? ConstructedFrom => _type.ConstructedFrom;

    public bool IsAbstract => _type.IsAbstract;

    public bool IsClosed => _type.IsClosed;

    public bool IsRefLikeType => _type.IsRefLikeType;

    public bool IsGenericType => _type.IsGenericType;

    public bool IsUnboundGenericType => _type.IsUnboundGenericType;

    public ImmutableArray<MethodSymbol> InstanceConstructors => _type.InstanceConstructors;

    public TypeSymbol Construct(params TypeSymbol[] typeArguments) => _type.Construct(typeArguments);
}

internal sealed class AliasTypeSymbol : AliasSymbolImplementation, TypeSymbol
{
    private readonly TypeSymbol _type;

    public AliasTypeSymbol(string name, TypeSymbol underlying)
        : base(name, underlying)
    {
        _type = underlying;
    }

    public bool IsNamespace => _type.IsNamespace;

    public bool IsType => _type.IsType;

    public ImmutableArray<Symbol> GetMembers() => _type.GetMembers();

    public ImmutableArray<Symbol> GetMembers(string name) => _type.GetMembers(name);

    public TypeSymbol? LookupType(string name) => _type.LookupType(name);

    public bool IsMemberDefined(string name, out Symbol? symbol) => _type.IsMemberDefined(name, out symbol);

    public NamedTypeSymbol? BaseType => _type.BaseType;

    public TypeSymbol? OriginalDefinition => _type.OriginalDefinition;

    public SpecialType SpecialType => _type.SpecialType;

    public TypeKind TypeKind => _type.TypeKind;

    public bool IsReferenceType => _type.IsReferenceType;

    public bool IsValueType => _type.IsValueType;

    public ImmutableArray<NamedTypeSymbol> Interfaces => _type.Interfaces;

    public ImmutableArray<NamedTypeSymbol> AllInterfaces => _type.AllInterfaces;
}

internal sealed class AliasMethodSymbol : AliasSymbolImplementation, MethodSymbol
{
    private readonly MethodSymbol _method;

    public AliasMethodSymbol(string name, MethodSymbol underlying)
        : base(name, underlying)
    {
        _method = underlying;
    }

    public MethodKind MethodKind => _method.MethodKind;

    public TypeSymbol ReturnType => _method.ReturnType;

    public ImmutableArray<ParameterSymbol> Parameters => _method.Parameters;

    public MethodSymbol? OriginalDefinition => _method.OriginalDefinition;

    public bool IsAbstract => _method.IsAbstract;

    public bool IsAsync => _method.IsAsync;

    public bool IsCheckedBuiltin => _method.IsCheckedBuiltin;

    public bool IsDefinition => _method.IsDefinition;

    public bool IsExtensionMethod => _method.IsExtensionMethod;

    public bool IsExtern => _method.IsExtern;

    public bool IsUnsafe => _method.IsUnsafe;

    public bool IsGenericMethod => _method.IsGenericMethod;

    public bool IsOverride => _method.IsOverride;

    public bool IsReadOnly => _method.IsReadOnly;

    public bool IsFinal => _method.IsFinal;

    public bool IsVirtual => _method.IsVirtual;

    public bool IsIterator => _method.IsIterator;

    public IteratorMethodKind IteratorKind => _method.IteratorKind;

    public TypeSymbol? IteratorElementType => _method.IteratorElementType;

    public ImmutableArray<MethodSymbol> ExplicitInterfaceImplementations => _method.ExplicitInterfaceImplementations;

    public ImmutableArray<TypeParameterSymbol> TypeParameters => _method.TypeParameters;

    public ImmutableArray<TypeSymbol> TypeArguments => _method.TypeArguments;

    public MethodSymbol? ConstructedFrom => _method.ConstructedFrom;

    public bool SetsRequiredMembers => _method.SetsRequiredMembers;

    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => _method.GetReturnTypeAttributes();

    public MethodSymbol Construct(params TypeSymbol[] typeArguments) => _method.Construct(typeArguments);
}

internal sealed class AliasPropertySymbol : AliasSymbolImplementation, PropertySymbol
{
    private readonly PropertySymbol _property;

    public AliasPropertySymbol(string name, PropertySymbol underlying)
        : base(name, underlying)
    {
        _property = underlying;
    }

    public TypeSymbol Type => _property.Type;

    public MethodSymbol? GetMethod => _property.GetMethod;

    public MethodSymbol? SetMethod => _property.SetMethod;

    public bool IsIndexer => _property.IsIndexer;

    public bool IsRequired => _property.IsRequired;
}

internal sealed class AliasFieldSymbol : AliasSymbolImplementation, FieldSymbol
{
    private readonly FieldSymbol _field;

    public AliasFieldSymbol(string name, FieldSymbol underlying)
        : base(name, underlying)
    {
        _field = underlying;
    }

    public TypeSymbol Type => _field.Type;

    public bool IsConst => _field.IsConst;

    public bool IsReadOnly => _field.IsReadOnly;

    public bool IsRequired => _field.IsRequired;

    public object? GetConstantValue() => _field.GetConstantValue();
}

internal static class AliasSymbolFactory
{
    public static AliasSymbol Create(string name, Symbol underlying) => underlying switch
    {
        NamespaceSymbol n => new AliasNamespaceSymbol(name, n),
        NamedTypeSymbol t => new AliasNamedTypeSymbol(name, t),
        TypeSymbol t => new AliasTypeSymbol(name, t),
        MethodSymbol m => new AliasMethodSymbol(name, m),
        PropertySymbol p => new AliasPropertySymbol(name, p),
        FieldSymbol f => new AliasFieldSymbol(name, f),
        _ => throw new ArgumentException("Unsupported alias target", nameof(underlying))
    };
}
