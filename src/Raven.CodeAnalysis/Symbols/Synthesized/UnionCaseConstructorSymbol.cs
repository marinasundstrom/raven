using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class UnionCaseConstructorSymbol : IMethodSymbol
{
    private readonly IMethodSymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructedReceiver;
    private ImmutableArray<IParameterSymbol>? _parameters;
    private ITypeSymbol? _returnType;

    public UnionCaseConstructorSymbol(IMethodSymbol original, ConstructedNamedTypeSymbol constructedReceiver)
    {
        _original = original ?? throw new ArgumentNullException(nameof(original));
        _constructedReceiver = constructedReceiver ?? throw new ArgumentNullException(nameof(constructedReceiver));
    }

    internal ConstructedNamedTypeSymbol ConstructedReceiver => _constructedReceiver;
    internal IMethodSymbol Original => _original;

    public string Name => _original.Name;
    public string MetadataName => _original.MetadataName;
    public SymbolKind Kind => _original.Kind;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool CanBeReferencedByName => _original.CanBeReferencedByName;
    public bool IsAlias => false;
    public ISymbol? UnderlyingSymbol => _original;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public bool IsStatic => _original.IsStatic;
    public ISymbol? ContainingSymbol => _original.ContainingSymbol;
    public INamedTypeSymbol? ContainingType => _original.ContainingType;
    public INamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public IAssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _original.ContainingModule;
    public ImmutableArray<Location> Locations => _original.Locations;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;

    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => _original.GetReturnTypeAttributes();

    public ITypeSymbol ReturnType => _returnType ??= _constructedReceiver.Substitute(_original.ReturnType);

    public ImmutableArray<IParameterSymbol> Parameters =>
        _parameters ??= _original.Parameters.Select(p => (IParameterSymbol)new UnionCaseConstructorParameterSymbol(p, this)).ToImmutableArray();

    public bool IsConstructor => _original.IsConstructor;
    public bool IsNamedConstructor => _original.IsNamedConstructor;

    public MethodKind MethodKind => _original.MethodKind;
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
    public bool IsIterator => _original.IsIterator;
    public IteratorMethodKind IteratorKind => _original.IteratorKind;
    public ITypeSymbol? IteratorElementType => _original.IteratorElementType;
    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => _original.ExplicitInterfaceImplementations;
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _original.TypeParameters;
    public ImmutableArray<ITypeSymbol> TypeArguments => _original.TypeArguments;
    public IMethodSymbol? ConstructedFrom => _original.ConstructedFrom ?? _original;
    public IMethodSymbol OriginalDefinition => _original.OriginalDefinition ?? _original;

    public void Accept(SymbolVisitor visitor) => visitor.VisitMethod(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitMethod(this);

    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);

    public override bool Equals(object? obj) => _original.Equals(obj);
    public override int GetHashCode() => _original.GetHashCode();

    public IMethodSymbol Construct(params ITypeSymbol[] typeArguments) => _original.Construct(typeArguments);

    internal bool TryGetSubstitutedConstraintTypes(
        ITypeParameterSymbol typeParameter,
        out ImmutableArray<ITypeSymbol> substituted)
    {
        substituted = default;
        if (!_original.TypeParameters.Contains(typeParameter))
            return false;

        var originalConstraints = typeParameter.ConstraintTypes;
        if (originalConstraints.IsDefaultOrEmpty || originalConstraints.Length == 0)
        {
            substituted = originalConstraints;
            return true;
        }

        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(originalConstraints.Length);
        foreach (var constraint in originalConstraints)
            builder.Add(_constructedReceiver.Substitute(constraint));

        substituted = builder.MoveToImmutable();
        return true;
    }

    private sealed class UnionCaseConstructorParameterSymbol : IParameterSymbol
    {
        private readonly IParameterSymbol _original;
        private readonly UnionCaseConstructorSymbol _owner;
        private ITypeSymbol? _type;

        public UnionCaseConstructorParameterSymbol(IParameterSymbol original, UnionCaseConstructorSymbol owner)
        {
            _original = original ?? throw new ArgumentNullException(nameof(original));
            _owner = owner ?? throw new ArgumentNullException(nameof(owner));
        }

        public string Name => _original.Name;
        public SymbolKind Kind => _original.Kind;
        public string MetadataName => _original.MetadataName;
        public ISymbol? ContainingSymbol => _owner;
        public IAssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
        public IModuleSymbol? ContainingModule => _original.ContainingModule;
        public INamedTypeSymbol? ContainingType => _original.ContainingType;
        public INamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
        public ImmutableArray<Location> Locations => _original.Locations;
        public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
        public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
        public bool IsStatic => false;
        public ISymbol UnderlyingSymbol => _original;
        public bool IsAlias => false;
        public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
        public bool IsParams => _original.IsParams;
        public RefKind RefKind => _original.RefKind;
        public bool IsMutable => _original.IsMutable;
        public bool HasExplicitDefaultValue => _original.HasExplicitDefaultValue;
        public object? ExplicitDefaultValue => _original.ExplicitDefaultValue;
        public ITypeSymbol Type => _type ??= _owner._constructedReceiver.Substitute(_original.Type);

        public void Accept(SymbolVisitor visitor) => visitor.VisitParameter(this);
        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitParameter(this);
        public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
        public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
        public override bool Equals(object? obj) => _original.Equals(obj);
        public override int GetHashCode() => _original.GetHashCode();
    }
}
