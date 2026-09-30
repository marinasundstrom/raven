using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
internal sealed class ConstructedMethodSymbol : IMethodSymbol
{
    private readonly IMethodSymbol _definition;
    private readonly ImmutableArray<ITypeSymbol> _typeArguments;
    private readonly Dictionary<ITypeParameterSymbol, ITypeSymbol> _substitutionMap;
    private readonly ISymbol? _containingSymbol;
    private readonly INamedTypeSymbol? _containingType;
    private ImmutableArray<IParameterSymbol>? _parameters;
    private ImmutableArray<IMethodSymbol>? _explicitImpls;
    private ITypeSymbol? _returnType;

    public ConstructedMethodSymbol(
        IMethodSymbol definition,
        ImmutableArray<ITypeSymbol> typeArguments,
        INamedTypeSymbol? constructedContainingType = null)
    {
        _definition = definition ?? throw new ArgumentNullException(nameof(definition));
        _typeArguments = typeArguments.IsDefault
            ? []
            : typeArguments;

        _containingType = constructedContainingType ?? definition.ContainingType;
        _containingSymbol = constructedContainingType ?? definition.ContainingSymbol;

        var typeParameters = definition.TypeParameters;
        if (typeParameters.Length != typeArguments.Length)
            throw new ArgumentException($"Method '{definition.Name}' expects {typeParameters.Length} type arguments, but got {typeArguments.Length}.", nameof(typeArguments));

        _substitutionMap = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(
            typeParameters.Length,
            TypeParameterSubstitutionComparer.Instance);
        for (int i = 0; i < typeParameters.Length; i++)
        {
            var canonical = CanonicalizeTypeParameter(typeParameters[i]);
            _substitutionMap[canonical] = typeArguments[i];
        }

        // When the method is obtained from a constructed containing type, include the containing
        // type substitutions as well so nested return/parameter types can re-anchor correctly.
        TypeSubstitution.AddContainingTypeSubstitutions(_containingType, _substitutionMap);
    }

    public IMethodSymbol Definition => _definition;

    internal bool TryGetTypeSubstitution(ITypeParameterSymbol parameter, out ITypeSymbol substitution)
        => _substitutionMap.TryGetValue(parameter, out substitution!);

    public string Name => _definition.Name;
    public string MetadataName => _definition.MetadataName;
    public SymbolKind Kind => _definition.Kind;
    public bool IsImplicitlyDeclared => _definition.IsImplicitlyDeclared;
    public bool CanBeReferencedByName => _definition.CanBeReferencedByName;
    public bool IsAlias => _definition.IsAlias;
    public ISymbol UnderlyingSymbol => this;
    public Accessibility DeclaredAccessibility => _definition.DeclaredAccessibility;
    public bool IsStatic => _definition.IsStatic;
    public ISymbol? ContainingSymbol => _containingSymbol ?? _definition.ContainingSymbol;
    public INamedTypeSymbol? ContainingType => _containingType ?? _definition.ContainingType;
    public INamespaceSymbol? ContainingNamespace =>
        _containingType?.ContainingNamespace ?? _definition.ContainingNamespace;
    public IAssemblySymbol? ContainingAssembly =>
        _containingType?.ContainingAssembly ?? _definition.ContainingAssembly;
    public IModuleSymbol? ContainingModule =>
        _containingType?.ContainingModule ?? _definition.ContainingModule;
    public ISymbol? AssociatedSymbol => _definition.AssociatedSymbol;
    public ImmutableArray<Location> Locations => _definition.Locations;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _definition.DeclaringSyntaxReferences;

    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations
    {
        get
        {
            if (_explicitImpls.HasValue)
                return _explicitImpls.Value;

            var originals = _definition.ExplicitInterfaceImplementations;

            if (originals.IsDefaultOrEmpty || originals.Length == 0)
            {
                _explicitImpls = originals;
                return originals;
            }

            var builder = ImmutableArray.CreateBuilder<IMethodSymbol>(originals.Length);

            foreach (var orig in originals)
            {
                // If the interface method itself is generic, construct it with our method type args.
                // If it’s not generic, Construct(...) should just return the same symbol (or you can guard).
                IMethodSymbol constructedIfaceMethod =
                    orig.IsGenericMethod && _typeArguments.Length == orig.TypeParameters.Length
                        ? orig.Construct(_typeArguments.ToArray())
                        : orig;

                builder.Add(constructedIfaceMethod);
            }

            _explicitImpls = builder.ToImmutable();
            return _explicitImpls.Value;
        }
    }

    public ImmutableArray<AttributeData> GetAttributes() => _definition.GetAttributes();
    public DocumentationComment? GetDocumentationComment() => _definition.GetDocumentationComment();

    public ITypeSymbol ReturnType => _returnType ??= Substitute(_definition.ReturnType);

    public ImmutableArray<IParameterSymbol> Parameters =>
        _parameters ??= _definition.Parameters.Select(p => (IParameterSymbol)new ConstructedParameterSymbol(p, this)).ToImmutableArray();

    public bool IsConstructor => _definition.IsConstructor;

    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => _definition.GetReturnTypeAttributes();
    public override bool Equals(object? obj) => obj is ISymbol symbol && Equals(symbol);
    public override int GetHashCode() => SymbolEqualityComparer.Default.GetHashCode(this);

    public MethodKind MethodKind => _definition.MethodKind;
    public IMethodSymbol? OriginalDefinition => _definition.OriginalDefinition ?? _definition;
    public bool IsAbstract => _definition.IsAbstract;
    public bool IsAsync => _definition.IsAsync;
    public bool IsCheckedBuiltin => _definition.IsCheckedBuiltin;
    public bool IsDefinition => false;
    public bool IsExtensionMethod => _definition.IsExtensionMethod;
    public bool IsExtern => _definition.IsExtern;
    public bool IsUnsafe => _definition.IsUnsafe;
    public bool IsGenericMethod => _definition.IsGenericMethod;
    public bool IsOverride => _definition.IsOverride;
    public bool IsReadOnly => _definition.IsReadOnly;
    public bool IsFinal => _definition.IsFinal;
    public bool IsVirtual => _definition.IsVirtual;
    public bool IsIterator => _definition.IsIterator;
    public IteratorMethodKind IteratorKind => _definition.IteratorKind;
    public ITypeSymbol? IteratorElementType => _definition.IteratorElementType;
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _definition.TypeParameters;
    public ImmutableArray<ITypeSymbol> TypeArguments => _typeArguments;
    public IMethodSymbol? ConstructedFrom => _definition;

    public bool SetsRequiredMembers => _definition.SetsRequiredMembers;

    public void Accept(SymbolVisitor visitor) => visitor.VisitMethod(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitMethod(this);

    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer)
    {
        if (other is ConstructedMethodSymbol constructed)
        {
            if (!comparer.Equals(_definition, constructed._definition))
                return false;
            if (_typeArguments.Length != constructed._typeArguments.Length)
                return false;

            for (int i = 0; i < _typeArguments.Length; i++)
            {
                if (!comparer.Equals(_typeArguments[i], constructed._typeArguments[i]))
                    return false;
            }

            return true;
        }

        return _definition.Equals(other, comparer);
    }

    public IMethodSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        if (typeArguments is null)
            throw new ArgumentNullException(nameof(typeArguments));

        return new ConstructedMethodSymbol(_definition, typeArguments.ToImmutableArray(), _containingType);
    }

    // Helper methods for chain-aware substitution of nested types.
    private INamedTypeSymbol? SubstituteContainingType(INamedTypeSymbol? containing)
    {
        if (containing is null)
            return null;

        var substituted = Substitute(containing) as INamedTypeSymbol;
        return substituted ?? containing;
    }

    private bool TryGetContainingOverride(INamedTypeSymbol namedType, out INamedTypeSymbol? containingOverride)
    {
        containingOverride = null;

        if (namedType.ContainingType is not INamedTypeSymbol containing)
            return false;

        var substitutedContaining = SubstituteContainingType(containing);
        if (substitutedContaining is null)
            return false;

        if (!AreNamedTypesEquivalentShallow(substitutedContaining, containing))
        {
            containingOverride = substitutedContaining;
            return true;
        }

        if (_containingType is not null)
        {
            var containingDefinition = TypeSubstitution.GetDefinitionForSubstitution(containing);
            var methodContainingDefinition = TypeSubstitution.GetDefinitionForSubstitution(_containingType);
            if (ReferenceEquals(containingDefinition, methodContainingDefinition))
            {
                containingOverride = _containingType;
                return true;
            }
        }

        return false;
    }

    private static bool AreNamedTypesEquivalentShallow(INamedTypeSymbol left, INamedTypeSymbol right)
    {
        if (ReferenceEquals(left, right))
            return true;

        var leftDefinition = TypeSubstitution.GetDefinitionForSubstitution(left);
        var rightDefinition = TypeSubstitution.GetDefinitionForSubstitution(right);

        if (!ReferenceEquals(leftDefinition, rightDefinition))
            return false;

        if (left.Arity != right.Arity)
            return false;

        var leftArgs = TypeSubstitution.GetShallowTypeArguments(left);
        var rightArgs = TypeSubstitution.GetShallowTypeArguments(right);

        if (leftArgs.IsDefaultOrEmpty || rightArgs.IsDefaultOrEmpty)
            return leftArgs.Length == rightArgs.Length;

        if (leftArgs.Length != rightArgs.Length)
            return false;

        for (var i = 0; i < leftArgs.Length; i++)
        {
            if (!ReferenceEquals(leftArgs[i], rightArgs[i]))
                return false;
        }

        return true;
    }

    private ITypeSymbol Substitute(ITypeSymbol type)
    {
        var visiting = new HashSet<ITypeSymbol>(ReferenceEqualityComparer.Instance);
        var cache = new Dictionary<ITypeSymbol, SubstitutionResult>(ReferenceEqualityComparer.Instance);
        return Substitute(type, visiting, cache).Type;
    }

    private SubstitutionResult Substitute(
        ITypeSymbol type,
        HashSet<ITypeSymbol> visiting,
        Dictionary<ITypeSymbol, SubstitutionResult> cache)
    {
        if (cache.TryGetValue(type, out var cached))
            return cached;

        if (!visiting.Add(type))
            return new SubstitutionResult(type, Changed: false);

        try
        {
            if (type is ITypeParameterSymbol tp)
            {
                tp = CanonicalizeTypeParameter(tp);
                if (_substitutionMap.TryGetValue(tp, out var replacement))
                {
                    return cache[type] = new SubstitutionResult(
                        replacement,
                        Changed: !ReferenceEquals(replacement, type));
                }
            }

            if (type is IIntersectionTypeSymbol intersection)
            {
                var result = TypeSubstitution.SubstituteIntersection(intersection,
                    constituent => Substitute(constituent, visiting, cache).Type);
                return cache[type] = new SubstitutionResult(result, Changed: !ReferenceEquals(result, type));
            }

            if (type is NullableTypeSymbol nullableTypeSymbol)
            {
                var underlying = Substitute(nullableTypeSymbol.UnderlyingType, visiting, cache);

                if (underlying.Changed)
                {
                    var underlyingType = underlying.Type;
                    var result = underlyingType.ApplySubstitutedNullability(nullableTypeSymbol);
                    return cache[type] = new SubstitutionResult(result, Changed: true);
                }

                return cache[type] = new SubstitutionResult(type, Changed: false);
            }

            if (type is RefTypeSymbol refType)
            {
                var element = Substitute(refType.ElementType, visiting, cache);

                if (element.Changed)
                {
                    var result = new RefTypeSymbol(element.Type);
                    return cache[type] = new SubstitutionResult(result, Changed: true);
                }

                return cache[type] = new SubstitutionResult(type, Changed: false);
            }

            if (type is IAddressTypeSymbol address)
            {
                var element = Substitute(address.ReferencedType, visiting, cache);

                if (element.Changed)
                {
                    var result = new AddressTypeSymbol(element.Type);
                    return cache[type] = new SubstitutionResult(result, Changed: true);
                }

                return cache[type] = new SubstitutionResult(type, Changed: false);
            }

            if (type is IPointerTypeSymbol pointerType)
            {
                var element = Substitute(pointerType.PointedAtType, visiting, cache);
                return cache[type] = element.Changed
                    ? new SubstitutionResult(new PointerTypeSymbol(element.Type), Changed: true)
                    : new SubstitutionResult(type, Changed: false);
            }

            if (type is IArrayTypeSymbol arrayType)
            {
                var element = Substitute(arrayType.ElementType, visiting, cache);

                if (element.Changed)
                {
                    var result = new ArrayTypeSymbol(arrayType.BaseType, element.Type, arrayType.ContainingSymbol, arrayType.ContainingType, arrayType.ContainingNamespace, [], arrayType.Rank, arrayType.FixedLength);
                    return cache[type] = new SubstitutionResult(result, Changed: true);
                }

                return cache[type] = new SubstitutionResult(type, Changed: false);
            }

            if (type is ITupleTypeSymbol tupleType)
            {
                var result = TypeSubstitution.SubstituteTupleElements(
                    tupleType,
                    element => Substitute(element, visiting, cache).Type);
                return cache[type] = new SubstitutionResult(
                    result,
                    Changed: !ReferenceEquals(result, type));
            }

            if (type is INamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
            {
                var typeArguments = TypeSubstitution.GetShallowTypeArguments(named);
                var substitutedArgs = new ITypeSymbol[typeArguments.Length];
                var changed = false;

                for (int i = 0; i < typeArguments.Length; i++)
                {
                    var originalArg = typeArguments[i];
                    var substitution = Substitute(originalArg, visiting, cache);
                    var substitutedArg = substitution.Type;

                    substitutedArgs[i] = substitutedArg;

                    if (substitution.Changed)
                        changed = true;
                }

                if ((named.ConstructedFrom ?? named).SpecialType == SpecialType.System_Nullable_T &&
                    substitutedArgs.Length == 1)
                {
                    var underlyingType = substitutedArgs[0];
                    var result = underlyingType.IsNullable
                        ? underlyingType
                        : underlyingType.GetNullableType();
                    return cache[type] = new SubstitutionResult(result, Changed: true);
                }

                if (!changed)
                {
                    // Even if type arguments did not change, nested types may need re-anchoring
                    // under a substituted containing type.
                    if (named.ContainingType is INamedTypeSymbol && TryGetContainingOverride(named, out var containingOverride) && containingOverride is not null)
                    {
                        var constructedFromSame = TypeSubstitution.GetDefinitionForSubstitution(named);
                        var reanchored = TypeSubstitution.ReanchorNested(
                            constructedFromSame,
                            containingOverride,
                            inheritedSubstitution: null,
                            typeArguments: typeArguments);
                        return cache[type] = new SubstitutionResult(
                            reanchored,
                            Changed: !ReferenceEquals(reanchored, type));
                    }

                    return cache[type] = new SubstitutionResult(named, Changed: false);
                }

                // Avoid reusing a possibly already-constructed named
                var constructedFrom = TypeSubstitution.GetDefinitionForSubstitution(named);

                if (named.ContainingType is INamedTypeSymbol namedContaining)
                {
                    var immutableArguments = ImmutableArray.Create(substitutedArgs);
                    var containingForNested = namedContaining;
                    if (TryGetContainingOverride(named, out var overrideContaining) && overrideContaining is not null)
                        containingForNested = overrideContaining;

                    var reanchored = TypeSubstitution.ReanchorNested(
                        constructedFrom,
                        containingForNested,
                        inheritedSubstitution: null,
                        typeArguments: immutableArguments);
                    return cache[type] = new SubstitutionResult(reanchored, Changed: true);
                }

                var constructedResult = constructedFrom.Construct(substitutedArgs);
                return cache[type] = new SubstitutionResult(constructedResult, Changed: true);
            }

            // Nested non-generic named types (e.g. Result<T,E>.Ok) must still be re-anchored under a substituted containing type.
            if (type is INamedTypeSymbol nestedNamed && nestedNamed.ContainingType is INamedTypeSymbol)
            {
                if (TryGetContainingOverride(nestedNamed, out var containingOverride) && containingOverride is not null)
                {
                    var reanchored = TypeSubstitution.ReanchorNested(
                        nestedNamed,
                        containingOverride,
                        inheritedSubstitution: null,
                        typeArguments: ImmutableArray<ITypeSymbol>.Empty);
                    return cache[type] = new SubstitutionResult(
                        reanchored,
                        Changed: !ReferenceEquals(reanchored, type));
                }
            }

            return cache[type] = new SubstitutionResult(type, Changed: false);
        }
        finally
        {
            visiting.Remove(type);
        }
    }

    private readonly record struct SubstitutionResult(ITypeSymbol Type, bool Changed);

    private ITypeParameterSymbol CanonicalizeTypeParameter(ITypeParameterSymbol typeParameter)
    {
        if (typeParameter.OwnerKind == TypeParameterOwnerKind.Method &&
            typeParameter.ContainingSymbol is IMethodSymbol containingMethod &&
            SymbolEqualityComparer.Default.Equals(
                containingMethod.OriginalDefinition ?? containingMethod,
                _definition.OriginalDefinition ?? _definition) &&
            typeParameter.Ordinal >= 0 &&
            typeParameter.Ordinal < _definition.TypeParameters.Length)
        {
            return _definition.TypeParameters[typeParameter.Ordinal];
        }

        if (typeParameter.OwnerKind == TypeParameterOwnerKind.Type)
            return (ITypeParameterSymbol)(typeParameter.OriginalDefinition ?? typeParameter);

        return (ITypeParameterSymbol)(typeParameter.OriginalDefinition ?? typeParameter);
    }

    [DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
    private sealed class ConstructedParameterSymbol : IParameterSymbol, IParameterDefaultValueInfo
    {
        private readonly IParameterSymbol _original;
        private readonly ConstructedMethodSymbol _owner;
        private ITypeSymbol? _type;

        public ConstructedParameterSymbol(IParameterSymbol original, ConstructedMethodSymbol owner)
        {
            _original = original;
            _owner = owner;
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
        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
        public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
        public bool IsStatic => false;
        public bool IsAlias => _original.IsAlias;
        public ISymbol UnderlyingSymbol => this;
        public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
        public ITypeSymbol Type => _type ??= _owner.Substitute(_original.Type);
        public bool HasImplicitName => _original.HasImplicitName;

        public Syntax.PatternSyntax? BindingPattern => _original.BindingPattern;
        public bool IsVarParams => _original.IsVarParams;
        public RefKind RefKind => _original.RefKind;
        public ScopedKind ScopedKind => _original.ScopedKind;
        public bool IsMutable => _original.IsMutable;
        public bool HasExplicitDefaultValue => _original.HasExplicitDefaultValue;
        public object? ExplicitDefaultValue => _original.ExplicitDefaultValue;
        bool IParameterDefaultValueInfo.ExplicitDefaultValueIsTypeDefault
            => _original is IParameterDefaultValueInfo { ExplicitDefaultValueIsTypeDefault: true };

        public void Accept(SymbolVisitor visitor) => visitor.VisitParameter(this);
        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitParameter(this);
        public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
        public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);

        public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();

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
