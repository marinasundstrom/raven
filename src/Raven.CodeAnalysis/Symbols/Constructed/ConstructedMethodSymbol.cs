using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class ConstructedMethodSymbol : IMethodSymbol
{
    private readonly IMethodSymbol _definition;
    private readonly ImmutableArray<ITypeSymbol> _typeArguments;
    private readonly Dictionary<ITypeParameterSymbol, ITypeSymbol> _substitutionMap;
    private ImmutableArray<IParameterSymbol>? _parameters;
    private ITypeSymbol? _returnType;

    public ConstructedMethodSymbol(IMethodSymbol definition, ImmutableArray<ITypeSymbol> typeArguments)
    {
        _definition = definition ?? throw new ArgumentNullException(nameof(definition));
        _typeArguments = typeArguments;

        var typeParameters = definition.TypeParameters;
        if (typeParameters.Length != typeArguments.Length)
            throw new ArgumentException($"Method '{definition.Name}' expects {typeParameters.Length} type arguments, but got {typeArguments.Length}.", nameof(typeArguments));

        _substitutionMap = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(typeParameters.Length, SymbolEqualityComparer.Default);
        for (int i = 0; i < typeParameters.Length; i++)
            _substitutionMap[typeParameters[i]] = typeArguments[i];
    }

    public IMethodSymbol Definition => _definition;

    public string Name => _definition.Name;
    public string MetadataName => _definition.MetadataName;
    public SymbolKind Kind => _definition.Kind;
    public bool IsImplicitlyDeclared => _definition.IsImplicitlyDeclared;
    public bool CanBeReferencedByName => _definition.CanBeReferencedByName;
    public bool IsAlias => _definition.IsAlias;
    public ISymbol? UnderlyingSymbol => this;
    public Accessibility DeclaredAccessibility => _definition.DeclaredAccessibility;
    public bool IsStatic => _definition.IsStatic;
    public ISymbol? ContainingSymbol => _definition.ContainingSymbol;
    public INamedTypeSymbol? ContainingType => _definition.ContainingType;
    public INamespaceSymbol? ContainingNamespace => _definition.ContainingNamespace;
    public IAssemblySymbol? ContainingAssembly => _definition.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _definition.ContainingModule;
    public ImmutableArray<Location> Locations => _definition.Locations;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _definition.DeclaringSyntaxReferences;

    public ImmutableArray<AttributeData> GetAttributes() => _definition.GetAttributes();

    public ITypeSymbol ReturnType => _returnType ??= Substitute(_definition.ReturnType);

    public ImmutableArray<IParameterSymbol> Parameters =>
        _parameters ??= _definition.Parameters.Select(p => (IParameterSymbol)new ConstructedParameterSymbol(p, this)).ToImmutableArray();

    public bool IsConstructor => _definition.IsConstructor;
    public bool IsNamedConstructor => _definition.IsNamedConstructor;

    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => _definition.GetReturnTypeAttributes();
    public override bool Equals(object? obj) => _definition.Equals(obj);
    public override int GetHashCode() => _definition.GetHashCode();

    public MethodKind MethodKind => _definition.MethodKind;
    public IMethodSymbol? OriginalDefinition => _definition.OriginalDefinition ?? _definition;
    public bool IsAbstract => _definition.IsAbstract;
    public bool IsAsync => _definition.IsAsync;
    public bool IsCheckedBuiltin => _definition.IsCheckedBuiltin;
    public bool IsDefinition => false;
    public bool IsExtensionMethod => _definition.IsExtensionMethod;
    public bool IsExtern => _definition.IsExtern;
    public bool IsGenericMethod => _definition.IsGenericMethod;
    public bool IsOverride => _definition.IsOverride;
    public bool IsReadOnly => _definition.IsReadOnly;
    public bool IsSealed => _definition.IsSealed;
    public bool IsVirtual => _definition.IsVirtual;
    public bool IsIterator => _definition.IsIterator;
    public IteratorMethodKind IteratorKind => _definition.IteratorKind;
    public ITypeSymbol? IteratorElementType => _definition.IteratorElementType;
    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => _definition.ExplicitInterfaceImplementations;
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _definition.TypeParameters;
    public ImmutableArray<ITypeSymbol> TypeArguments => _typeArguments;
    public IMethodSymbol? ConstructedFrom => _definition;

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

        return new ConstructedMethodSymbol(_definition, typeArguments.ToImmutableArray());
    }

    private ITypeSymbol Substitute(ITypeSymbol type)
    {
        if (type is ITypeParameterSymbol tp && _substitutionMap.TryGetValue(tp, out var replacement))
            return replacement;

        if (type is ByRefTypeSymbol byRef)
        {
            var substitutedElement = Substitute(byRef.ElementType);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, byRef.ElementType))
                return new ByRefTypeSymbol(substitutedElement, byRef.RefKind);

            return type;
        }

        if (type is INamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
        {
            var substitutedArgs = named.TypeArguments.Select(Substitute).ToArray();
            bool changed = false;
            for (int i = 0; i < substitutedArgs.Length; i++)
            {
                if (!SymbolEqualityComparer.Default.Equals(substitutedArgs[i], named.TypeArguments[i]))
                {
                    changed = true;
                    break;
                }
            }

            if (changed)
                return named.Construct(substitutedArgs);
        }

        return type;
    }

    private sealed class ConstructedParameterSymbol : IParameterSymbol
    {
        private readonly IParameterSymbol _original;
        private readonly ConstructedMethodSymbol _owner;

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
        public ITypeSymbol Type => _owner.Substitute(_original.Type);
        public bool IsParams => _original.IsParams;
        public RefKind RefKind => _original.RefKind;
        public bool HasExplicitDefaultValue => _original.HasExplicitDefaultValue;
        public object? ExplicitDefaultValue => _original.ExplicitDefaultValue;

        public void Accept(SymbolVisitor visitor) => visitor.VisitParameter(this);
        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitParameter(this);
        public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
        public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);

        public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
    }

    internal MethodInfo GetMethodInfo(CodeGen.CodeGenerator codeGen)
    {
        if (codeGen is null)
            throw new ArgumentNullException(nameof(codeGen));

        var containingType = _definition.ContainingType
            ?? throw new InvalidOperationException("Constructed method is missing a containing type.");

        var containingClrType = containingType.GetClrTypeTreatingUnitAsVoid(codeGen);
        var runtimeTypeArguments = TypeArguments
            .Select(argument => GetProjectedRuntimeType(argument, codeGen, treatUnitAsVoid: false))
            .ToArray();

        RegisterMethodTypeParameters(runtimeTypeArguments, codeGen);

        if (containingClrType is TypeBuilder typeBuilder)
        {
            var builderMethod = ResolveMethodOnTypeBuilder(typeBuilder, codeGen);
            return InstantiateGenericMethod(builderMethod, runtimeTypeArguments);
        }

        var isTypeBuilderInstantiation = string.Equals(
            containingClrType.GetType().FullName,
            "System.Reflection.Emit.TypeBuilderInstantiation",
            StringComparison.Ordinal);

        if (isTypeBuilderInstantiation)
        {
            var instantiatedMethod = ResolveMethodOnTypeBuilderInstantiation(containingClrType, codeGen);
            if (instantiatedMethod is not null)
                return InstantiateGenericMethod(instantiatedMethod, runtimeTypeArguments);
        }

        var methodSearchType = isTypeBuilderInstantiation
            ? containingClrType.GetGenericTypeDefinition()
            : containingClrType;
        var parameterSymbols = Parameters;
        var returnTypeSymbol = ReturnType;

        const BindingFlags Flags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;
        foreach (var method in methodSearchType.GetMethods(Flags))
        {
            MethodInfo candidate = method;

            if (isTypeBuilderInstantiation)
            {
                var instantiated = TypeBuilder.GetMethod(containingClrType, method);
                if (instantiated is null)
                    continue;

                candidate = instantiated;
            }

            if (!string.Equals(candidate.Name, _definition.Name, StringComparison.Ordinal))
                continue;

            if (candidate.IsGenericMethodDefinition != _definition.IsGenericMethod)
            {
                if (!candidate.IsGenericMethodDefinition)
                    continue;
            }

            if (candidate.IsGenericMethodDefinition)
            {
                if (candidate.GetGenericArguments().Length != runtimeTypeArguments.Length)
                    continue;
                candidate = candidate.MakeGenericMethod(runtimeTypeArguments);
            }
            else if (candidate.ContainsGenericParameters)
            {
                continue;
            }

            var candidateParameters = candidate.GetParameters();
            var methodRuntimeArguments = candidate.IsGenericMethod
                ? candidate.GetGenericArguments()
                : Array.Empty<Type>();
            var typeRuntimeArguments = candidate.DeclaringType is not null && candidate.DeclaringType.IsGenericType
                ? candidate.DeclaringType.GetGenericArguments()
                : Array.Empty<Type>();

            if (!ParametersMatch(candidateParameters, parameterSymbols, methodRuntimeArguments, typeRuntimeArguments, codeGen))
                continue;

            var normalizedReturnType = SubstituteRuntimeType(candidate.ReturnType, methodRuntimeArguments, typeRuntimeArguments);
            if (!MethodSymbolExtensionsForCodeGen.ReturnTypesMatch(normalizedReturnType, returnTypeSymbol, codeGen))
                continue;

            return candidate;
        }

        throw new InvalidOperationException($"Unable to resolve constructed method '{_definition.Name}'.");
    }

    private MethodInfo ResolveMethodOnTypeBuilder(TypeBuilder containingTypeBuilder, CodeGen.CodeGenerator codeGen)
    {
        var definitionInfo = _definition.GetClrMethodInfo(codeGen);

        if (ReferenceEquals(definitionInfo.DeclaringType, containingTypeBuilder))
            return definitionInfo;

        var resolved = TypeBuilder.GetMethod(containingTypeBuilder, definitionInfo);
        if (resolved is not null)
            return resolved;

        throw new InvalidOperationException($"Unable to map method '{_definition.Name}' onto type builder '{containingTypeBuilder}'.");
    }

    private MethodInfo? ResolveMethodOnTypeBuilderInstantiation(Type containingClrType, CodeGen.CodeGenerator codeGen)
    {
        if (containingClrType.GetGenericTypeDefinition() is TypeBuilder genericDefinition)
        {
            var definitionMethod = ResolveMethodOnTypeBuilder(genericDefinition, codeGen);
            var instantiated = TypeBuilder.GetMethod(containingClrType, definitionMethod);
            if (instantiated is not null)
                return instantiated;
        }

        var definitionInfo = _definition.GetClrMethodInfo(codeGen);
        return TypeBuilder.GetMethod(containingClrType, definitionInfo);
    }

    private static MethodInfo InstantiateGenericMethod(MethodInfo method, Type[] runtimeTypeArguments)
    {
        if (runtimeTypeArguments.Length == 0)
            return method;

        if (method.IsGenericMethodDefinition)
            return method.MakeGenericMethod(runtimeTypeArguments);

        if (method.ContainsGenericParameters)
        {
            var definition = method.IsGenericMethod ? method.GetGenericMethodDefinition() : method;
            if (definition.IsGenericMethodDefinition)
                return definition.MakeGenericMethod(runtimeTypeArguments);
        }

        return method;
    }

    private bool ParametersMatch(
        ParameterInfo[] runtimeParameters,
        ImmutableArray<IParameterSymbol> parameterSymbols,
        Type[] methodRuntimeArguments,
        Type[]? typeRuntimeArguments,
        CodeGen.CodeGenerator codeGen)
    {
        if (runtimeParameters.Length != parameterSymbols.Length)
            return false;

        for (var i = 0; i < runtimeParameters.Length; i++)
        {
            if (!ParameterMatches(runtimeParameters[i], parameterSymbols[i], methodRuntimeArguments, typeRuntimeArguments, codeGen))
                return false;
        }

        return true;
    }

    private bool ParameterMatches(
        ParameterInfo runtimeParameter,
        IParameterSymbol symbolParameter,
        Type[] methodRuntimeArguments,
        Type[]? typeRuntimeArguments,
        CodeGen.CodeGenerator codeGen)
    {
        if (symbolParameter.RefKind == RefKind.Out && !runtimeParameter.IsOut)
            return false;

        if (symbolParameter.RefKind == RefKind.Ref && !runtimeParameter.ParameterType.IsByRef)
            return false;

        if (symbolParameter.RefKind == RefKind.In && !(runtimeParameter.IsIn || runtimeParameter.ParameterType.IsByRef))
            return false;

        var normalizedRuntimeType = SubstituteRuntimeType(runtimeParameter.ParameterType, methodRuntimeArguments, typeRuntimeArguments);
        return MethodSymbolExtensionsForCodeGen.TypesEquivalent(normalizedRuntimeType, symbolParameter.Type, codeGen);
    }

    private static Type SubstituteRuntimeType(Type runtimeType, Type[] methodRuntimeArguments, Type[]? typeRuntimeArguments)
    {
        if (runtimeType.IsByRef)
        {
            var element = SubstituteRuntimeType(runtimeType.GetElementType()!, methodRuntimeArguments, typeRuntimeArguments);
            return element.MakeByRefType();
        }

        if (runtimeType.IsPointer)
        {
            var element = SubstituteRuntimeType(runtimeType.GetElementType()!, methodRuntimeArguments, typeRuntimeArguments);
            return element.MakePointerType();
        }

        if (runtimeType.IsArray)
        {
            var element = SubstituteRuntimeType(runtimeType.GetElementType()!, methodRuntimeArguments, typeRuntimeArguments);
            return runtimeType.GetArrayRank() == 1
                ? element.MakeArrayType()
                : element.MakeArrayType(runtimeType.GetArrayRank());
        }

        if (runtimeType.IsGenericParameter)
        {
            if (runtimeType.DeclaringMethod is not null)
            {
                var position = runtimeType.GenericParameterPosition;
                if (position >= 0 && position < methodRuntimeArguments.Length)
                    return methodRuntimeArguments[position];
            }
            else if (runtimeType.DeclaringType is not null && typeRuntimeArguments is { Length: > 0 })
            {
                var mapped = SubstituteTypeParameterFromDeclaringType(runtimeType, runtimeType.DeclaringType, typeRuntimeArguments, methodRuntimeArguments);
                if (mapped is not null)
                    return mapped;
            }

            return runtimeType;
        }

        if (runtimeType.IsGenericType)
        {
            var substitutedArguments = runtimeType.GetGenericArguments()
                .Select(argument => SubstituteRuntimeType(argument, methodRuntimeArguments, typeRuntimeArguments))
                .ToArray();

            var definition = runtimeType.IsGenericTypeDefinition
                ? runtimeType
                : runtimeType.GetGenericTypeDefinition();

            return MakeGenericTypePreservingBuilders(definition, substitutedArguments);
        }

        return runtimeType;
    }

    private void RegisterMethodTypeParameters(Type[] runtimeTypeArguments, CodeGen.CodeGenerator codeGen)
    {
        if (runtimeTypeArguments.Length == 0)
            return;

        var typeParameters = _definition.TypeParameters;
        var length = Math.Min(typeParameters.Length, runtimeTypeArguments.Length);

        for (var i = 0; i < length; i++)
        {
            var runtimeType = runtimeTypeArguments[i];
            if (runtimeType is not null)
                codeGen.CacheRuntimeTypeParameter(typeParameters[i], runtimeType);
        }
    }

    private static Type? SubstituteTypeParameterFromDeclaringType(Type genericParameter, Type declaringType, Type[] typeRuntimeArguments, Type[] methodRuntimeArguments)
    {
        var definition = declaringType.IsGenericTypeDefinition
            ? declaringType
            : declaringType.GetGenericTypeDefinition();

        var definitionParameters = definition.GetGenericArguments();
        var index = Array.IndexOf(definitionParameters, genericParameter);
        if (index >= 0 && index < typeRuntimeArguments.Length)
        {
            var mapped = typeRuntimeArguments[index];
            if (!ReferenceEquals(mapped, genericParameter))
                return SubstituteRuntimeType(mapped, methodRuntimeArguments, typeRuntimeArguments);
        }

        // Some runtimes reuse declaring-type parameters for nested generic parameter builders where GenericParameterPosition
        // refers to the overall argument list instead of the declaring definition. Fall back to positional lookup if available.
        var position = genericParameter.GenericParameterPosition;
        if (position >= 0 && position < typeRuntimeArguments.Length)
        {
            var mapped = typeRuntimeArguments[position];
            if (!ReferenceEquals(mapped, genericParameter))
                return SubstituteRuntimeType(mapped, methodRuntimeArguments, typeRuntimeArguments);
        }

        return null;
    }

    private static Type MakeGenericTypePreservingBuilders(Type definition, Type[] arguments)
        => definition.MakeGenericType(arguments);

    private Type GetProjectedRuntimeType(
        ITypeSymbol symbol,
        CodeGen.CodeGenerator codeGen,
        bool treatUnitAsVoid,
        bool isTopLevel = true)
    {
        if (symbol is null)
            throw new ArgumentNullException(nameof(symbol));
        if (codeGen is null)
            throw new ArgumentNullException(nameof(codeGen));

        if (symbol is ITypeParameterSymbol typeParameter)
        {
            if (_substitutionMap.TryGetValue(typeParameter, out var substitution))
                return GetProjectedRuntimeType(substitution, codeGen, treatUnitAsVoid, isTopLevel);

            if (codeGen.TryGetRuntimeTypeForTypeParameter(typeParameter, out var runtimeType))
                return runtimeType;

            throw new InvalidOperationException($"Unable to resolve runtime type for type parameter '{typeParameter.Name}'.");
        }

        if (symbol is ByRefTypeSymbol byRef)
        {
            var elementType = GetProjectedRuntimeType(byRef.ElementType, codeGen, treatUnitAsVoid, isTopLevel: false);
            return elementType.MakeByRefType();
        }

        if (symbol is IArrayTypeSymbol array)
        {
            var elementType = GetProjectedRuntimeType(array.ElementType, codeGen, treatUnitAsVoid, isTopLevel: false);
            return array.Rank == 1
                ? elementType.MakeArrayType()
                : elementType.MakeArrayType(array.Rank);
        }

        if (symbol is NullableTypeSymbol nullable)
        {
            var underlying = GetProjectedRuntimeType(nullable.UnderlyingType, codeGen, treatUnitAsVoid, isTopLevel: false);
            return typeof(Nullable<>).MakeGenericType(underlying);
        }

        if (symbol is LiteralTypeSymbol literal)
            return GetProjectedRuntimeType(literal.UnderlyingType, codeGen, treatUnitAsVoid, isTopLevel: false);

        if (symbol is ITupleTypeSymbol tuple)
        {
            return GetProjectedRuntimeType(tuple.UnderlyingTupleType, codeGen, treatUnitAsVoid, isTopLevel: false);
        }

        if (symbol is INamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
        {
            var definition = named.ConstructedFrom as INamedTypeSymbol;
            Type runtimeDefinition;

            if (definition is not null && !SymbolEqualityComparer.Default.Equals(definition, named))
            {
                runtimeDefinition = GetProjectedRuntimeType(definition, codeGen, treatUnitAsVoid, isTopLevel: false);
            }
            else
            {
                runtimeDefinition = treatUnitAsVoid && isTopLevel
                    ? named.GetClrTypeTreatingUnitAsVoid(codeGen)
                    : named.GetClrType(codeGen);
            }

            if (!runtimeDefinition.IsGenericTypeDefinition && !runtimeDefinition.ContainsGenericParameters)
                return runtimeDefinition;

            var runtimeArguments = named.TypeArguments
                .Select(argument => GetProjectedRuntimeType(argument, codeGen, treatUnitAsVoid, isTopLevel: false))
                .ToArray();

            return runtimeDefinition.MakeGenericType(runtimeArguments);
        }

        return treatUnitAsVoid && isTopLevel
            ? symbol.GetClrTypeTreatingUnitAsVoid(codeGen)
            : symbol.GetClrType(codeGen);
    }
}
