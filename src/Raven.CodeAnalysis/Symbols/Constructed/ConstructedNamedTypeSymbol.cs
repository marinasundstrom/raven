using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.CodeGen;

namespace Raven.CodeAnalysis.Symbols;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
internal sealed class ConstructedNamedTypeSymbol : INamedTypeSymbol
{
    private readonly INamedTypeSymbol _originalDefinition;
    private readonly Dictionary<ITypeParameterSymbol, ITypeSymbol> _substitutionMap;
    private readonly INamedTypeSymbol? _containingTypeOverride;
    private ImmutableArray<ISymbol>? _members;
    private ImmutableArray<IFieldSymbol>? _tupleElements;
    private ImmutableArray<INamedTypeSymbol>? _interfaces;
    private ImmutableArray<INamedTypeSymbol>? _allInterfaces;

    public ImmutableArray<ITypeSymbol> TypeArguments { get; }

    public ConstructedNamedTypeSymbol(INamedTypeSymbol originalDefinition, ImmutableArray<ITypeSymbol> typeArguments)
        : this(originalDefinition, typeArguments, inheritedSubstitution: null, containingTypeOverride: null)
    {
    }

    private static Dictionary<ITypeParameterSymbol, ITypeSymbol> CreateSubstitutionMap(
        INamedTypeSymbol originalDefinition,
        ImmutableArray<ITypeSymbol> typeArguments,
        Dictionary<ITypeParameterSymbol, ITypeSymbol>? inheritedSubstitution)
    {
        if (inheritedSubstitution is null)
        {
            var map = originalDefinition.TypeParameters
                .Zip(typeArguments, (p, a) => (p, a))
                .ToDictionary(x => x.p, x => x.a);

            return new Dictionary<ITypeParameterSymbol, ITypeSymbol>(map, SymbolEqualityComparer.Default);
        }

        var substitution = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(inheritedSubstitution, SymbolEqualityComparer.Default);
        var typeParameters = originalDefinition.TypeParameters;

        if (!typeArguments.IsDefaultOrEmpty)
        {
            for (var i = 0; i < typeParameters.Length && i < typeArguments.Length; i++)
                substitution[typeParameters[i]] = typeArguments[i];
        }

        return substitution;
    }

    private ConstructedNamedTypeSymbol(
        INamedTypeSymbol originalDefinition,
        ImmutableArray<ITypeSymbol> typeArguments,
        Dictionary<ITypeParameterSymbol, ITypeSymbol>? inheritedSubstitution,
        INamedTypeSymbol? containingTypeOverride)
    {
        ConstructedFrom = originalDefinition;
        _originalDefinition = originalDefinition;
        TypeArguments = typeArguments;
        _containingTypeOverride = containingTypeOverride;

        _substitutionMap = CreateSubstitutionMap(originalDefinition, typeArguments, inheritedSubstitution);
    }

    public ITypeSymbol Substitute(ITypeSymbol type)
    {
        if (type is ITypeParameterSymbol tp && _substitutionMap.TryGetValue(tp, out var concrete))
            return concrete;

        if (type is INamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
        {
            var args = named.TypeArguments.Select(Substitute).ToImmutableArray();
            if (named.ConstructedFrom is INamedTypeSymbol original)
                return new ConstructedNamedTypeSymbol(original, args);

            return type;
        }

        return type;
    }

    public ImmutableArray<ISymbol> GetMembers() =>
        _members ??= _originalDefinition.GetMembers().Select(SubstituteMember).ToImmutableArray();

    public ImmutableArray<ISymbol> GetMembers(string name) =>
        GetMembers().Where(m => m.Name == name).ToImmutableArray();

    internal ImmutableArray<ITypeSymbol> GetAllTypeArguments()
    {
        if (_containingTypeOverride is ConstructedNamedTypeSymbol constructedContaining)
        {
            var inherited = constructedContaining.GetAllTypeArguments();
            if (TypeArguments.IsDefaultOrEmpty || TypeArguments.Length == 0)
                return inherited;

            return inherited.AddRange(TypeArguments);
        }

        return TypeArguments.IsDefault ? ImmutableArray<ITypeSymbol>.Empty : TypeArguments;
    }

    private ISymbol SubstituteMember(ISymbol member) => member switch
    {
        IMethodSymbol m => new SubstitutedMethodSymbol(m, this),
        IFieldSymbol f => new SubstitutedFieldSymbol(f, this),
        IPropertySymbol p => new SubstitutedPropertySymbol(p, this),
        INamedTypeSymbol t => SubstituteNamedType(t),
        _ => member
    };

    private INamedTypeSymbol SubstituteNamedType(INamedTypeSymbol namedType)
    {
        var containingOverride = namedType.ContainingType is INamedTypeSymbol containing &&
            SymbolEqualityComparer.Default.Equals(containing, _originalDefinition)
            ? this
            : null;

        if (namedType.Arity == 0)
        {
            return containingOverride is not null
                ? new ConstructedNamedTypeSymbol(namedType, ImmutableArray<ITypeSymbol>.Empty, _substitutionMap, containingOverride)
                : namedType;
        }

        var typeArguments = new ITypeSymbol[namedType.Arity];
        var typeParameters = namedType.TypeParameters;
        var outerArity = _originalDefinition.Arity;

        for (var i = 0; i < typeArguments.Length; i++)
        {
            if (i < TypeArguments.Length)
            {
                typeArguments[i] = TypeArguments[i];
                continue;
            }

            var parameter = typeParameters[i];
            if (_substitutionMap.TryGetValue(parameter, out var replacement))
            {
                typeArguments[i] = replacement;
            }
            else if (i < outerArity)
            {
                typeArguments[i] = TypeArguments[i];
            }
            else
            {
                typeArguments[i] = parameter;
            }
        }

        if (containingOverride is null)
            return (INamedTypeSymbol)namedType.Construct(typeArguments);

        var immutableArguments = ImmutableArray.Create(typeArguments);
        return new ConstructedNamedTypeSymbol(namedType, immutableArguments, _substitutionMap, containingOverride);
    }

    // Symbol metadata forwarding
    public string Name => _originalDefinition.Name;
    public string MetadataName => _originalDefinition.MetadataName;
    public SymbolKind Kind => _originalDefinition.Kind;
    public TypeKind TypeKind
    {
        get
        {
            if (ConstructedFrom is INamedTypeSymbol constructedFrom)
                return constructedFrom.TypeKind;

            return _originalDefinition.TypeKind;
        }
    }
    public SpecialType SpecialType => _originalDefinition.SpecialType;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsReferenceType => _originalDefinition.IsReferenceType;
    public bool IsValueType => _originalDefinition.IsValueType;
    public INamedTypeSymbol? ContainingType => _containingTypeOverride ?? _originalDefinition.ContainingType;
    public INamespaceSymbol? ContainingNamespace => _originalDefinition.ContainingNamespace;
    public ISymbol? ContainingSymbol => _originalDefinition.ContainingSymbol;
    public IAssemblySymbol? ContainingAssembly => _originalDefinition.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _originalDefinition.ContainingModule;
    public Accessibility DeclaredAccessibility => _originalDefinition.DeclaredAccessibility;
    public bool IsStatic => false;
    public bool IsImplicitlyDeclared => true;
    public bool CanBeReferencedByName => true;
    public ImmutableArray<Location> Locations => _originalDefinition.Locations;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _originalDefinition.DeclaringSyntaxReferences;
    public ISymbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _originalDefinition.GetAttributes();
    public int Arity => TypeArguments.Length;
    public ImmutableArray<ITypeSymbol> GetTypeArguments() => TypeArguments;
    public ITypeSymbol? OriginalDefinition => _originalDefinition;
    public INamedTypeSymbol? BaseType => _originalDefinition.BaseType;
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _originalDefinition.TypeParameters;
    public ITypeSymbol? ConstructedFrom { get; }
    public bool IsAbstract => _originalDefinition.IsAbstract;
    public bool IsSealed => _originalDefinition.IsSealed;
    public bool IsGenericType => true;
    public bool IsUnboundGenericType => false;
    public ImmutableArray<INamedTypeSymbol> Interfaces =>
        _interfaces ??= _originalDefinition.Interfaces
            .Select(i => (INamedTypeSymbol)Substitute(i))
            .ToImmutableArray();
    public ImmutableArray<INamedTypeSymbol> AllInterfaces =>
        _allInterfaces ??= _originalDefinition.AllInterfaces
            .Select(i => (INamedTypeSymbol)Substitute(i))
            .ToImmutableArray();
    public ImmutableArray<IMethodSymbol> Constructors => GetMembers().OfType<IMethodSymbol>().Where(x => !x.IsStatic && x.IsConstructor).ToImmutableArray();
    public IMethodSymbol? StaticConstructor => GetMembers().OfType<IMethodSymbol>().Where(x => x.IsStatic && x.IsConstructor).FirstOrDefault();

    public INamedTypeSymbol UnderlyingTupleType
    {
        get
        {
            var underlying = _originalDefinition.UnderlyingTupleType;
            if (underlying is null)
                return null!;

            if (SymbolEqualityComparer.Default.Equals(underlying, _originalDefinition))
                return this;

            return SubstituteNamedType(underlying);
        }
    }

    public ImmutableArray<IFieldSymbol> TupleElements =>
        _tupleElements ??= _originalDefinition.TupleElements
            .Select(member => SubstituteMember(member))
            .OfType<IFieldSymbol>()
            .ToImmutableArray();

    public void Accept(SymbolVisitor visitor) => visitor.VisitNamedType(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamedType(this);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        if (_containingTypeOverride is null)
            return _originalDefinition.Construct(typeArguments);

        var immutableArguments = ImmutableArray.Create(typeArguments);
        return new ConstructedNamedTypeSymbol(_originalDefinition, immutableArguments, _substitutionMap, _containingTypeOverride);
    }

    public ITypeSymbol? LookupType(string name)
    {
        foreach (var member in _originalDefinition.GetMembers(name))
        {
            if (member is INamedTypeSymbol namedType)
                return SubstituteNamedType(namedType);
        }

        return null;
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        foreach (var member in _originalDefinition.GetMembers(name))
        {
            if (member.Name == name)
            {
                symbol = SubstituteMember(member);
                return true;
            }
        }

        symbol = null;
        return false;
    }

    internal System.Reflection.TypeInfo GetTypeInfo(CodeGenerator codeGen)
    {
        var runtimeArguments = GetAllTypeArguments();

        if (_originalDefinition is PENamedTypeSymbol pen)
        {
            var genericTypeDef = pen.GetClrType(codeGen);
            if (runtimeArguments.IsDefaultOrEmpty)
                return genericTypeDef.GetTypeInfo();

            var resolved = runtimeArguments
                .Select(arg => ResolveRuntimeTypeArgument(arg, codeGen))
                .ToArray();
            return genericTypeDef.MakeGenericType(resolved).GetTypeInfo();
        }

        if (_originalDefinition is SourceNamedTypeSymbol source)
        {
            var definitionType = codeGen.GetTypeBuilder(source) ?? throw new InvalidOperationException("Missing type builder for generic definition.");
            if (runtimeArguments.IsDefaultOrEmpty)
                return definitionType.GetTypeInfo();

            var runtimeArgs = runtimeArguments
                .Select(arg => ResolveRuntimeTypeArgument(arg, codeGen))
                .ToArray();
            var constructed = definitionType.MakeGenericType(runtimeArgs);
            return constructed.GetTypeInfo();
        }

        throw new InvalidOperationException("ConstructedNamedTypeSymbol is not based on a supported symbol type.");
    }

    private Type ResolveRuntimeTypeArgument(ITypeSymbol typeArgument, CodeGenerator codeGen)
    {
        if (typeArgument is ITypeParameterSymbol { ContainingSymbol: IMethodSymbol methodSymbol } methodTypeParameter)
        {
            if (TryGetMethodGenericParameter(methodSymbol, methodTypeParameter.Ordinal, codeGen, out var methodParameter))
                return methodParameter;

            if (codeGen.TryGetRuntimeTypeForTypeParameter(methodTypeParameter, out var resolved))
            {
                if (resolved is { IsGenericParameter: true, DeclaringMethod: not null })
                    return resolved;

                if (TryGetMethodGenericParameter(methodSymbol, methodTypeParameter.Ordinal, codeGen, out var refreshedMethodParameter))
                {
                    codeGen.CacheRuntimeTypeParameter(methodTypeParameter, refreshedMethodParameter);
                    return refreshedMethodParameter;
                }

                if (_originalDefinition is SynthesizedAsyncStateMachineTypeSymbol stateMachine &&
                    TryGetMethodGenericParameter(stateMachine.AsyncMethod, methodTypeParameter.Ordinal, codeGen, out var asyncMethodParameter))
                {
                    codeGen.CacheRuntimeTypeParameter(methodTypeParameter, asyncMethodParameter);
                    return asyncMethodParameter;
                }

                return resolved;
            }

            if (_originalDefinition is SynthesizedAsyncStateMachineTypeSymbol fallbackStateMachine &&
                TryGetMethodGenericParameter(fallbackStateMachine.AsyncMethod, methodTypeParameter.Ordinal, codeGen, out var mappedFallback))
            {
                codeGen.CacheRuntimeTypeParameter(methodTypeParameter, mappedFallback);
                return mappedFallback;
            }
        }

        if (typeArgument is ITypeParameterSymbol typeParameter)
        {
            if (codeGen.TryGetRuntimeTypeForTypeParameter(typeParameter, out var runtimeType))
                return runtimeType;

            if (TryGetMappedAsyncParameter(typeParameter, out var stateMachine, out var asyncParameter) &&
                stateMachine is not null &&
                asyncParameter is not null)
            {
                if (TryGetMethodGenericParameter(stateMachine.AsyncMethod, asyncParameter.Ordinal, codeGen, out var asyncMethodParameter))
                {
                    codeGen.CacheRuntimeTypeParameter(asyncParameter, asyncMethodParameter);
                    return asyncMethodParameter;
                }

                if (codeGen.TryGetRuntimeTypeForTypeParameter(asyncParameter, out var asyncResolved))
                {
                    if (asyncResolved is { IsGenericParameter: true, DeclaringMethod: not null })
                        return asyncResolved;

                    if (TryGetMethodGenericParameter(stateMachine.AsyncMethod, asyncParameter.Ordinal, codeGen, out var refreshedAsyncMethodParameter))
                    {
                        codeGen.CacheRuntimeTypeParameter(asyncParameter, refreshedAsyncMethodParameter);
                        return refreshedAsyncMethodParameter;
                    }

                    throw new InvalidOperationException("Unable to map async method type parameter to runtime generic parameter.");
                }

                if (TryGetMethodGenericParameter(stateMachine.AsyncMethod, asyncParameter.Ordinal, codeGen, out var mappedFallback))
                {
                    codeGen.CacheRuntimeTypeParameter(asyncParameter, mappedFallback);
                    return mappedFallback;
                }

                throw new InvalidOperationException("Unable to resolve async method generic parameter for state machine mapping.");
            }
            throw new InvalidOperationException("Unable to map state machine type parameter to async method generic parameter.");
        }

        return typeArgument.GetClrType(codeGen);
    }

    private static bool TryGetMappedAsyncParameter(
        ITypeParameterSymbol typeParameter,
        out SynthesizedAsyncStateMachineTypeSymbol? stateMachine,
        out ITypeParameterSymbol? asyncParameter)
    {
        stateMachine = null;
        asyncParameter = null;

        var containingType = typeParameter.ContainingType;
        if (containingType is SynthesizedAsyncStateMachineTypeSymbol direct &&
            direct.TryMapToAsyncMethodTypeParameter(typeParameter, out var mapped))
        {
            stateMachine = direct;
            asyncParameter = mapped;
            return true;
        }

        if (containingType is ConstructedNamedTypeSymbol constructed &&
            constructed.ConstructedFrom is SynthesizedAsyncStateMachineTypeSymbol constructedStateMachine &&
            typeParameter.OriginalDefinition is ITypeParameterSymbol original &&
            constructedStateMachine.TryMapToAsyncMethodTypeParameter(original, out mapped))
        {
            stateMachine = constructedStateMachine;
            asyncParameter = mapped;
            return true;
        }

        return false;
    }

    private static bool TryGetMethodGenericParameter(IMethodSymbol methodSymbol, int ordinal, CodeGenerator codeGen, out Type parameter)
    {
        if (methodSymbol is null)
            throw new ArgumentNullException(nameof(methodSymbol));

        parameter = null!;

        if (TryGetSourceMethod(methodSymbol, out var sourceMethod) &&
            codeGen.TryGetMemberBuilder(sourceMethod, out var member) &&
            member is MethodInfo methodInfo)
        {
            var definition = methodInfo.IsGenericMethodDefinition
                ? methodInfo
                : methodInfo.GetGenericMethodDefinition();

            var arguments = definition.GetGenericArguments();
            if ((uint)ordinal < (uint)arguments.Length)
            {
                parameter = arguments[ordinal];
                return true;
            }
        }

        return false;
    }

    private static bool TryGetSourceMethod(IMethodSymbol methodSymbol, out SourceMethodSymbol sourceMethod)
    {
        switch (methodSymbol)
        {
            case SourceMethodSymbol source:
                sourceMethod = source;
                return true;
            case IAliasSymbol alias when alias.UnderlyingSymbol is IMethodSymbol aliasMethod &&
                TryGetSourceMethod(aliasMethod, out sourceMethod):
                return true;
        }

        if (methodSymbol.UnderlyingSymbol is IMethodSymbol underlying &&
            !ReferenceEquals(underlying, methodSymbol) &&
            TryGetSourceMethod(underlying, out sourceMethod))
        {
            return true;
        }

        var originalDefinition = methodSymbol.OriginalDefinition;
        if (originalDefinition is not null &&
            !ReferenceEquals(originalDefinition, methodSymbol) &&
            TryGetSourceMethod(originalDefinition, out sourceMethod))
        {
            return true;
        }

        var constructedFrom = methodSymbol.ConstructedFrom;
        if (constructedFrom is not null &&
            !ReferenceEquals(constructedFrom, methodSymbol) &&
            TryGetSourceMethod(constructedFrom, out sourceMethod))
        {
            return true;
        }

        sourceMethod = null!;
        return false;
    }
}

internal sealed class SubstitutedMethodSymbol : IMethodSymbol
{
    private readonly IMethodSymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructed;
    private ImmutableArray<IParameterSymbol>? _parameters;
    private Dictionary<ITypeParameterSymbol, ImmutableArray<ITypeSymbol>>? _constraintTypeMap;

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

    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => _original.GetReturnTypeAttributes();

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
    public bool IsIterator => _original.IsIterator;
    public IteratorMethodKind IteratorKind => _original.IteratorKind;
    public ITypeSymbol? IteratorElementType => _original.IteratorElementType;
    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => _original.ExplicitInterfaceImplementations;
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _original.TypeParameters;
    public ImmutableArray<ITypeSymbol> TypeArguments => _original.TypeArguments;
    public IMethodSymbol? ConstructedFrom => _original.ConstructedFrom ?? _original;
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
    public ISymbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();

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

    public IMethodSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        return new ConstructedMethodSymbol(this, typeArguments.ToImmutableArray());
    }

    internal bool TryGetSubstitutedConstraintTypes(
        ITypeParameterSymbol typeParameter,
        out ImmutableArray<ITypeSymbol> substituted)
    {
        substituted = default;

        if (!_original.TypeParameters.Contains(typeParameter))
            return false;

        _constraintTypeMap ??= new Dictionary<ITypeParameterSymbol, ImmutableArray<ITypeSymbol>>(SymbolEqualityComparer.Default);

        if (_constraintTypeMap.TryGetValue(typeParameter, out substituted))
            return true;

        var originalConstraints = typeParameter.ConstraintTypes;

        if (originalConstraints.IsDefaultOrEmpty || originalConstraints.Length == 0)
        {
            substituted = originalConstraints;
            _constraintTypeMap[typeParameter] = substituted;
            return true;
        }

        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(originalConstraints.Length);

        foreach (var constraint in originalConstraints)
            builder.Add(_constructed.Substitute(constraint));

        substituted = builder.ToImmutable();
        _constraintTypeMap[typeParameter] = substituted;
        return true;
    }

    internal ConstructorInfo GetConstructorInfo(CodeGenerator codeGen)
    {
        if (_original is SourceMethodSymbol cachedSource &&
            codeGen.TryGetMemberBuilder(cachedSource, _constructed.TypeArguments, out var cachedMember) &&
            cachedMember is ConstructorInfo cachedConstructor)
        {
            return cachedConstructor;
        }

        if (_original is PEMethodSymbol peMethod)
        {
            var baseCtor = peMethod.GetClrConstructorInfo(codeGen);

            if (baseCtor.DeclaringType.IsGenericType)
            {
                var constructedType = _constructed.GetTypeInfo(codeGen).AsType();
                var parameters = baseCtor.GetParameters().Select(p => p.ParameterType).ToArray();
                return constructedType.GetConstructor(parameters)!;
            }

            return baseCtor;
        }

        if (_original is SourceMethodSymbol sourceMethod)
        {
            var constructedType = _constructed.GetTypeInfo(codeGen).AsType();
            if (codeGen.GetMemberBuilder(sourceMethod) is ConstructorInfo definitionCtor)
            {
                var constructedCtor = TypeBuilder.GetConstructor(constructedType, definitionCtor);
                if (constructedCtor is not null)
                {
                    codeGen.AddMemberBuilder(sourceMethod, constructedCtor, _constructed.TypeArguments);
                    return constructedCtor;
                }
            }

            throw new InvalidOperationException("Constructor builder not found for source method.");
        }

        throw new Exception("Unexpected method kind");
    }

    internal MethodInfo GetMethodInfo(CodeGenerator codeGen)
    {
        if (_original is SourceMethodSymbol cachedSource &&
            codeGen.TryGetMemberBuilder(cachedSource, _constructed.TypeArguments, out var cachedMember) &&
            cachedMember is MethodInfo cachedMethod)
        {
            return cachedMethod;
        }

        if (_original is PEMethodSymbol peMethod)
        {
            var baseMethod = peMethod.GetClrMethodInfo(codeGen);

            // Resolve the constructed runtime type
            var constructedType = _constructed.GetTypeInfo(codeGen).AsType();

            if (constructedType is TypeBuilder || constructedType.GetType().FullName == "System.Reflection.Emit.TypeBuilderInstantiation")
            {
                var instantiated = TypeBuilder.GetMethod(constructedType, baseMethod);
                if (instantiated is not null)
                    return instantiated;
            }

            // Use metadata name and parameter types to resolve the method on the constructed type
            var parameterTypes = Parameters
                .Select(x => x.Type.GetClrTypeTreatingUnitAsVoid(codeGen))
                .ToArray();
            var method = constructedType.GetMethod(
                baseMethod.Name,
                BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static,
                null,
                parameterTypes,
                null
            );

            if (method != null)
                return method;

            throw new MissingMethodException($"Method '{baseMethod.Name}' with specified parameters not found on constructed type '{constructedType}'.");
        }

        if (_original is SourceMethodSymbol sourceMethod)
        {
            if (codeGen.GetMemberBuilder(sourceMethod) is not MethodInfo definitionMethod)
                throw new InvalidOperationException("Method builder not found for source method.");

            var constructedType = _constructed.GetTypeInfo(codeGen).AsType();

            if (!ReferenceEquals(constructedType, definitionMethod.DeclaringType) && constructedType.IsGenericType)
            {
                var constructedMethod = TypeBuilder.GetMethod(constructedType, definitionMethod);
                if (constructedMethod is not null)
                {
                    codeGen.AddMemberBuilder(sourceMethod, constructedMethod, _constructed.TypeArguments);
                    return constructedMethod;
                }
            }

            if (ReferenceEquals(constructedType, definitionMethod.DeclaringType))
                return definitionMethod;

            var parameterTypes = sourceMethod.Parameters
                .Select(p => p.Type.GetClrTypeTreatingUnitAsVoid(codeGen))
                .ToArray();

            var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
            var resolved = constructedType.GetMethod(definitionMethod.Name, bindingFlags, null, parameterTypes, null);
            if (resolved is not null)
            {
                codeGen.AddMemberBuilder(sourceMethod, resolved, _constructed.TypeArguments);
                return resolved;
            }

            throw new MissingMethodException($"Method '{definitionMethod.Name}' with specified parameters not found on constructed type '{constructedType}'.");
        }

        throw new InvalidOperationException("Expected PE or source method symbol.");
    }
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
    public ISymbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();

    public void Accept(SymbolVisitor visitor) => visitor.VisitField(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitField(this);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);

    public object? GetConstantValue() => _original.GetConstantValue();

    internal FieldInfo GetFieldInfo(CodeGenerator codeGen)
    {
        if (_original is SourceFieldSymbol cachedSource &&
            codeGen.TryGetMemberBuilder(cachedSource, _constructed.TypeArguments, out var cachedMember) &&
            cachedMember is FieldInfo cachedField)
        {
            return cachedField;
        }

        if (_original is PEFieldSymbol peField)
        {
            var field = peField.GetFieldInfo();
            var constructedType = _constructed.GetTypeInfo(codeGen).AsType();
            return constructedType.GetField(field.Name)!;
        }

        if (_original is SourceFieldSymbol sourceField)
        {
            if (codeGen.GetMemberBuilder(sourceField) is not FieldInfo definitionField)
                throw new InvalidOperationException("Field builder not found for source field.");

            var constructedType = _constructed.GetTypeInfo(codeGen).AsType();

            if (!ReferenceEquals(constructedType, definitionField.DeclaringType) && constructedType.IsGenericType)
            {
                var constructedField = TypeBuilder.GetField(constructedType, definitionField);
                if (constructedField is not null)
                {
                    codeGen.AddMemberBuilder(sourceField, constructedField, _constructed.TypeArguments);
                    return constructedField;
                }
            }

            if (ReferenceEquals(constructedType, definitionField.DeclaringType))
                return definitionField;

            var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
            var resolved = constructedType.GetField(definitionField.Name, bindingFlags);
            if (resolved is not null)
            {
                codeGen.AddMemberBuilder(sourceField, resolved, _constructed.TypeArguments);
                return resolved;
            }

            throw new MissingFieldException(constructedType.FullName, definitionField.Name);
        }

        throw new Exception("Not a supported field symbol.");
    }
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
    public ISymbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();

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
    public ISymbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
    public bool IsParams => _original.IsParams;
    public RefKind RefKind => _original.RefKind;
    public bool IsMutable => _original.IsMutable;
    public bool HasExplicitDefaultValue => _original.HasExplicitDefaultValue;
    public object? ExplicitDefaultValue => _original.ExplicitDefaultValue;

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
