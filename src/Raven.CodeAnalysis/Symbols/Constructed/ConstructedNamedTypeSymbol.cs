using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.CodeGen;

namespace Raven.CodeAnalysis.Symbols;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
internal sealed class ConstructedNamedTypeSymbol : INamedTypeSymbol, IDiscriminatedUnionSymbol, IDiscriminatedUnionCaseSymbol
{
    private readonly INamedTypeSymbol _originalDefinition;
    private readonly Dictionary<ITypeParameterSymbol, ITypeSymbol> _substitutionMap;
    private readonly INamedTypeSymbol? _containingTypeOverride;
    private ImmutableArray<ISymbol>? _members;
    private ImmutableArray<IFieldSymbol>? _tupleElements;
    private ImmutableArray<INamedTypeSymbol>? _interfaces;
    private ImmutableArray<INamedTypeSymbol>? _allInterfaces;
    private ImmutableArray<IDiscriminatedUnionCaseSymbol>? _cases;
    private ImmutableArray<IParameterSymbol>? _constructorParameters;
    private ImmutableArray<ITypeParameterSymbol> _typeParameters;
    private IDiscriminatedUnionSymbol? _union;
    private IFieldSymbol? _discriminatorField;
    private IFieldSymbol? _payloadField;

    private readonly ImmutableArray<ITypeSymbol> _explicitTypeArguments;
    private ImmutableArray<ITypeSymbol> _typeArguments;
    private ImmutableArray<ITypeSymbol> _allTypeArguments;

    public ConstructedNamedTypeSymbol(INamedTypeSymbol originalDefinition, ImmutableArray<ITypeSymbol> typeArguments)
        : this(originalDefinition, typeArguments, inheritedSubstitution: null, containingTypeOverride: null)
    {
    }

    private static Dictionary<ITypeParameterSymbol, ITypeSymbol> CreateSubstitutionMap(
     INamedTypeSymbol originalDefinition,
     ImmutableArray<ITypeSymbol> typeArguments,
     Dictionary<ITypeParameterSymbol, ITypeSymbol>? inheritedSubstitution)
    {
        static ITypeParameterSymbol NormalizeKey(ITypeParameterSymbol parameter) =>
            (ITypeParameterSymbol)(parameter.OriginalDefinition ?? parameter);

        var substitution = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(
            TypeParameterSubstitutionComparer.Instance);

        if (inheritedSubstitution is not null)
        {
            foreach (var (key, value) in inheritedSubstitution)
                substitution[NormalizeKey(key)] = value;
        }

        if (!typeArguments.IsDefaultOrEmpty)
        {
            var typeParameters = originalDefinition.TypeParameters;
            var argIndex = 0;

            foreach (var rawParam in typeParameters)
            {
                if (argIndex >= typeArguments.Length)
                    break;

                var parameter = NormalizeKey(rawParam);
                var argument = typeArguments[argIndex++];

                if (substitution.TryGetValue(parameter, out var existing))
                {
                    // If they’re logically equal, nothing to do
                    if (SymbolEqualityComparer.Default.Equals(existing, argument))
                        continue;

                    // If we already have a concrete type and the new one is still a TP,
                    // keep the existing mapping.
                    if (existing is not ITypeParameterSymbol && argument is ITypeParameterSymbol)
                        continue;
                }

                substitution[parameter] = argument;
            }
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
        _explicitTypeArguments = typeArguments.IsDefault ? ImmutableArray<ITypeSymbol>.Empty : typeArguments;
        _containingTypeOverride = containingTypeOverride;

        _substitutionMap = CreateSubstitutionMap(originalDefinition, typeArguments, inheritedSubstitution);
    }

    public ImmutableArray<ITypeSymbol> TypeArguments =>
        _typeArguments.IsDefault ? _typeArguments = BuildTypeArguments() : _typeArguments;

    private bool TryGetSubstitution(ITypeParameterSymbol parameter, out ITypeSymbol replacement)
    {
        var lookup = (ITypeParameterSymbol)(parameter.OriginalDefinition ?? parameter);
        return _substitutionMap.TryGetValue(lookup, out replacement!);
    }

    public ITypeSymbol Substitute(
        ITypeSymbol type,
        Dictionary<ITypeParameterSymbol, ITypeParameterSymbol>? methodMap = null)
    {
        if (type is ITypeParameterSymbol tp)
        {
            if (methodMap is not null && methodMap.TryGetValue(tp, out var mappedMethodParameter))
                return mappedMethodParameter;

            if (TryGetSubstitution(tp, out var concrete))
                return concrete;
        }

        if (type is NullableTypeSymbol nullableTypeSymbol)
        {
            var underlyingType = Substitute(nullableTypeSymbol.UnderlyingType, methodMap);

            if (!SymbolEqualityComparer.Default.Equals(underlyingType, nullableTypeSymbol.UnderlyingType))
                return underlyingType.MakeNullable();

            return type;
        }

        if (type is ByRefTypeSymbol byRef)
        {
            var substitutedElement = Substitute(byRef.ElementType, methodMap);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, byRef.ElementType))
                return new ByRefTypeSymbol(substitutedElement);

            return type;
        }

        if (type is IAddressTypeSymbol address)
        {
            var substitutedElement = Substitute(address.ReferencedType, methodMap);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, address.ReferencedType))
                return new AddressTypeSymbol(substitutedElement);

            return type;
        }

        if (type is IArrayTypeSymbol arrayType)
        {
            var substitutedElement = Substitute(arrayType.ElementType, methodMap);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, arrayType.ElementType))
                return new ArrayTypeSymbol(arrayType.BaseType, substitutedElement, arrayType.ContainingSymbol, arrayType.ContainingType, arrayType.ContainingNamespace, [], arrayType.Rank);

            return type;
        }

        if (type is INamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
        {
            var typeArguments = named.TypeArguments;
            var substitutedArgs = new ITypeSymbol[typeArguments.Length];
            var changed = false;

            for (int i = 0; i < typeArguments.Length; i++)
            {
                var originalArg = typeArguments[i];
                var substitutedArg = Substitute(originalArg, methodMap);

                substitutedArgs[i] = substitutedArg;

                if (!SymbolEqualityComparer.Default.Equals(substitutedArg, originalArg))
                    changed = true;
            }

            if (!changed)
                return named;

            // Avoid reusing a possibly already-constructed named
            var constructedFrom = (INamedTypeSymbol?)named.ConstructedFrom ?? named;
            return constructedFrom.Construct(substitutedArgs);
        }

        if (type is INamedTypeSymbol nestedNamed &&
            !nestedNamed.IsGenericType &&
            nestedNamed.ContainingType is INamedTypeSymbol containingType &&
            containingType.TypeParameters.Length > 0)
        {
            var needsContainingSubstitution = false;
            foreach (var typeParameter in containingType.TypeParameters)
            {
                if (TryGetSubstitution(typeParameter, out var replacement) &&
                    !SymbolEqualityComparer.Default.Equals(replacement, typeParameter))
                {
                    needsContainingSubstitution = true;
                    break;
                }
            }

            if (needsContainingSubstitution)
                return SubstituteNamedType(nestedNamed);
        }

        return type;
    }

    public ImmutableArray<ISymbol> GetMembers() =>
        _members ??= _originalDefinition.GetMembers().Select(SubstituteMember).ToImmutableArray();

    public ImmutableArray<ISymbol> GetMembers(string name) =>
        GetMembers().Where(m => m.Name == name).ToImmutableArray();

    internal ImmutableArray<ITypeSymbol> GetAllTypeArguments() =>
        _allTypeArguments.IsDefault ? _allTypeArguments = BuildAllTypeArguments() : _allTypeArguments;

    private ImmutableArray<ITypeSymbol> BuildAllTypeArguments()
    {
        var selfArgs = TypeArguments;

        if (_containingTypeOverride is ConstructedNamedTypeSymbol constructedContaining)
        {
            var outerArgs = constructedContaining.GetAllTypeArguments();
            if (selfArgs.IsDefaultOrEmpty || selfArgs.Length == 0)
                return outerArgs;

            return outerArgs.AddRange(selfArgs);
        }

        return selfArgs;
    }

    private ImmutableArray<ITypeSymbol> BuildTypeArguments()
    {
        // Nested non-generic types (e.g. Test<T>.A) do NOT become generic.
        // Outer substitution is carried via _substitutionMap and _containingTypeOverride.
        return NormalizeTypeArguments(_explicitTypeArguments);
    }

    private ImmutableArray<ITypeParameterSymbol> BuildTypeParameters()
    {
        // Nested types do not implicitly inherit containing type parameters.
        return _originalDefinition.TypeParameters;
    }

    private ImmutableArray<ITypeSymbol> NormalizeTypeArguments(ImmutableArray<ITypeSymbol> typeArguments)
    {
        if (typeArguments.IsDefault)
            return ImmutableArray<ITypeSymbol>.Empty;

        if (typeArguments.IsDefaultOrEmpty || typeArguments.Length == 0)
            return typeArguments;

        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>(typeArguments.Length);
        foreach (var argument in typeArguments)
            builder.Add(Substitute(argument));

        return builder.MoveToImmutable();
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

        var typeParameters = namedType.TypeParameters;
        if (typeParameters.Length == 0)
        {
            return containingOverride is not null
                ? new ConstructedNamedTypeSymbol(namedType, ImmutableArray<ITypeSymbol>.Empty, _substitutionMap, containingOverride)
                : namedType;
        }

        var typeArguments = new ITypeSymbol[typeParameters.Length];
        for (var i = 0; i < typeParameters.Length; i++)
        {
            var parameter = typeParameters[i];
            if (TryGetSubstitution(parameter, out var replacement))
            {
                typeArguments[i] = replacement;
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

    private bool TryGetUnionDefinition(out IDiscriminatedUnionSymbol unionDefinition)
    {
        if (_originalDefinition is IDiscriminatedUnionSymbol union)
        {
            unionDefinition = union;
            return true;
        }

        if (ConstructedFrom is IDiscriminatedUnionSymbol constructedUnion)
        {
            unionDefinition = constructedUnion;
            return true;
        }

        unionDefinition = null!;
        return false;
    }

    private bool TryGetCaseDefinition(out IDiscriminatedUnionCaseSymbol caseDefinition)
    {
        if (_originalDefinition is IDiscriminatedUnionCaseSymbol caseSymbol)
        {
            caseDefinition = caseSymbol;
            return true;
        }

        if (ConstructedFrom is IDiscriminatedUnionCaseSymbol constructedCase)
        {
            caseDefinition = constructedCase;
            return true;
        }

        caseDefinition = null!;
        return false;
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
    public bool IsDiscriminatedUnion => _originalDefinition.IsDiscriminatedUnion;
    public bool IsDiscriminatedUnionCase => _originalDefinition.IsDiscriminatedUnionCase;
    public INamedTypeSymbol? UnderlyingDiscriminatedUnion => _originalDefinition.UnderlyingDiscriminatedUnion;
    public INamedTypeSymbol? ContainingType => _containingTypeOverride ?? _originalDefinition.ContainingType;
    public INamespaceSymbol? ContainingNamespace => _originalDefinition.ContainingNamespace;
    public ISymbol? ContainingSymbol => _originalDefinition.ContainingSymbol;
    public IAssemblySymbol? ContainingAssembly => _originalDefinition.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _originalDefinition.ContainingModule;
    public Accessibility DeclaredAccessibility => _originalDefinition.DeclaredAccessibility;
    public bool IsStatic => _originalDefinition.IsStatic;
    public bool IsImplicitlyDeclared => true;
    public bool CanBeReferencedByName => true;
    public ImmutableArray<Location> Locations => _originalDefinition.Locations;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _originalDefinition.DeclaringSyntaxReferences;
    public ISymbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _originalDefinition.GetAttributes();
    public int Arity => _originalDefinition.Arity;
    public ImmutableArray<ITypeSymbol> GetTypeArguments() => TypeArguments;
    public ITypeSymbol? OriginalDefinition => _originalDefinition;
    public INamedTypeSymbol? BaseType => _originalDefinition.BaseType;
    public ImmutableArray<ITypeParameterSymbol> TypeParameters =>
        _typeParameters.IsDefault ? _typeParameters = BuildTypeParameters() : _typeParameters;
    public ITypeSymbol? ConstructedFrom { get; }
    public bool IsAbstract => _originalDefinition.IsAbstract;
    public bool IsSealed => _originalDefinition.IsSealed;
    public bool IsGenericType => _originalDefinition.IsGenericType;
    public bool IsUnboundGenericType => false;
    public ImmutableArray<INamedTypeSymbol> Interfaces =>
        _interfaces ??= _originalDefinition.Interfaces
            .Select(i => (INamedTypeSymbol)Substitute(i))
            .ToImmutableArray();
    public ImmutableArray<INamedTypeSymbol> AllInterfaces =>
        _allInterfaces ??= _originalDefinition.AllInterfaces
            .Select(i => (INamedTypeSymbol)Substitute(i))
            .ToImmutableArray();
    public ImmutableArray<IDiscriminatedUnionCaseSymbol> Cases
    {
        get
        {
            if (!TryGetUnionDefinition(out var unionDefinition))
                return ImmutableArray<IDiscriminatedUnionCaseSymbol>.Empty;

            return _cases ??= SubstituteUnionCases(unionDefinition.Cases);
        }
    }
    public IFieldSymbol DiscriminatorField
    {
        get
        {
            if (_discriminatorField is not null)
                return _discriminatorField;

            if (!TryGetUnionDefinition(out var unionDefinition))
                throw new InvalidOperationException("Constructed type is not a discriminated union.");

            _discriminatorField = new SubstitutedFieldSymbol(unionDefinition.DiscriminatorField, this);
            return _discriminatorField;
        }
    }
    public IFieldSymbol PayloadField
    {
        get
        {
            if (_payloadField is not null)
                return _payloadField;

            if (!TryGetUnionDefinition(out var unionDefinition))
                throw new InvalidOperationException("Constructed type is not a discriminated union.");

            _payloadField = new SubstitutedFieldSymbol(unionDefinition.PayloadField, this);
            return _payloadField;
        }
    }
    public ImmutableArray<IMethodSymbol> Constructors => GetMembers().OfType<IMethodSymbol>().Where(x => !x.IsStatic && x.IsConstructor).ToImmutableArray();
    public ImmutableArray<IMethodSymbol> InstanceConstructors => Constructors;
    public IMethodSymbol? StaticConstructor => GetMembers().OfType<IMethodSymbol>().Where(x => x.IsStatic && x.IsConstructor).FirstOrDefault();

    public IDiscriminatedUnionSymbol Union
    {
        get
        {
            if (_union is not null)
                return _union;

            if (ContainingType is IDiscriminatedUnionSymbol containingUnion)
                return _union = containingUnion;

            if (!TryGetCaseDefinition(out var caseDefinition))
                throw new InvalidOperationException("Constructed type is not a discriminated union case.");

            return _union = (IDiscriminatedUnionSymbol)SubstituteNamedType((INamedTypeSymbol)caseDefinition.Union);
        }
    }

    public ImmutableArray<IParameterSymbol> ConstructorParameters
    {
        get
        {
            if (!TryGetCaseDefinition(out var caseDefinition))
                return ImmutableArray<IParameterSymbol>.Empty;

            return _constructorParameters ??= caseDefinition.ConstructorParameters
                .Select(p => (IParameterSymbol)new SubstitutedParameterSymbol(p, this))
                .ToImmutableArray();
        }
    }

    public int Ordinal => TryGetCaseDefinition(out var caseDefinition) ? caseDefinition.Ordinal : 0;

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

    private ImmutableArray<IDiscriminatedUnionCaseSymbol> SubstituteUnionCases(ImmutableArray<IDiscriminatedUnionCaseSymbol> cases)
    {
        if (cases.IsDefaultOrEmpty || cases.Length == 0)
            return cases;

        var builder = ImmutableArray.CreateBuilder<IDiscriminatedUnionCaseSymbol>(cases.Length);
        foreach (var caseSymbol in cases)
            builder.Add((IDiscriminatedUnionCaseSymbol)SubstituteNamedType((INamedTypeSymbol)caseSymbol));

        return builder.MoveToImmutable();
    }

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

    public string ToFullyQualifiedMetadataName()
    {
        var segments = new Stack<string>();

        for (INamedTypeSymbol? current = this; current is not null; current = current.ContainingType)
        {
            var definition = (current as ConstructedNamedTypeSymbol)?._originalDefinition ?? current;
            var metadataName = definition.Name;
            var arity = definition.Arity;

            if (arity > 0)
                metadataName = $"{metadataName}`{arity}";

            segments.Push(metadataName);
        }

        var typeName = segments.Count > 0
            ? string.Join("+", segments)
            : MetadataName;

        var containingNamespace = ContainingNamespace;

        if (containingNamespace is null || containingNamespace.IsGlobalNamespace)
            return typeName;

        var namespaceName = containingNamespace.ToMetadataName();

        return string.IsNullOrEmpty(namespaceName)
            ? typeName
            : $"{namespaceName}.{typeName}";
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
            if (source.IsExtensionDeclaration)
                return definitionType.GetTypeInfo();
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

internal sealed class SubstitutedMethodSymbol : IMethodSymbol
{
    private readonly IMethodSymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructed;
    private ImmutableArray<IParameterSymbol>? _parameters;
    private Dictionary<ITypeParameterSymbol, ImmutableArray<ITypeSymbol>>? _constraintTypeMap;
    private ImmutableArray<IMethodSymbol>? _explicitInterfaceImplementations;
    private readonly ImmutableArray<ITypeParameterSymbol> _typeParameters;
    private readonly Dictionary<ITypeParameterSymbol, ITypeParameterSymbol> _methodTypeParameterMap;

    public SubstitutedMethodSymbol(IMethodSymbol original, ConstructedNamedTypeSymbol constructed)
    {
        _original = original;
        _constructed = constructed;

        if (!_original.TypeParameters.IsDefaultOrEmpty)
        {
            var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(_original.TypeParameters.Length);
            _methodTypeParameterMap = new Dictionary<ITypeParameterSymbol, ITypeParameterSymbol>(
                _original.TypeParameters.Length,
                TypeParameterSubstitutionComparer.Instance);

            foreach (var typeParameter in _original.TypeParameters)
            {
                var substituted = new SubstitutedMethodTypeParameterSymbol(
                    typeParameter,
                    this,
                    _constructed,
                    _constructed.ContainingNamespace ?? typeParameter.ContainingNamespace);

                builder.Add(substituted);
                _methodTypeParameterMap[typeParameter] = substituted;
            }

            _typeParameters = builder.ToImmutable();
        }
        else
        {
            _typeParameters = _original.TypeParameters.IsDefault
                ? ImmutableArray<ITypeParameterSymbol>.Empty
                : _original.TypeParameters;
            _methodTypeParameterMap = new Dictionary<ITypeParameterSymbol, ITypeParameterSymbol>(
                TypeParameterSubstitutionComparer.Instance);
        }
    }

    public string Name => _original.Name;
    public ITypeSymbol ReturnType => _constructed.Substitute(_original.ReturnType, _methodTypeParameterMap);
    public ImmutableArray<IParameterSymbol> Parameters =>
        _parameters ??= _original.Parameters
            .Select(p => (IParameterSymbol)new SubstitutedParameterSymbol(p, _constructed, _methodTypeParameterMap))
            .ToImmutableArray();

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
    public bool IsFinal => _original.IsFinal;
    public bool IsVirtual => _original.IsVirtual;
    public bool IsIterator => _original.IsIterator;
    public IteratorMethodKind IteratorKind => _original.IteratorKind;
    public ITypeSymbol? IteratorElementType => _original.IteratorElementType;

    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations
    {
        get
        {
            if (_explicitInterfaceImplementations.HasValue)
                return _explicitInterfaceImplementations.Value;

            var originals = _original.ExplicitInterfaceImplementations;

            if (originals.IsDefaultOrEmpty || originals.Length == 0)
            {
                _explicitInterfaceImplementations = originals;
                return originals;
            }

            var builder = ImmutableArray.CreateBuilder<IMethodSymbol>(originals.Length);

            foreach (var origImpl in originals)
            {
                var origIface = (INamedTypeSymbol?)origImpl.ContainingType;
                if (origIface is null)
                {
                    builder.Add(origImpl);
                    continue;
                }

                // Substitute the interface type with the constructed type’s substitution
                var substitutedIface = _constructed.Substitute(origIface) as INamedTypeSymbol;

                // If nothing changed, just reuse the original method
                if (substitutedIface is null ||
                    SymbolEqualityComparer.Default.Equals(origIface, substitutedIface))
                {
                    builder.Add(origImpl);
                    continue;
                }

                // Find the corresponding method on the substituted interface
                IMethodSymbol? substitutedMethod = null;

                foreach (var candidate in substitutedIface.GetMembers(origImpl.Name).OfType<IMethodSymbol>())
                {
                    // Match via OriginalDefinition – this is the key
                    if (SymbolEqualityComparer.Default.Equals(candidate.OriginalDefinition, origImpl.OriginalDefinition))
                    {
                        substitutedMethod = candidate;
                        break;
                    }
                }

                builder.Add(substitutedMethod ?? origImpl);
            }

            _explicitInterfaceImplementations = builder.ToImmutable();
            return _explicitInterfaceImplementations.Value;
        }
    }

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _typeParameters;
    public ImmutableArray<ITypeSymbol> TypeArguments => _original.TypeArguments;
    public IMethodSymbol? ConstructedFrom => _original.ConstructedFrom ?? _original;
    public SymbolKind Kind => _original.Kind;
    public string MetadataName => _original.MetadataName;
    public IAssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _original.ContainingModule;
    public INamedTypeSymbol? ContainingType => _constructed;
    public INamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public ISymbol? AssociatedSymbol => _original.AssociatedSymbol;
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
        return new ConstructedMethodSymbol(this, typeArguments.ToImmutableArray(), _constructed);
    }

    internal bool TryGetSubstitutedConstraintTypes(
        ITypeParameterSymbol typeParameter,
        out ImmutableArray<ITypeSymbol> substituted)
    {
        substituted = default;

        if (_typeParameters.IsDefaultOrEmpty ||
            !_typeParameters.Any(tp => TypeParameterSubstitutionComparer.Instance.Equals(tp, typeParameter)))
        {
            return false;
        }

        _constraintTypeMap ??= new Dictionary<ITypeParameterSymbol, ImmutableArray<ITypeSymbol>>(
            TypeParameterSubstitutionComparer.Instance);

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
            builder.Add(_constructed.Substitute(constraint, _methodTypeParameterMap));

        substituted = builder.ToImmutable();
        _constraintTypeMap[typeParameter] = substituted;
        return true;
    }

    internal ConstructorInfo GetConstructorInfo(CodeGenerator codeGen)
    {
        var cacheArguments = _constructed.GetAllTypeArguments();

        if (_original is SourceMethodSymbol cachedSource &&
            codeGen.TryGetMemberBuilder(cachedSource, cacheArguments, out var cachedMember) &&
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

                if (constructedType.GetType().FullName == "System.Reflection.Emit.TypeBuilderInstantiation")
                {
                    var constructedCtor = TypeBuilder.GetConstructor(constructedType, baseCtor);
                    if (constructedCtor is not null)
                        return constructedCtor;
                }

                var parameterTypes = Parameters
                    .Select(parameter => parameter.Type.GetClrType(codeGen))
                    .ToArray();

                var resolved = constructedType.GetConstructor(
                    BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
                    binder: null,
                    types: parameterTypes,
                    modifiers: null);

                if (resolved is not null)
                    return resolved;

                if (constructedType is TypeBuilder typeBuilder)
                {
                    var constructedCtor = TypeBuilder.GetConstructor(typeBuilder, baseCtor);
                    if (constructedCtor is not null)
                        return constructedCtor;
                }

                throw new InvalidOperationException($"Unable to resolve constructed constructor for '{_constructed}' from metadata definition '{_original}'.");
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
                    codeGen.AddMemberBuilder(sourceMethod, constructedCtor, cacheArguments);
                    return constructedCtor;
                }
            }

            throw new InvalidOperationException("Constructor builder not found for source method.");
        }

        throw new Exception("Unexpected method kind");
    }

    internal MethodInfo GetMethodInfo(CodeGenerator codeGen)
    {
        var cacheArguments = _constructed.GetAllTypeArguments();

        if (_original is SourceMethodSymbol cachedSource &&
            codeGen.TryGetMemberBuilder(cachedSource, cacheArguments, out var cachedMember) &&
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
                .Select(x => x.Type.GetClrType(codeGen))
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
                    codeGen.AddMemberBuilder(sourceMethod, constructedMethod, cacheArguments);
                    return constructedMethod;
                }
            }

            if (ReferenceEquals(constructedType, definitionMethod.DeclaringType))
                return definitionMethod;

            var parameterTypes = sourceMethod.Parameters
                .Select(p => p.Type.GetClrType(codeGen))
                .ToArray();

            var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
            var resolved = constructedType.GetMethod(definitionMethod.Name, bindingFlags, null, parameterTypes, null);
            if (resolved is not null)
            {
                codeGen.AddMemberBuilder(sourceMethod, resolved, cacheArguments);
                return resolved;
            }

            throw new MissingMethodException($"Method '{definitionMethod.Name}' with specified parameters not found on constructed type '{constructedType}'.");
        }

        throw new InvalidOperationException("Expected PE or source method symbol.");
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

internal sealed class SubstitutedMethodTypeParameterSymbol : ITypeParameterSymbol
{
    private readonly ITypeParameterSymbol _original;
    private readonly SubstitutedMethodSymbol _containingMethod;
    private readonly ConstructedNamedTypeSymbol _constructed;
    private readonly INamespaceSymbol? _containingNamespace;
    private ImmutableArray<ITypeSymbol>? _constraintTypes;

    public SubstitutedMethodTypeParameterSymbol(
        ITypeParameterSymbol original,
        SubstitutedMethodSymbol containingMethod,
        ConstructedNamedTypeSymbol constructed,
        INamespaceSymbol? containingNamespace)
    {
        _original = original;
        _containingMethod = containingMethod;
        _constructed = constructed;
        _containingNamespace = containingNamespace;
    }

    public string Name => _original.Name;
    public string MetadataName => _original.MetadataName;
    public SymbolKind Kind => _original.Kind;
    public ISymbol? ContainingSymbol => _containingMethod;
    public IAssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _original.ContainingModule;
    public INamedTypeSymbol? ContainingType => _constructed;
    public INamespaceSymbol? ContainingNamespace => _containingNamespace ?? _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => false;
    public ISymbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();

    public int Ordinal => _original.Ordinal;
    public TypeParameterConstraintKind ConstraintKind => _original.ConstraintKind;
    public ImmutableArray<ITypeSymbol> ConstraintTypes
    {
        get
        {
            if (_constraintTypes.HasValue)
                return _constraintTypes.Value;

            if (_containingMethod.TryGetSubstitutedConstraintTypes(_original, out var substituted))
                _constraintTypes = substituted;
            else
                _constraintTypes = _original.ConstraintTypes;

            return _constraintTypes.Value;
        }
    }
    public VarianceKind Variance => _original.Variance;

    public INamedTypeSymbol? BaseType => _original.BaseType;
    public ITypeSymbol? OriginalDefinition => _original.OriginalDefinition ?? _original;
    public SpecialType SpecialType => _original.SpecialType;
    public TypeKind TypeKind => _original.TypeKind;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsReferenceType => _original.IsReferenceType;
    public bool IsValueType => _original.IsValueType;
    public bool IsInterface => _original.IsInterface;
    public bool IsTupleType => _original.IsTupleType;
    public bool IsTypeUnion => _original.IsTypeUnion;
    public bool IsDiscriminatedUnion => _original.IsDiscriminatedUnion;
    public bool IsDiscriminatedUnionCase => _original.IsDiscriminatedUnionCase;
    public INamedTypeSymbol? UnderlyingDiscriminatedUnion => _original.UnderlyingDiscriminatedUnion;
    public ImmutableArray<INamedTypeSymbol> Interfaces => _original.Interfaces;
    public ImmutableArray<INamedTypeSymbol> AllInterfaces => _original.AllInterfaces;
    public ImmutableArray<ISymbol> GetMembers() => ImmutableArray<ISymbol>.Empty;
    public ImmutableArray<ISymbol> GetMembers(string name) => ImmutableArray<ISymbol>.Empty;
    public ITypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = null;
        return false;
    }

    public void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
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

    public bool IsConst => _original.IsConst;
    public bool IsMutable => _original.IsMutable;
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
        var cacheArguments = _constructed.GetAllTypeArguments();

        if (_original is SourceFieldSymbol cachedSource &&
            codeGen.TryGetMemberBuilder(cachedSource, cacheArguments, out var cachedMember) &&
            cachedMember is FieldInfo cachedField)
        {
            return cachedField;
        }

        if (_original is PEFieldSymbol peField)
        {
            var field = peField.GetFieldInfo();
            var constructedType = _constructed.GetTypeInfo(codeGen).AsType();
            var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
            var resolved = constructedType.GetField(field.Name, bindingFlags);
            if (resolved is not null)
                return resolved;

            throw new MissingFieldException(constructedType.FullName, field.Name);
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
                    codeGen.AddMemberBuilder(sourceField, constructedField, cacheArguments);
                    return constructedField;
                }
            }

            if (ReferenceEquals(constructedType, definitionField.DeclaringType))
                return definitionField;

            var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
            var resolved = constructedType.GetField(definitionField.Name, bindingFlags);
            if (resolved is not null)
            {
                codeGen.AddMemberBuilder(sourceField, resolved, cacheArguments);
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
    private ImmutableArray<IPropertySymbol>? _explicitInterfaceImplementations;

    public SubstitutedPropertySymbol(IPropertySymbol original, ConstructedNamedTypeSymbol constructed)
    {
        _original = original;
        _constructed = constructed;
    }

    public string Name => _original.Name;
    public ITypeSymbol Type => _constructed.Substitute(_original.Type);
    public ISymbol ContainingSymbol => _constructed;
    public IPropertySymbol? OriginalDefinition { get; }
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

    public ImmutableArray<IPropertySymbol> ExplicitInterfaceImplementations
    {
        get
        {
            if (_explicitInterfaceImplementations.HasValue)
                return _explicitInterfaceImplementations.Value;

            var originals = _original.ExplicitInterfaceImplementations;

            if (originals.IsDefaultOrEmpty || originals.Length == 0)
            {
                _explicitInterfaceImplementations = originals;
                return originals;
            }

            var builder = ImmutableArray.CreateBuilder<IPropertySymbol>(originals.Length);

            foreach (var origImpl in originals)
            {
                var origIface = (INamedTypeSymbol?)origImpl.ContainingType;
                if (origIface is null)
                {
                    builder.Add(origImpl);
                    continue;
                }

                // Substitute the interface type with the constructed type’s substitution
                var substitutedIface = _constructed.Substitute(origIface) as INamedTypeSymbol;

                // If nothing changed, just reuse the original property
                if (substitutedIface is null ||
                    SymbolEqualityComparer.Default.Equals(origIface, substitutedIface))
                {
                    builder.Add(origImpl);
                    continue;
                }

                // Find the corresponding property on the substituted interface
                IPropertySymbol? substitutedProperty = null;

                foreach (var candidate in substitutedIface.GetMembers(origImpl.Name).OfType<IPropertySymbol>())
                {
                    // Match via OriginalDefinition – this is the key
                    if (SymbolEqualityComparer.Default.Equals(candidate.OriginalDefinition, origImpl.OriginalDefinition))
                    {
                        substitutedProperty = candidate;
                        break;
                    }
                }

                builder.Add(substitutedProperty ?? origImpl);
            }

            _explicitInterfaceImplementations = builder.ToImmutable();
            return _explicitInterfaceImplementations.Value;
        }
    }

    public void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
}

internal sealed class SubstitutedParameterSymbol : IParameterSymbol
{
    private readonly IParameterSymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructed;
    private readonly Dictionary<ITypeParameterSymbol, ITypeParameterSymbol>? _methodMap;

    public SubstitutedParameterSymbol(
        IParameterSymbol original,
        ConstructedNamedTypeSymbol constructed,
        Dictionary<ITypeParameterSymbol, ITypeParameterSymbol>? methodMap = null)
    {
        _original = original;
        _constructed = constructed;
        _methodMap = methodMap;
    }

    public string Name => _original.Name;
    public ITypeSymbol Type => _constructed.Substitute(_original.Type, _methodMap);

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

internal sealed class TypeParameterSubstitutionComparer : IEqualityComparer<ITypeParameterSymbol>
{
    public static readonly TypeParameterSubstitutionComparer Instance = new();

    private TypeParameterSubstitutionComparer() { }

    public bool Equals(ITypeParameterSymbol? x, ITypeParameterSymbol? y)
    {
        if (ReferenceEquals(x, y))
            return true;
        if (x is null || y is null)
            return false;

        var defX = (ITypeParameterSymbol)(x.OriginalDefinition ?? x);
        var defY = (ITypeParameterSymbol)(y.OriginalDefinition ?? y);

        return SymbolEqualityComparer.Default.IgnoreContainingNamespaceOrType().Equals(defX, defY);
    }

    public int GetHashCode(ITypeParameterSymbol obj)
    {
        var def = (ITypeParameterSymbol)(obj.OriginalDefinition ?? obj);
        return SymbolEqualityComparer.Default.GetHashCode(def);
    }
}
