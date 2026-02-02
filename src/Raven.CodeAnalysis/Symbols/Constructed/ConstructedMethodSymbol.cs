using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis;

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

        _substitutionMap = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(typeParameters.Length, SymbolEqualityComparer.Default);
        for (int i = 0; i < typeParameters.Length; i++)
        {
            var canonical = CanonicalizeTypeParameter(typeParameters[i]);
            _substitutionMap[canonical] = typeArguments[i];
        }
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

    // Helper methods for chain-aware substitution of nested types (matches ConstructedNamedTypeSymbol behavior)
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

        if (!SymbolEqualityComparer.Default.Equals(substitutedContaining, containing))
        {
            containingOverride = substitutedContaining;
            return true;
        }

        return false;
    }

    private ITypeSymbol Substitute(ITypeSymbol type)
    {
        if (type is ITypeParameterSymbol tp)
        {
            tp = CanonicalizeTypeParameter(tp);
            if (_substitutionMap.TryGetValue(tp, out var replacement))
                return replacement;
        }

        if (type is NullableTypeSymbol nullableTypeSymbol)
        {
            var underlyingType = Substitute(nullableTypeSymbol.UnderlyingType);

            if (!SymbolEqualityComparer.Default.Equals(underlyingType, nullableTypeSymbol.UnderlyingType))
                return underlyingType.MakeNullable();

            return type;
        }

        if (type is ByRefTypeSymbol byRef)
        {
            var substitutedElement = Substitute(byRef.ElementType);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, byRef.ElementType))
                return new ByRefTypeSymbol(substitutedElement);

            return type;
        }

        if (type is IAddressTypeSymbol address)
        {
            var substitutedElement = Substitute(address.ReferencedType);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, address.ReferencedType))
                return new AddressTypeSymbol(substitutedElement);

            return type;
        }

        if (type is IArrayTypeSymbol arrayType)
        {
            var substitutedElement = Substitute(arrayType.ElementType);

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
                var substitutedArg = Substitute(originalArg);

                substitutedArgs[i] = substitutedArg;

                if (!SymbolEqualityComparer.Default.Equals(substitutedArg, originalArg))
                    changed = true;
            }

            if (!changed)
            {
                // Even if type arguments did not change, nested types may need re-anchoring
                // under a substituted containing type.
                if (named.ContainingType is INamedTypeSymbol && TryGetContainingOverride(named, out var containingOverride) && containingOverride is not null)
                {
                    var constructedFromSame = (INamedTypeSymbol?)named.ConstructedFrom ?? named;
                    return ConstructedNamedTypeSymbol.ReanchorNested(
                        constructedFromSame,
                        containingOverride,
                        inheritedSubstitution: null,
                        typeArguments: named.TypeArguments);
                }

                return named;
            }

            // Avoid reusing a possibly already-constructed named
            var constructedFrom = (INamedTypeSymbol?)named.ConstructedFrom ?? named;

            if (named.ContainingType is INamedTypeSymbol && TryGetContainingOverride(named, out var overrideContaining) && overrideContaining is not null)
            {
                var immutableArguments = ImmutableArray.Create(substitutedArgs);
                return ConstructedNamedTypeSymbol.ReanchorNested(
                    constructedFrom,
                    overrideContaining,
                    inheritedSubstitution: null,
                    typeArguments: immutableArguments);
            }

            return constructedFrom.Construct(substitutedArgs);
        }

        // Nested non-generic named types (e.g. Result<T,E>.Ok) must still be re-anchored under a substituted containing type.
        if (type is INamedTypeSymbol nestedNamed && nestedNamed.ContainingType is INamedTypeSymbol)
        {
            if (TryGetContainingOverride(nestedNamed, out var containingOverride) && containingOverride is not null)
            {
                return ConstructedNamedTypeSymbol.ReanchorNested(
                    nestedNamed,
                    containingOverride,
                    inheritedSubstitution: null,
                    typeArguments: ImmutableArray<ITypeSymbol>.Empty);
            }
        }

        return type;
    }

    private ITypeParameterSymbol CanonicalizeTypeParameter(ITypeParameterSymbol typeParameter)
    {
        if (typeParameter.ContainingSymbol is IMethodSymbol &&
            typeParameter.Ordinal >= 0 &&
            typeParameter.Ordinal < _definition.TypeParameters.Length)
        {
            return _definition.TypeParameters[typeParameter.Ordinal];
        }

        return typeParameter;
    }

    [DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
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
        public bool IsMutable => _original.IsMutable;
        public bool HasExplicitDefaultValue => _original.HasExplicitDefaultValue;
        public object? ExplicitDefaultValue => _original.ExplicitDefaultValue;

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

    internal MethodInfo GetMethodInfo(CodeGen.CodeGenerator codeGen)
    {
        if (codeGen is null)
            throw new ArgumentNullException(nameof(codeGen));

        var containingType = _containingType ?? _definition.ContainingType
            ?? throw new InvalidOperationException("Constructed method is missing a containing type.");

        var containingClrType = containingType.GetClrTypeTreatingUnitAsVoid(codeGen);
        var isTypeBuilderInstantiation = string.Equals(
            containingClrType.GetType().FullName,
            "System.Reflection.Emit.TypeBuilderInstantiation",
            StringComparison.Ordinal);
        var methodSearchType = isTypeBuilderInstantiation
            ? containingClrType.GetGenericTypeDefinition()
            : containingClrType;
        var parameterSymbols = Parameters;
        var returnTypeSymbol = ReturnType;
        var debug = ConstructedMethodDebugging.IsEnabled();
        var typeArguments = TypeArguments.IsDefault ? ImmutableArray<ITypeSymbol>.Empty : TypeArguments;
        var runtimeTypeArguments = typeArguments
            .Select(argument => GetProjectedRuntimeType(argument, codeGen, treatUnitAsVoid: false))
            .ToArray();

        if (methodSearchType is TypeBuilder && _definition is SourceMethodSymbol sourceMethod)
        {
            var methodBuilder = (MethodBuilder)codeGen.GetMemberBuilder(sourceMethod);
            return methodBuilder.IsGenericMethodDefinition
                ? methodBuilder.MakeGenericMethod(runtimeTypeArguments)
                : methodBuilder;
        }

        if (debug)
        {
            static string FormatType(Type type)
            {
                if (type is null)
                    return "<null>";

                var formatted = $"{type} (gp={type.IsGenericParameter})";

                if (!type.IsGenericType)
                    return formatted;

                var args = type.GetGenericArguments();
                var formattedArgs = string.Join(", ", args.Select(FormatType));
                return $"{formatted}[{formattedArgs}]";
            }

            for (var i = 0; i < TypeArguments.Length && i < runtimeTypeArguments.Length; i++)
            {
                var runtimeArg = runtimeTypeArguments[i];
                var symbolArg = TypeArguments[i];
                var symbolDescription = symbolArg is INamedTypeSymbol named
                    ? $"{named.ConstructedFrom}<{string.Join(", ", named.TypeArguments.Select(a => $"{a} (kind={a.TypeKind})"))}>"
                    : symbolArg.ToString();
                Console.Error.WriteLine($"[ConstructedMethodSymbol] Type argument {i}: symbol={symbolDescription} runtime={FormatType(runtimeArg)} containsGP={runtimeArg.ContainsGenericParameters}");
            }
        }

        for (var i = 0; i < TypeArguments.Length && i < runtimeTypeArguments.Length; i++)
        {
            if (TypeArguments[i] is ITypeParameterSymbol { ContainingSymbol: IMethodSymbol methodSymbol } methodTypeParameter &&
                runtimeTypeArguments[i] is Type runtimeArgument &&
                runtimeArgument.IsGenericParameter &&
                runtimeArgument.DeclaringMethod is null &&
                TryGetMethodGenericParameter(methodSymbol, methodTypeParameter.Ordinal, codeGen, out var remapped))
            {
                runtimeTypeArguments[i] = remapped;
            }
        }

        runtimeTypeArguments = NormalizeStateMachineRuntimeTypes(runtimeTypeArguments, codeGen);

        if (TryResolveFromCachedDefinition(
                codeGen,
                containingClrType,
                isTypeBuilderInstantiation,
                parameterSymbols,
                returnTypeSymbol,
                runtimeTypeArguments,
                out var cached,
                debug))
        {
            return cached;
        }

        const BindingFlags Flags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;
        foreach (var method in methodSearchType.GetMethods(Flags))
        {
            MethodInfo candidate = method;

            if (isTypeBuilderInstantiation)
            {
                try
                {
                    var instantiated = TypeBuilder.GetMethod(containingClrType, method);
                    if (instantiated is null)
                        continue;

                    candidate = instantiated;
                }
                catch (ArgumentException)
                {
                    continue;
                }
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

            var parametersMatch = ParametersMatch(candidateParameters, parameterSymbols, methodRuntimeArguments, typeRuntimeArguments, codeGen, debug);
            if (!parametersMatch)
            {
                if (debug)
                {
                    Console.Error.WriteLine($"  Rejected candidate {candidate} due to parameter mismatch.");
                    Console.Error.WriteLine($"    Candidate params: {string.Join(", ", candidateParameters.Select(p => p.ParameterType))}");
                }
                continue;
            }

            var normalizedReturnType = SubstituteRuntimeType(candidate.ReturnType, methodRuntimeArguments, typeRuntimeArguments);
            if (!MethodSymbolExtensionsForCodeGen.ReturnTypesMatch(normalizedReturnType, returnTypeSymbol, codeGen))
            {
                if (debug)
                {
                    Console.Error.WriteLine($"  Rejected candidate {candidate} due to return type mismatch: {normalizedReturnType} vs {returnTypeSymbol}.");
                }
                continue;
            }

            return candidate;
        }

        if (debug)
        {
            Console.Error.WriteLine($"[ConstructedMethodSymbol] Unable to resolve '{_definition}' on '{containingClrType}'.");
            Console.Error.WriteLine($"  Parameters: {string.Join(", ", parameterSymbols.Select(p => p.Type.ToString()))}");
            Console.Error.WriteLine($"  Type arguments: {string.Join(", ", TypeArguments.Select(a => a.ToString()))}");
            Console.Error.WriteLine($"  Runtime type arguments: {string.Join(", ", runtimeTypeArguments.Select(t => t?.FullName ?? t?.ToString() ?? "<null>"))}");
        }

        throw new InvalidOperationException($"Unable to resolve constructed method '{_definition.Name}'.");
    }

    private bool TryResolveFromCachedDefinition(
        CodeGen.CodeGenerator codeGen,
        Type containingClrType,
        bool isTypeBuilderInstantiation,
        ImmutableArray<IParameterSymbol> parameterSymbols,
        ITypeSymbol returnTypeSymbol,
        Type[] runtimeTypeArguments,
        out MethodInfo methodInfo,
        bool debug)
    {
        methodInfo = null!;

        if (!TryGetSourceDefinitionSymbol(_definition, out var sourceDefinition))
            return false;

        if (!codeGen.TryGetMemberBuilder(sourceDefinition, TypeArguments, out var member) ||
            member is not MethodInfo definitionMethod)
        {
            if (!codeGen.TryGetMemberBuilder(sourceDefinition, out member) ||
                member is not MethodInfo definitionMethodFromDefinition)
            {
                return false;
            }

            definitionMethod = definitionMethodFromDefinition;
        }

        var candidateDefinition = definitionMethod;

        if (candidateDefinition.IsGenericMethod && !candidateDefinition.IsGenericMethodDefinition)
            candidateDefinition = candidateDefinition.GetGenericMethodDefinition();

        if (isTypeBuilderInstantiation)
        {
            try
            {
                var projected = TypeBuilder.GetMethod(containingClrType, candidateDefinition);
                if (projected is null)
                    return false;
                candidateDefinition = projected;
            }
            catch (ArgumentException)
            {
                return false;
            }
        }

        var candidate = candidateDefinition;

        if (candidate.IsGenericMethodDefinition)
        {
            if (runtimeTypeArguments.Length != candidate.GetGenericArguments().Length)
                return false;

            candidate = candidate.MakeGenericMethod(runtimeTypeArguments);
        }

        if (candidate is MethodBuilder ||
            string.Equals(candidate.GetType().FullName, "System.Reflection.Emit.MethodBuilderInstantiation", StringComparison.Ordinal))
        {
            codeGen.AddMemberBuilder(sourceDefinition, candidate, TypeArguments);
            methodInfo = candidate;
            return true;
        }

        var candidateParameters = candidate.GetParameters();
        var methodRuntimeArguments = candidate.IsGenericMethod
            ? candidate.GetGenericArguments()
            : Array.Empty<Type>();
        var typeRuntimeArguments = candidate.DeclaringType is not null && candidate.DeclaringType.IsGenericType
            ? candidate.DeclaringType.GetGenericArguments()
            : Array.Empty<Type>();

        if (!ParametersMatch(candidateParameters, parameterSymbols, methodRuntimeArguments, typeRuntimeArguments, codeGen, debug))
            return false;

        var normalizedReturnType = SubstituteRuntimeType(candidate.ReturnType, methodRuntimeArguments, typeRuntimeArguments);
        if (!MethodSymbolExtensionsForCodeGen.ReturnTypesMatch(normalizedReturnType, returnTypeSymbol, codeGen))
            return false;

        methodInfo = candidate;
        codeGen.AddMemberBuilder(sourceDefinition, candidate, TypeArguments);
        return true;
    }

    private static bool TryGetSourceDefinitionSymbol(IMethodSymbol methodSymbol, out SourceSymbol sourceSymbol)
    {
        switch (methodSymbol)
        {
            case SourceSymbol source:
                sourceSymbol = source;
                return true;
            case IAliasSymbol alias when alias.UnderlyingSymbol is IMethodSymbol underlyingMethod:
                return TryGetSourceDefinitionSymbol(underlyingMethod, out sourceSymbol);
            default:
                {
                    var originalDefinition = methodSymbol.OriginalDefinition;
                    if (originalDefinition is not null && !ReferenceEquals(originalDefinition, methodSymbol) &&
                        TryGetSourceDefinitionSymbol(originalDefinition, out sourceSymbol))
                    {
                        return true;
                    }

                    var constructedFrom = methodSymbol.ConstructedFrom;
                    if (constructedFrom is not null && !ReferenceEquals(constructedFrom, methodSymbol) &&
                        TryGetSourceDefinitionSymbol(constructedFrom, out sourceSymbol))
                    {
                        return true;
                    }

                    sourceSymbol = null!;
                    return false;
                }
        }
    }

    private static bool TryGetMethodGenericParameter(
        IMethodSymbol methodSymbol,
        int ordinal,
        CodeGen.CodeGenerator codeGen,
        out Type parameter)
    {
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

    private Type[] NormalizeStateMachineRuntimeTypes(Type[] runtimeTypeArguments, CodeGen.CodeGenerator codeGen)
    {
        if (runtimeTypeArguments.Length == 0)
            return runtimeTypeArguments;

        var normalized = new Type[runtimeTypeArguments.Length];
        var updated = false;

        for (var i = 0; i < runtimeTypeArguments.Length; i++)
        {
            var symbolArgument = TypeArguments.Length > i ? TypeArguments[i] : null;
            var argument = runtimeTypeArguments[i];
            var normalizedArgument = NormalizeStateMachineRuntimeType(symbolArgument, argument, codeGen);
            normalized[i] = normalizedArgument;

            if (!ReferenceEquals(argument, normalizedArgument))
                updated = true;
        }

        return updated ? normalized : runtimeTypeArguments;
    }

    private Type NormalizeStateMachineRuntimeType(ITypeSymbol? symbolArgument, Type runtimeArgument, CodeGen.CodeGenerator codeGen)
    {
        if (runtimeArgument is null)
            return runtimeArgument;

        if (symbolArgument is ConstructedNamedTypeSymbol constructed &&
            constructed.ConstructedFrom is SynthesizedAsyncStateMachineTypeSymbol stateMachine &&
            runtimeArgument.IsGenericType)
        {
            var asyncRuntimeParameters = new Type[stateMachine.TypeParameters.Length];

            foreach (var mapping in stateMachine.TypeParameterMappings)
            {
                if (TryGetMethodGenericParameter(stateMachine.AsyncMethod, mapping.AsyncParameter.Ordinal, codeGen, out var asyncRuntime))
                {
                    asyncRuntimeParameters[mapping.StateMachineParameter.Ordinal] = asyncRuntime;
                    continue;
                }

                if (codeGen.TryGetRuntimeTypeForTypeParameter(mapping.AsyncParameter, out var asyncResolved))
                    asyncRuntimeParameters[mapping.StateMachineParameter.Ordinal] = asyncResolved;
            }

            runtimeArgument = SubstituteStateMachineRuntimeGenerics(runtimeArgument, asyncRuntimeParameters);
        }

        return SubstituteRuntimeTypeUsingSymbol(runtimeArgument, symbolArgument, codeGen);
    }

    private Type SubstituteRuntimeTypeUsingSymbol(Type runtimeType, ITypeSymbol? symbolArgument, CodeGen.CodeGenerator codeGen)
    {
        if (symbolArgument is null)
            return runtimeType;

        if (runtimeType.IsByRef)
        {
            var symbolElement = (symbolArgument as ByRefTypeSymbol)?.ElementType ?? symbolArgument;
            var substitutedElement = SubstituteRuntimeTypeUsingSymbol(runtimeType.GetElementType()!, symbolElement, codeGen);
            return substitutedElement.MakeByRefType();
        }

        if (runtimeType.IsPointer)
        {
            var symbolElement = (symbolArgument as IPointerTypeSymbol)?.PointedAtType ?? symbolArgument;
            var substitutedElement = SubstituteRuntimeTypeUsingSymbol(runtimeType.GetElementType()!, symbolElement, codeGen);
            return substitutedElement.MakePointerType();
        }

        if (runtimeType.IsArray)
        {
            var symbolElement = (symbolArgument as IArrayTypeSymbol)?.ElementType ?? symbolArgument;
            var substitutedElement = SubstituteRuntimeTypeUsingSymbol(runtimeType.GetElementType()!, symbolElement, codeGen);
            return runtimeType.GetArrayRank() == 1
                ? substitutedElement.MakeArrayType()
                : substitutedElement.MakeArrayType(runtimeType.GetArrayRank());
        }

        if (runtimeType.IsGenericParameter)
        {
            return GetProjectedRuntimeType(symbolArgument, codeGen, treatUnitAsVoid: false, isTopLevel: false);
        }

        if (runtimeType.IsGenericType && symbolArgument is INamedTypeSymbol named && named.IsGenericType)
        {
            var definition = runtimeType.IsGenericTypeDefinition
                ? runtimeType
                : runtimeType.GetGenericTypeDefinition();

            var runtimeArguments = runtimeType.GetGenericArguments();
            var symbolArguments = named.TypeArguments;
            var substitutedArguments = new Type[runtimeArguments.Length];
            var changed = false;

            for (var i = 0; i < runtimeArguments.Length; i++)
            {
                var symbolArg = i < symbolArguments.Length ? symbolArguments[i] : null;
                substitutedArguments[i] = SubstituteRuntimeTypeUsingSymbol(runtimeArguments[i], symbolArg, codeGen);

                if (!ReferenceEquals(substitutedArguments[i], runtimeArguments[i]))
                    changed = true;
            }

            if (!changed)
                return runtimeType;

            return definition.MakeGenericType(substitutedArguments);
        }

        return runtimeType;
    }

    private static Type SubstituteStateMachineRuntimeGenerics(Type runtimeType, Type[] asyncRuntimeParameters)
    {
        if (runtimeType.IsGenericParameter)
        {
            var position = runtimeType.GenericParameterPosition;
            if ((uint)position < (uint)asyncRuntimeParameters.Length && asyncRuntimeParameters[position] is Type mapped)
                return mapped;

            return runtimeType;
        }

        if (!runtimeType.IsGenericType)
            return runtimeType;

        var definition = runtimeType.IsGenericTypeDefinition ? runtimeType : runtimeType.GetGenericTypeDefinition();
        var arguments = runtimeType.GetGenericArguments();
        var replaced = false;

        for (var i = 0; i < arguments.Length; i++)
        {
            var substituted = SubstituteStateMachineRuntimeGenerics(arguments[i], asyncRuntimeParameters);
            if (!ReferenceEquals(substituted, arguments[i]))
            {
                arguments[i] = substituted;
                replaced = true;
            }
        }

        if (!replaced)
            return runtimeType;

        return definition.MakeGenericType(arguments);
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

    private bool ParametersMatch(
        ParameterInfo[] runtimeParameters,
        ImmutableArray<IParameterSymbol> parameterSymbols,
        Type[] methodRuntimeArguments,
        Type[]? typeRuntimeArguments,
        CodeGen.CodeGenerator codeGen,
        bool debug)
    {
        if (runtimeParameters.Length != parameterSymbols.Length)
            return false;

        for (var i = 0; i < runtimeParameters.Length; i++)
        {
            if (!ParameterMatches(runtimeParameters[i], parameterSymbols[i], methodRuntimeArguments, typeRuntimeArguments, codeGen))
            {
                if (debug)
                {
                    var normalized = SubstituteRuntimeType(runtimeParameters[i].ParameterType, methodRuntimeArguments, typeRuntimeArguments);
                    var expected = parameterSymbols[i].Type.GetClrTypeTreatingUnitAsVoid(codeGen);
                    Console.Error.WriteLine($"    Parameter mismatch at {i}: runtime={normalized} expected={expected} symbol={parameterSymbols[i].Type}");
                }

                return false;
            }
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
        var equivalent = MethodSymbolExtensionsForCodeGen.TypesEquivalent(normalizedRuntimeType, symbolParameter.Type, codeGen);

        if (!equivalent)
        {
            var symbolClrType = symbolParameter.Type.GetClrType(codeGen);
            if (string.Equals(normalizedRuntimeType.ToString(), symbolClrType.ToString(), StringComparison.Ordinal))
                equivalent = true;
        }

        return equivalent;
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

            if (typeParameter.ContainingType is SynthesizedAsyncStateMachineTypeSymbol stateMachine &&
                stateMachine.TryMapToAsyncMethodTypeParameter(typeParameter, out var asyncParameter) &&
                codeGen.TryGetRuntimeTypeForTypeParameter(asyncParameter, out var asyncRuntimeType))
            {
                return asyncRuntimeType;
            }

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
            return nullable.UnderlyingType.IsValueType
                ? typeof(Nullable<>).MakeGenericType(underlying)
                : underlying;
        }

        if (symbol is LiteralTypeSymbol literal)
            return GetProjectedRuntimeType(literal.UnderlyingType, codeGen, treatUnitAsVoid, isTopLevel: false);

        if (symbol is ITupleTypeSymbol tuple)
        {
            var elementClrTypes = tuple.TupleElements
                .Select(element => GetProjectedRuntimeType(element.Type, codeGen, treatUnitAsVoid, isTopLevel: false))
                .ToArray();

            return TypeSymbolExtensionsForCodeGen.GetValueTupleClrType(elementClrTypes, codeGen.Compilation);
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

internal static class ConstructedMethodDebugging
{
    public static bool IsEnabled()
    {
        var value = Environment.GetEnvironmentVariable("RAVEN_DEBUG_CONSTRUCTED_METHOD");
        return string.Equals(value, "1", StringComparison.Ordinal);
    }
}
