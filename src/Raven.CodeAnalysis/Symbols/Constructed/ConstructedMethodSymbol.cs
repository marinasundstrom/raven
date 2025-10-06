using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

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

        _substitutionMap = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(typeParameters.Length);
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

        var containingClrType = containingType.GetClrType(codeGen);
        var expectedParameterTypes = Parameters
            .Select(parameter => parameter.Type.GetClrType(codeGen))
            .ToArray();
        var returnTypeSymbol = ReturnType;
        var expectedReturnType = returnTypeSymbol.GetClrTypeTreatingUnitAsVoid(codeGen);
        var runtimeTypeArguments = TypeArguments
            .Select(argument => argument.GetClrType(codeGen))
            .ToArray();

        const BindingFlags Flags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;
        foreach (var method in containingClrType.GetMethods(Flags))
        {
            if (!string.Equals(method.Name, _definition.Name, StringComparison.Ordinal))
                continue;

            if (method.IsGenericMethodDefinition != _definition.IsGenericMethod)
            {
                if (!method.IsGenericMethodDefinition)
                    continue;
            }

            MethodInfo candidate = method;

            if (method.IsGenericMethodDefinition)
            {
                if (method.GetGenericArguments().Length != runtimeTypeArguments.Length)
                    continue;

                candidate = method.MakeGenericMethod(runtimeTypeArguments);
            }
            else if (method.ContainsGenericParameters)
            {
                continue;
            }

            var candidateParameters = candidate.GetParameters();
            if (candidateParameters.Length != expectedParameterTypes.Length)
                continue;

            var parametersMatch = true;
            for (var i = 0; i < candidateParameters.Length; i++)
            {
                if (!TypesEquivalent(candidateParameters[i].ParameterType, expectedParameterTypes[i]))
                {
                    parametersMatch = false;
                    break;
                }
            }

            if (!parametersMatch)
                continue;

            if (!ReturnTypesEquivalent(candidate.ReturnType, expectedReturnType, returnTypeSymbol, codeGen))
                continue;

            return candidate;
        }

        throw new InvalidOperationException($"Unable to resolve constructed method '{_definition.Name}'.");
    }

    private static bool ReturnTypesEquivalent(Type runtimeReturnType, Type expectedReturnType, ITypeSymbol returnTypeSymbol, CodeGen.CodeGenerator codeGen)
    {
        if (returnTypeSymbol.SpecialType == SpecialType.System_Unit)
        {
            if (runtimeReturnType == typeof(void))
                return true;

            if (codeGen.UnitType is not null)
                return TypesEquivalent(runtimeReturnType, codeGen.UnitType);

            return false;
        }

        return TypesEquivalent(runtimeReturnType, expectedReturnType);
    }

    private static bool TypesEquivalent(Type left, Type right)
    {
        if (left == right)
            return true;

        if (left.IsByRef || right.IsByRef)
        {
            if (left.IsByRef != right.IsByRef)
                return false;

            return TypesEquivalent(left.GetElementType()!, right.GetElementType()!);
        }

        if (left.IsPointer || right.IsPointer)
        {
            if (left.IsPointer != right.IsPointer)
                return false;

            return TypesEquivalent(left.GetElementType()!, right.GetElementType()!);
        }

        if (left.IsArray || right.IsArray)
        {
            if (left.IsArray != right.IsArray || left.GetArrayRank() != right.GetArrayRank())
                return false;

            return TypesEquivalent(left.GetElementType()!, right.GetElementType()!);
        }

        if (left.IsGenericType || right.IsGenericType)
        {
            if (left.IsGenericType != right.IsGenericType)
                return false;

            var leftDefinition = left.IsGenericTypeDefinition ? left : left.GetGenericTypeDefinition();
            var rightDefinition = right.IsGenericTypeDefinition ? right : right.GetGenericTypeDefinition();

            if (!TypesEquivalent(leftDefinition, rightDefinition))
                return false;

            if (!left.IsGenericTypeDefinition)
            {
                var leftArguments = left.GetGenericArguments();
                var rightArguments = right.GetGenericArguments();

                if (leftArguments.Length != rightArguments.Length)
                    return false;

                for (var i = 0; i < leftArguments.Length; i++)
                {
                    if (!TypesEquivalent(leftArguments[i], rightArguments[i]))
                        return false;
                }
            }

            return true;
        }

        return string.Equals(left.FullName, right.FullName, StringComparison.Ordinal) &&
               string.Equals(left.Assembly.FullName, right.Assembly.FullName, StringComparison.Ordinal);
    }
}
