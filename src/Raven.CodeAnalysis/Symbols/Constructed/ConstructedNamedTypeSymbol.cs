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
    private ImmutableArray<ISymbol>? _members;
    private ImmutableArray<INamedTypeSymbol>? _interfaces;
    private ImmutableArray<INamedTypeSymbol>? _allInterfaces;

    public ImmutableArray<ITypeSymbol> TypeArguments { get; }

    public ConstructedNamedTypeSymbol(INamedTypeSymbol originalDefinition, ImmutableArray<ITypeSymbol> typeArguments)
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

    public INamedTypeSymbol UnderlyingTupleType => throw new NotImplementedException();

    public ImmutableArray<IFieldSymbol> TupleElements => throw new NotImplementedException();

    public void Accept(SymbolVisitor visitor) => visitor.VisitNamedType(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamedType(this);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments) => throw new NotSupportedException();

    public ITypeSymbol? LookupType(string name)
    {
        throw new NotImplementedException();
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        throw new NotImplementedException();
    }

    internal System.Reflection.TypeInfo GetTypeInfo(CodeGenerator codeGen)
    {
        if (_originalDefinition is PENamedTypeSymbol pen)
        {
            var genericTypeDef = pen.GetClrType(codeGen);
            return genericTypeDef.MakeGenericType(TypeArguments.Select(x => x.GetClrType(codeGen)).ToArray()).GetTypeInfo();
        }

        if (_originalDefinition is SourceNamedTypeSymbol source)
        {
            var definitionType = codeGen.GetTypeBuilder(source) ?? throw new InvalidOperationException("Missing type builder for generic definition.");
            var runtimeArgs = TypeArguments.Select(x => x.GetClrType(codeGen)).ToArray();
            var constructed = definitionType.MakeGenericType(runtimeArgs);
            return constructed.GetTypeInfo();
        }

        throw new InvalidOperationException("ConstructedNamedTypeSymbol is not based on a supported symbol type.");
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

    internal ConstructorInfo GetConstructorInfo(CodeGenerator codeGen)
    {
        if (_original is PEMethodSymbol peMethod)
        {
            var baseCtor = peMethod.GetConstructorInfo();

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
                return TypeBuilder.GetConstructor(constructedType, definitionCtor);
            }

            throw new InvalidOperationException("Constructor builder not found for source method.");
        }

        throw new Exception("Unexpected method kind");
    }

    internal MethodInfo GetMethodInfo(CodeGenerator codeGen)
    {
        if (_original is PEMethodSymbol peMethod)
        {
            var baseMethod = peMethod.GetMethodInfo();

            // Resolve the constructed runtime type
            var constructedType = _constructed.GetTypeInfo(codeGen).AsType();

            // Use metadata name and parameter types to resolve the method on the constructed type
            var parameterTypes = Parameters.Select(x => x.Type.GetClrType(codeGen)).ToArray();
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
                    return constructedMethod;
            }

            if (ReferenceEquals(constructedType, definitionMethod.DeclaringType))
                return definitionMethod;

            var parameterTypes = sourceMethod.Parameters
                .Select(p => p.Type.GetClrType(codeGen))
                .ToArray();

            var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
            var resolved = constructedType.GetMethod(definitionMethod.Name, bindingFlags, null, parameterTypes, null);
            if (resolved is not null)
                return resolved;

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
                    return constructedField;
            }

            if (ReferenceEquals(constructedType, definitionField.DeclaringType))
                return definitionField;

            var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
            var resolved = constructedType.GetField(definitionField.Name, bindingFlags);
            if (resolved is not null)
                return resolved;

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
