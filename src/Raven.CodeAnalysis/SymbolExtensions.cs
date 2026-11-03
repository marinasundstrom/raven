using System.Collections.Immutable;
using System.IO.Pipelines;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public enum ExtensionMemberKind
{
    None,
    Instance,
    Static
}

public static partial class SymbolExtensions
{
    internal static string GetLookupIdentityKey(this Symbol symbol)
    {
        var underlying = symbol.UnderlyingSymbol;

        return underlying switch
        {
            MethodSymbol method => GetMethodLookupIdentityKey(method),
            PropertySymbol property => GetPropertyLookupIdentityKey(property),
            TypeSymbol type => GetTypeLookupIdentityKey(type),
            _ => $"{underlying.Kind}:{underlying.MetadataName}"
        };
    }

    internal static string GetShallowLookupIdentityKey(this Symbol symbol)
    {
        var underlying = symbol.UnderlyingSymbol;

        return underlying switch
        {
            MethodSymbol method => GetMethodShallowLookupIdentityKey(method),
            PropertySymbol property => GetPropertyShallowLookupIdentityKey(property),
            TypeSymbol type => GetTypeLookupIdentityKey(type),
            _ => $"{underlying.Kind}:{underlying.MetadataName}"
        };
    }

    private static string GetMethodShallowLookupIdentityKey(MethodSymbol method)
    {
        var definition = UnwrapMethodDefinition(method);
        var typeArguments = method.TypeArguments.IsDefaultOrEmpty
            ? string.Empty
            : string.Join(",", method.TypeArguments.Select(static type => GetTypeLookupIdentityKey(type)));

        if (definition is IMethodLookupIdentity identity)
            return $"MP:{identity.ShallowDeclarationLookupKey}|{method.Arity}|{typeArguments}";

        var containingType = GetTypeLookupIdentityKey(definition.ContainingType);
        var parameters = string.Join(",", definition.Parameters.Select(static p => $"{p.RefKind}:{GetTypeLookupIdentityKey(p.Type)}"));
        return $"M:{containingType}|{definition.MetadataName}|{method.Arity}|{typeArguments}|{parameters}";
    }

    private static MethodSymbol UnwrapMethodDefinition(MethodSymbol method)
    {
        if (method is ConstructedMethodSymbol constructedMethod)
            return UnwrapMethodDefinition(constructedMethod.Definition);

        return method.OriginalDefinition ?? method;
    }

    private static string GetPropertyShallowLookupIdentityKey(PropertySymbol property)
    {
        var definition = property.OriginalDefinition ?? property;
        var containingType = GetTypeLookupIdentityKey(definition.ContainingType);
        var parameters = string.Join(",", definition.Parameters.Select(static p => $"{p.RefKind}:{GetTypeLookupIdentityKey(p.Type)}"));
        return $"P:{containingType}|{definition.MetadataName}|{parameters}";
    }

    private static string GetMethodLookupIdentityKey(MethodSymbol method)
    {
        var definition = method.OriginalDefinition ?? method;
        var containingType = GetTypeLookupIdentityKey(definition.ContainingType);
        var parameters = string.Join(",", method.Parameters.Select(static p => $"{p.RefKind}:{GetTypeLookupIdentityKey(p.Type)}"));
        var typeArguments = method.TypeArguments.IsDefaultOrEmpty
            ? string.Empty
            : string.Join(",", method.TypeArguments.Select(static type => GetTypeLookupIdentityKey(type)));
        var returnType = GetTypeLookupIdentityKey(method.ReturnType);
        return $"M:{containingType}|{definition.MetadataName}|{method.Arity}|{typeArguments}|{parameters}|{returnType}";
    }

    private static string GetPropertyLookupIdentityKey(PropertySymbol property)
    {
        var definition = property.OriginalDefinition ?? property;
        var containingType = GetTypeLookupIdentityKey(definition.ContainingType);
        var parameters = string.Join(",", property.Parameters.Select(static p => $"{p.RefKind}:{GetTypeLookupIdentityKey(p.Type)}"));
        var propertyType = GetTypeLookupIdentityKey(property.Type);
        return $"P:{containingType}|{definition.MetadataName}|{parameters}|{propertyType}";
    }

    private static string GetTypeLookupIdentityKey(TypeSymbol? type)
    {
        if (type is null)
            return string.Empty;

        if (type is FunctionReturnTypeSymbol projection)
            return $"FR:{GetTypeLookupIdentityKey(projection.FunctionType)}";

        if (type is StructuralFunctionTypeSymbol function)
            return $"SF:[{string.Join(";", function.ParameterTypes.Select((parameter, index) => $"{function.ParameterRefKinds[index]}:{GetTypeLookupIdentityKey(parameter)}"))}]->{GetTypeLookupIdentityKey(function.ReturnType)}";

        if (type is StructuralUnionTypeSymbol union)
            return $"SU:[{string.Join(";", union.ConstituentTypes.Select(GetTypeLookupIdentityKey).Order(StringComparer.Ordinal))}]";

        if (type is IntersectionTypeSymbol intersection)
            return $"SI:[{string.Join(";", intersection.ConstituentTypes.Select(GetTypeLookupIdentityKey).Order(StringComparer.Ordinal))}]";

        if (type is StructuralTupleTypeSymbol tuple)
            return $"ST:[{string.Join(";", tuple.TupleElements.Select(element => GetTypeLookupIdentityKey(element.Type)))}]";

        if (type is ArrayTypeSymbol array)
            return $"A:{array.Rank}:{GetTypeLookupIdentityKey(array.ElementType)}";

        if (type is NullableTypeSymbol nullable)
            return $"N:{GetTypeLookupIdentityKey(nullable.UnderlyingType)}";

        if (type is TypeParameterSymbol typeParameter)
            return $"TP:{typeParameter.ContainingSymbol?.MetadataName}:{typeParameter.MetadataName}";

        var definition = type.OriginalDefinition ?? type;
        var container = GetTypeLookupIdentityKey(definition.ContainingType);
        if (string.IsNullOrEmpty(container))
            container = GetNamespaceLookupIdentityKey(definition.ContainingNamespace);

        var typeArguments = type is NamedTypeSymbol namedType && !namedType.TypeArguments.IsDefaultOrEmpty
            ? $"<{string.Join(",", namedType.TypeArguments.Select(static typeArgument => GetTypeLookupIdentityKey(typeArgument)))}>"
            : string.Empty;

        return $"T:{container}|{definition.MetadataName}{typeArguments}";
    }

    private static string GetNamespaceLookupIdentityKey(NamespaceSymbol? namespaceSymbol)
    {
        if (namespaceSymbol is null)
            return string.Empty;

        var containing = GetNamespaceLookupIdentityKey(namespaceSymbol.ContainingNamespace);
        return string.IsNullOrEmpty(containing)
            ? namespaceSymbol.MetadataName
            : $"{containing}.{namespaceSymbol.MetadataName}";
    }

    public static TypeSymbol GetByRefElementType(this ParameterSymbol parameter)
        => parameter.Type is RefTypeSymbol refType ? refType.ElementType : parameter.Type;

    /// <summary>
    /// Unwraps the actual type of a property symbol or local symbol. 
    /// If the symbol is a type symbol, then return it, otherwise null.
    /// </summary>
    /// <param name="symbol"></param>
    /// <returns></returns>
    public static TypeSymbol? UnwrapType(this Symbol symbol)
    {
        if (symbol is PropertySymbol propertySymbol) return propertySymbol.Type;

        if (symbol is FieldSymbol fieldSymbol) return fieldSymbol.Type;

        if (symbol is ParameterSymbol parameterSymbol) return parameterSymbol.Type;

        if (symbol is LocalSymbol localSymbol) return localSymbol.Type;

        return symbol as TypeSymbol;
    }

    public static NamedTypeSymbol? GetAbsoluteBaseType(this TypeSymbol symbol)
    {
        NamedTypeSymbol? baseType = symbol.BaseType;

        if (baseType is null) return symbol as NamedTypeSymbol;

        while (baseType?.BaseType is not null)
        {
            baseType = baseType.BaseType;
        }

        return baseType;
    }

    public static IEnumerable<Symbol> ResolveMembers(this TypeSymbol symbol, string name)
    {
        var seenSignatures = new HashSet<string>();

        for (TypeSymbol? current = symbol; current is not null; current = current.BaseType)
        {
            foreach (var member in current.GetMembers())
            {
                if (member.Name != name)
                    continue;

                // Vi skapar en unik signatursträng: return type + param types
                var signature = GetSignatureKey(member);

                // Har vi redan sett denna metodsignatur? Hoppa över base-definitioner.
                if (seenSignatures.Add(signature))
                    yield return member;
            }
        }
    }

    private static string GetSignatureKey(Symbol symbol)
    {
        if (symbol is MethodSymbol method)
        {
            var paramTypes = string.Join(",", method.Parameters.Select(p => p.Type.ToDisplayString()));
            return $"{method.Name}({paramTypes})";
        }

        return symbol.Name; // fallback för andra symboltyper
    }

    public static TypeSymbol? UnwrapLiteralType(this TypeSymbol? type)
        => type is LiteralTypeSymbol literal ? literal.UnderlyingType : type;

    public static TypeSymbol? GetExtensionReceiverType(this PropertySymbol property)
    {
        if (property.GetMethod?.GetExtensionReceiverType() is { } getterReceiver)
            return getterReceiver;

        if (property.SetMethod?.GetExtensionReceiverType() is { } setterReceiver)
            return setterReceiver;

        if (property is SourcePropertySymbol sourceProperty && sourceProperty.IsDeclaredInExtension)
            return sourceProperty.ExtensionReceiverType;

        if (property.ContainingType?.GetExtensionReceiverType() is { } containerReceiver)
            return containerReceiver;

        return property.ContainingType is IExtensionReceiverResolver resolver
            ? resolver.GetExtensionReceiverType(property)
            : null;
    }

    public static TypeSymbol? GetExtensionReceiverType(this MethodSymbol method)
    {
        if (method is ProjectedMethodSymbol)
            return null;

        if (method.MethodKind is MethodKind.Conversion or MethodKind.UserDefinedOperator &&
            !method.Parameters.IsDefaultOrEmpty)
        {
            return method.Parameters[0].Type;
        }

        if (method.IsExtensionMethod && !method.Parameters.IsDefaultOrEmpty)
            return method.Parameters[0].Type;

        var resolver = method.OriginalDefinition?.ContainingType as IExtensionReceiverResolver
            ?? method.ContainingType as IExtensionReceiverResolver;
        if (resolver?.GetExtensionReceiverType(method) is { } providerReceiver)
            return providerReceiver;

        if (method.ContainingType is SourceNamedTypeSymbol sourceType &&
            sourceType.IsExtensionDeclaration &&
            sourceType.ExtensionReceiverType is { } extensionReceiverType)
        {
            var containerParameters = sourceType.TypeParameters;
            var methodParameters = method.TypeParameters;

            if (!containerParameters.IsDefaultOrEmpty &&
                methodParameters.Length >= containerParameters.Length)
            {
                var map = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);
                for (int i = 0; i < containerParameters.Length; i++)
                    map[containerParameters[i]] = methodParameters[i];

                return SubstituteTypeParameters(extensionReceiverType, map);
            }

            return extensionReceiverType;
        }

        return method.ContainingType?.GetExtensionReceiverType();
    }

    public static TypeSymbol? GetExtensionReceiverType(this NamedTypeSymbol type)
    {
        return type switch
        {
            IExtensionTypeInfo info => info.ExtensionReceiverType,
            SourceNamedTypeSymbol sourceType when sourceType.IsExtensionDeclaration => sourceType.ExtensionReceiverType,
            _ => null
        };
    }

    internal static TypeSymbol SubstituteTypeParameters(
        TypeSymbol type,
        Dictionary<TypeParameterSymbol, TypeSymbol> map)
    {
        if (type is TypeParameterSymbol parameter &&
            map.TryGetValue(parameter, out var replacement))
        {
            return replacement;
        }

        if (type is NullableTypeSymbol nullableTypeSymbol)
        {
            var underlyingType = SubstituteTypeParameters(nullableTypeSymbol.UnderlyingType, map);

            if (!SymbolEqualityComparer.Default.Equals(underlyingType, nullableTypeSymbol.UnderlyingType))
                return underlyingType.GetNullableType();

            return type;
        }

        if (type is RefTypeSymbol refType)
        {
            var substitutedElement = SubstituteTypeParameters(refType.ElementType, map);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, refType.ElementType))
                return new RefTypeSymbol(substitutedElement);

            return type;
        }

        if (type is AddressTypeSymbol address)
        {
            var substitutedElement = SubstituteTypeParameters(address.ReferencedType, map);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, address.ReferencedType))
                return new AddressTypeSymbolImplementation(substitutedElement);

            return type;
        }

        if (type is ArrayTypeSymbol arrayType)
        {
            var substitutedElement = SubstituteTypeParameters(arrayType.ElementType, map);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, arrayType.ElementType))
                return new ArrayTypeSymbolImplementation(arrayType.BaseType, substitutedElement, arrayType.ContainingSymbol, arrayType.ContainingType, arrayType.ContainingNamespace, [], arrayType.Rank, arrayType.FixedLength);

            return type;
        }

        if (type is PointerTypeSymbol pointerType)
        {
            var substitutedElement = SubstituteTypeParameters(pointerType.PointedAtType, map);

            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, pointerType.PointedAtType))
                return new PointerTypeSymbolImplementation(substitutedElement);

            return type;
        }

        if (type is NamedTypeSymbol namedType)
        {
            var typeArguments = TypeSubstitution.GetShallowTypeArguments(namedType);
            if (typeArguments.IsDefaultOrEmpty)
                return type;

            var substitutedArguments = new TypeSymbol[typeArguments.Length];
            var changed = false;

            for (int i = 0; i < substitutedArguments.Length; i++)
            {
                var original = typeArguments[i];
                var substituted = SubstituteTypeParameters(original, map);
                substitutedArguments[i] = substituted;
                changed |= !SymbolEqualityComparer.Default.Equals(original, substituted);
            }

            if (!changed)
                return type;

            return TypeSubstitution.GetDefinitionForSubstitution(namedType).Construct(substitutedArguments);
        }

        return type;
    }

    public static TypeSymbol? GetExtensionReceiverType(this Symbol symbol)
    {
        return symbol switch
        {
            MethodSymbol method => method.GetExtensionReceiverType(),
            PropertySymbol property => property.GetExtensionReceiverType(),
            NamedTypeSymbol type => type.GetExtensionReceiverType(),
            _ => null
        };
    }

    private static bool IsInstanceExtensionMemberCore(MethodSymbol method)
    {
        if (method.IsExtensionMethod)
            return true;

        var receiverType = method.GetExtensionReceiverType();
        if (receiverType is null)
            return false;

        if (method.Parameters.IsDefaultOrEmpty || method.Parameters.Length == 0)
            return false;

        return SymbolEqualityComparer.Default.Equals(receiverType, method.Parameters[0].Type);
    }

    extension(MethodSymbol method)
    {
        public ExtensionMemberKind ExtensionMemberKind
        {
            get
            {
                if (IsInstanceExtensionMemberCore(method))
                    return Raven.CodeAnalysis.ExtensionMemberKind.Instance;

                return method.GetExtensionReceiverType() is not null
                    ? Raven.CodeAnalysis.ExtensionMemberKind.Static
                    : Raven.CodeAnalysis.ExtensionMemberKind.None;
            }
        }

        public bool IsInstanceExtensionMember
            => IsInstanceExtensionMemberCore(method);

        public bool IsStaticExtensionMember
            => method.ExtensionMemberKind == Raven.CodeAnalysis.ExtensionMemberKind.Static;
    }

    extension(PropertySymbol property)
    {
        public ExtensionMemberKind ExtensionMemberKind
        {
            get
            {
                if (property is SourcePropertySymbol sourceProperty && sourceProperty.IsDeclaredInExtension)
                    return Raven.CodeAnalysis.ExtensionMemberKind.Instance;

                var accessor = property.GetMethod ?? property.SetMethod;
                if (accessor?.IsInstanceExtensionMember == true)
                    return Raven.CodeAnalysis.ExtensionMemberKind.Instance;

                return property.GetExtensionReceiverType() is not null
                    ? Raven.CodeAnalysis.ExtensionMemberKind.Static
                    : Raven.CodeAnalysis.ExtensionMemberKind.None;
            }
        }

        public bool IsExtensionProperty
            => property.ExtensionMemberKind == Raven.CodeAnalysis.ExtensionMemberKind.Instance;

        public bool IsInstanceExtensionMember
            => property.ExtensionMemberKind == Raven.CodeAnalysis.ExtensionMemberKind.Instance;

        public bool IsStaticExtensionMember
            => property.ExtensionMemberKind == Raven.CodeAnalysis.ExtensionMemberKind.Static;
    }

    extension(NamedTypeSymbol type)
    {
        public bool HasStaticExtensionMembers
        {
            get
            {
                if (type.GetExtensionReceiverType() is not null)
                    return true;

                return type is IExtensionTypeInfo { HasMemberLevelExtensions: true };
            }
        }
    }

    extension(RefKind refKind)
    {
        public bool IsByRef
            => refKind is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter;
    }

    extension(ParameterSymbol parameter)
    {
        public bool IsByRefParameter
            => parameter.RefKind.IsByRef;
    }
}
