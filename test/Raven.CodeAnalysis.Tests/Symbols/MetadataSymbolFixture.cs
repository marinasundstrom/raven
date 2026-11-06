using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests.Symbols;

public static class MetadataSymbolFixture
{
    internal static NamedTypeSymbol Named(string assemblyName, string fullName)
    {
        var assembly = Make<AssemblySymbol>(SymbolKind.Assembly, assemblyName);
        var ns = Make<NamespaceSymbol>(SymbolKind.Namespace, "Example");
        Set(ns, "get_ContainingSymbol", assembly);
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, fullName.Split('.').Last(), TypeKind.Class);
        Set(type, "get_ContainingNamespace", ns);
        Set(type, "get_ContainingSymbol", ns);
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "ToFullyQualifiedMetadataName", fullName);
        return type;
    }

    internal static TypeSymbol Annotated(NamedTypeSymbol underlying)
    {
        Set(underlying, "get_IsReferenceType", true);
        var annotated = Make<IAnnotatedType>(SymbolKind.Type, underlying.Name, TypeKind.Nullable);
        Set(annotated, "get_UnderlyingType", underlying);
        return annotated;
    }

    internal interface IAnnotatedType : TypeSymbol, INullableTypeIdentity { }

    internal static TypeParameterSymbol Parameter(string name, int ordinal)
    {
        var p = Make<TypeParameterSymbol>(SymbolKind.Type, name, TypeKind.TypeParameter);
        Set(p, "get_Ordinal", ordinal);
        Set(p, "get_DeclaringTypeParameterOwner", null);
        Set(p, "get_Variance", VarianceKind.None);
        Set(p, "get_OwnerKind", TypeParameterOwnerKind.Type);
        Set(p, "ToFullyQualifiedMetadataName", name);
        ((IdentityProxy)(object)p).ReadOnce.Add("get_ConstraintTypes");
        return p;
    }

    internal static T Make<T>(SymbolKind kind, string name, TypeKind typeKind = default) where T : class
    {
        var value = DispatchProxy.Create<T, IdentityProxy>();
        Set(value, "get_Kind", kind);
        Set(value, "get_Name", name);
        Set(value, "get_MetadataName", name);
        Set(value, "get_TypeKind", typeKind);
        Set(value, "get_SpecialType", SpecialType.None);
        Set(value, "get_OriginalDefinition", value);
        Set(value, "get_IsAlias", false);
        Set(value, "get_IsGenericType", false);
        Set(value, "get_ContainingNamespace", null);
        Set(value, "get_ContainingSymbol", null);
        Set(value, "get_ContainingType", null);
        return value;
    }

    internal static void Set(object value, string name, object? result) => ((IdentityProxy)value).Values[name] = result;

    public class IdentityProxy : DispatchProxy
    {
        public Dictionary<string, object?> Values { get; } = new();
        internal HashSet<string> ReadOnce { get; } = new();

        protected override object? Invoke(MethodInfo? method, object?[]? arguments)
        {
            var member = method!.Name;
            if (!Values.TryGetValue(member, out var value))
                throw new InvalidOperationException(member);
            if (ReadOnce.Contains(member))
                Values.Remove(member);
            return value is Func<object?[]?, object?> invoke ? invoke(arguments) : value;
        }
    }
}
