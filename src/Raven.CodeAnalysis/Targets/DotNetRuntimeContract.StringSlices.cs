using System.Linq;

namespace Raven.CodeAnalysis.Targets;

internal sealed partial class DotNetRuntimeContract
{
    internal override RuntimeRangeSliceMembers? ResolveStringSliceMembers(Compilation compilation, TypeSymbol rangeType)
        => ResolveRangeSliceMembers(compilation, compilation.GetSpecialType(SpecialType.System_String), rangeType, "Substring");

    internal override RuntimeRangeSliceMembers? ResolveSpanSliceMembers(Compilation compilation, TypeSymbol receiverType, TypeSymbol rangeType)
    {
        if (receiverType is not NamedTypeSymbol { Arity: 1 } named)
            return null;
        var definition = named.OriginalDefinition ?? named;
        var metadataName = definition.ToFullyQualifiedMetadataName();
        if (metadataName is not ("System.Span`1" or "System.ReadOnlySpan`1") ||
            !SymbolEqualityComparer.Default.Equals(definition, compilation.GetTypeByMetadataName(metadataName)))
            return null;
        return ResolveRangeSliceMembers(compilation, receiverType, rangeType, "Slice");
    }

    private static RuntimeRangeSliceMembers? ResolveRangeSliceMembers(Compilation compilation,
        TypeSymbol receiverType, TypeSymbol rangeType, string sliceMethodName)
    {
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        bool IsInt(TypeSymbol type) => SymbolEqualityComparer.Default.Equals(type, intType);
        bool IsPublicInstance(MethodSymbol method) => !method.IsStatic &&
            method.DeclaredAccessibility == Accessibility.Public && !method.IsGenericMethod;

        var getLength = receiverType.GetMembers("Length").OfType<PropertySymbol>()
            .FirstOrDefault(property => IsInt(property.Type) && property.Parameters.Length == 0)?.GetMethod;
        var offsets = rangeType.GetMembers("GetOffsetAndLength").OfType<MethodSymbol>()
            .FirstOrDefault(method => IsPublicInstance(method) && method.Parameters.Length == 1 && IsInt(method.Parameters[0].Type));
        var slice = receiverType.GetMembers(sliceMethodName).OfType<MethodSymbol>()
            .FirstOrDefault(method => IsPublicInstance(method) && method.Parameters.Length == 2 &&
                method.Parameters.All(parameter => IsInt(parameter.Type)) &&
                SymbolEqualityComparer.Default.Equals(method.ReturnType, receiverType));
        var offset = offsets?.ReturnType.GetMembers("Item1").OfType<FieldSymbol>()
            .FirstOrDefault(field => !field.IsStatic && field.DeclaredAccessibility == Accessibility.Public && IsInt(field.Type));
        var length = offsets?.ReturnType.GetMembers("Item2").OfType<FieldSymbol>()
            .FirstOrDefault(field => !field.IsStatic && field.DeclaredAccessibility == Accessibility.Public && IsInt(field.Type));
        if (getLength is null || !IsPublicInstance(getLength) || offsets is null || slice is null || offset is null || length is null)
            return null;

        return new RuntimeRangeSliceMembers(getLength, offsets, offset, length, slice);
    }
}
