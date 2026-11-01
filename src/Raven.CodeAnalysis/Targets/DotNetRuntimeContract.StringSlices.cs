using System.Linq;

namespace Raven.CodeAnalysis.Targets;

internal sealed partial class DotNetRuntimeContract
{
    internal override StringSliceMembers? ResolveStringSliceMembers(Compilation compilation, ITypeSymbol rangeType)
    {
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        bool IsInt(ITypeSymbol type) => SymbolEqualityComparer.Default.Equals(type, intType);
        bool IsPublicInstance(IMethodSymbol method) => !method.IsStatic &&
            method.DeclaredAccessibility == Accessibility.Public && !method.IsGenericMethod;

        var getLength = stringType.GetMembers("Length").OfType<IPropertySymbol>()
            .FirstOrDefault(property => IsInt(property.Type) && property.Parameters.Length == 0)?.GetMethod;
        var offsets = rangeType.GetMembers("GetOffsetAndLength").OfType<IMethodSymbol>()
            .FirstOrDefault(method => IsPublicInstance(method) && method.Parameters.Length == 1 && IsInt(method.Parameters[0].Type));
        var substring = stringType.GetMembers("Substring").OfType<IMethodSymbol>()
            .FirstOrDefault(method => IsPublicInstance(method) && method.Parameters.Length == 2 &&
                method.Parameters.All(parameter => IsInt(parameter.Type)) &&
                SymbolEqualityComparer.Default.Equals(method.ReturnType, stringType));
        var offset = offsets?.ReturnType.GetMembers("Item1").OfType<IFieldSymbol>()
            .FirstOrDefault(field => !field.IsStatic && field.DeclaredAccessibility == Accessibility.Public && IsInt(field.Type));
        var length = offsets?.ReturnType.GetMembers("Item2").OfType<IFieldSymbol>()
            .FirstOrDefault(field => !field.IsStatic && field.DeclaredAccessibility == Accessibility.Public && IsInt(field.Type));
        if (getLength is null || !IsPublicInstance(getLength) || offsets is null || substring is null || offset is null || length is null)
            return null;

        return new StringSliceMembers(getLength, offsets, offset, length, substring);
    }
}
