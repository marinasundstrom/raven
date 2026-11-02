using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Targets;

internal sealed partial class DotNetRuntimeContract
{
    internal override INamedTypeSymbol? GetNullableValueMemberLookupType(Compilation compilation, ITypeSymbol type, string memberName)
    {
        if (type is not NullableTypeSymbol
            {
                AbiProjection: NullableAbiProjection.NullableValueType,
                UnderlyingType.IsValueType: true
            } nullable ||
            compilation.GetSpecialType(SpecialType.System_Nullable_T) is not INamedTypeSymbol { Arity: 1 } definition)
        {
            return null;
        }

        if (definition.Construct(nullable.UnderlyingType) is not INamedTypeSymbol runtimeType)
            return null;
        return runtimeType.GetMembers(memberName).Any(member =>
            member is IMethodSymbol { IsStatic: false } or IPropertySymbol { IsStatic: false })
            ? runtimeType
            : null;
    }
}
