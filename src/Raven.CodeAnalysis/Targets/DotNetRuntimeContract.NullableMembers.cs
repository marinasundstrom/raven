using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Targets;

internal sealed partial class DotNetRuntimeContract
{
    internal override NamedTypeSymbol? GetNullableValueMemberLookupType(Compilation compilation, TypeSymbol type, string memberName)
    {
        if (type is not NullableTypeSymbol
            {
                AbiProjection: NullableAbiProjection.NullableValueType,
                UnderlyingType.IsValueType: true
            } nullable ||
            compilation.GetSpecialType(SpecialType.System_Nullable_T) is not NamedTypeSymbol { Arity: 1 } definition)
        {
            return null;
        }

        if (definition.Construct(nullable.UnderlyingType) is not NamedTypeSymbol runtimeType)
            return null;
        return runtimeType.GetMembers(memberName).Any(member =>
            member is MethodSymbol { IsStatic: false } or PropertySymbol { IsStatic: false })
            ? runtimeType
            : null;
    }
}
