using System;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEMethodSymbol
{
    bool IParameterConversionClassifier.TryClassifyParameterConversion(
        ITypeSymbol argumentType, int parameterIndex, out ParameterConversionKind conversion)
    {
        conversion = ParameterConversionKind.None;
        if (argumentType.TypeKind == TypeKind.Error ||
            !TryGetSpecialTypeMetadataName(argumentType.GetNonNullableType(), out var argumentMetadataName) ||
            !TryGetParameterRuntimeTypeMetadataName(parameterIndex, out var parameterMetadataName) ||
            string.IsNullOrWhiteSpace(parameterMetadataName))
        {
            return false;
        }

        if (string.Equals(parameterMetadataName, argumentMetadataName, StringComparison.Ordinal))
            conversion = ParameterConversionKind.Identity;
        else if (IsImplicitSpecialTypeMetadataConversion(argumentMetadataName, parameterMetadataName))
            conversion = ParameterConversionKind.ImplicitNumeric;
        else if (string.Equals(parameterMetadataName, "System.Object", StringComparison.Ordinal))
            conversion = ParameterConversionKind.ToObject;

        return true;
    }

    private static bool TryGetSpecialTypeMetadataName(ITypeSymbol type, out string metadataName)
    {
        if (type is IArrayTypeSymbol { Rank: 1, ElementType: { } elementType } &&
            TryGetSpecialTypeMetadataName(elementType.GetNonNullableType(), out var elementMetadataName))
        {
            metadataName = elementMetadataName + "[]";
            return true;
        }

        metadataName = type.SpecialType switch
        {
            SpecialType.System_Boolean => "System.Boolean",
            SpecialType.System_Byte => "System.Byte",
            SpecialType.System_Char => "System.Char",
            SpecialType.System_Decimal => "System.Decimal",
            SpecialType.System_Double => "System.Double",
            SpecialType.System_Int16 => "System.Int16",
            SpecialType.System_Int32 => "System.Int32",
            SpecialType.System_Int64 => "System.Int64",
            SpecialType.System_Object => "System.Object",
            SpecialType.System_SByte => "System.SByte",
            SpecialType.System_Single => "System.Single",
            SpecialType.System_String => "System.String",
            SpecialType.System_UInt16 => "System.UInt16",
            SpecialType.System_UInt32 => "System.UInt32",
            SpecialType.System_UInt64 => "System.UInt64",
            _ => string.Empty
        };

        return metadataName.Length > 0;
    }

    private static bool IsImplicitSpecialTypeMetadataConversion(string sourceMetadataName, string targetMetadataName)
        => sourceMetadataName switch
        {
            "System.Byte" => targetMetadataName is "System.Int16" or "System.UInt16" or "System.Int32" or "System.UInt32" or "System.Int64" or "System.UInt64" or "System.Single" or "System.Double" or "System.Decimal",
            "System.SByte" => targetMetadataName is "System.Int16" or "System.Int32" or "System.Int64" or "System.Single" or "System.Double" or "System.Decimal",
            "System.Int16" => targetMetadataName is "System.Int32" or "System.Int64" or "System.Single" or "System.Double" or "System.Decimal",
            "System.UInt16" => targetMetadataName is "System.Int32" or "System.UInt32" or "System.Int64" or "System.UInt64" or "System.Single" or "System.Double" or "System.Decimal",
            "System.Int32" => targetMetadataName is "System.Int64" or "System.Single" or "System.Double" or "System.Decimal",
            "System.UInt32" => targetMetadataName is "System.Int64" or "System.UInt64" or "System.Single" or "System.Double" or "System.Decimal",
            "System.Int64" => targetMetadataName is "System.Single" or "System.Double" or "System.Decimal",
            "System.UInt64" => targetMetadataName is "System.Single" or "System.Double" or "System.Decimal",
            "System.Single" => targetMetadataName is "System.Double",
            _ => false
        };

}
