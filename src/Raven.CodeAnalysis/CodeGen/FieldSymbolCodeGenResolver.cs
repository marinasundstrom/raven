using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal static class FieldSymbolCodeGenResolver
{
    internal static FieldInfo GetClrFieldInfo(IFieldSymbol fieldSymbol, CodeGenerator codeGen)
    {
        if (fieldSymbol is null)
            throw new ArgumentNullException(nameof(fieldSymbol));
        if (codeGen is null)
            throw new ArgumentNullException(nameof(codeGen));

        return fieldSymbol switch
        {
            TupleFieldSymbol tupleFieldSymbol => tupleFieldSymbol.UnderlyingField.GetFieldInfo(codeGen),
            SourceFieldSymbol sourceField => (FieldInfo)codeGen.GetMemberBuilder(sourceField),
            PEFieldSymbol peField => ResolveRuntimeFieldInfo(peField, codeGen),
            SubstitutedFieldSymbol substituted => substituted.GetFieldInfo(codeGen),
            _ => throw new NotSupportedException($"Unsupported field type: {fieldSymbol.GetType().Name}")
        };
    }

    private static FieldInfo ResolveRuntimeFieldInfo(PEFieldSymbol fieldSymbol, CodeGenerator codeGen)
    {
        if (fieldSymbol.ContainingType is null)
            throw new InvalidOperationException($"Field symbol '{fieldSymbol}' is missing a containing type.");

        var runtimeType = TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(fieldSymbol.ContainingType, codeGen);
        var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly |
                           (fieldSymbol.IsStatic ? BindingFlags.Static : BindingFlags.Instance);

        var candidates = runtimeType
            .GetFields(bindingFlags)
            .Where(field => string.Equals(field.Name, fieldSymbol.MetadataName, StringComparison.Ordinal));

        var expectedFieldType = TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(fieldSymbol.Type, codeGen);

        foreach (var candidate in candidates)
        {
            if (candidate.IsStatic != fieldSymbol.IsStatic)
                continue;

            if (!RuntimeFieldTypeMatches(candidate.FieldType, expectedFieldType))
                continue;

            return candidate;
        }

        throw new MissingFieldException(
            runtimeType.FullName,
            $"{fieldSymbol.MetadataName} : {expectedFieldType.FullName}");
    }

    private static bool RuntimeFieldTypeMatches(Type runtimeType, Type expectedType)
    {
        if (runtimeType == expectedType)
            return true;

        if (runtimeType.IsGenericTypeDefinition && expectedType.IsGenericType)
            return runtimeType == expectedType.GetGenericTypeDefinition();

        if (runtimeType.IsGenericType && expectedType.IsGenericTypeDefinition)
            return runtimeType.GetGenericTypeDefinition() == expectedType;

        return false;
    }
}
