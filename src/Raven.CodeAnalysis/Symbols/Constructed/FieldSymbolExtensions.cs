using System.Reflection;

using Raven.CodeAnalysis.CodeGen;

namespace Raven.CodeAnalysis.Symbols;

internal static class FieldSymbolExtensions
{

    internal static FieldInfo GetFieldInfo(this IFieldSymbol fieldSymbol, CodeGenerator codeGen)
    {
        return fieldSymbol switch
        {
            TupleFieldSymbol tupleFieldSymbol => tupleFieldSymbol.UnderlyingField.GetFieldInfo(codeGen),
            SourceFieldSymbol sourceField => (FieldInfo)codeGen.GetMemberBuilder(sourceField),
            PEFieldSymbol field => field.GetFieldInfo(),
            SubstitutedFieldSymbol field => field.GetFieldInfo(codeGen),
            _ => throw new NotSupportedException($"Unsupported field type: {fieldSymbol.GetType().Name}")
        };
    }
}
