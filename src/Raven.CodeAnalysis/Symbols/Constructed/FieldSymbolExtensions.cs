using System.Reflection;

using Raven.CodeAnalysis.CodeGen;

namespace Raven.CodeAnalysis.Symbols;

internal static class FieldSymbolExtensions
{

    internal static FieldInfo GetFieldInfo(this IFieldSymbol fieldSymbol, CodeGenerator codeGen)
    {
        return FieldSymbolCodeGenResolver.GetClrFieldInfo(fieldSymbol, codeGen);
    }
}
