using System;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal interface IRuntimeSymbolResolver
{
    Type GetType(TypeSymbol typeSymbol, bool treatUnitAsVoid = false, RuntimeTypeUsage usage = RuntimeTypeUsage.Signature);
    MethodInfo GetMethodInfo(MethodSymbol methodSymbol);
    ConstructorInfo GetConstructorInfo(MethodSymbol constructorSymbol);
    FieldInfo GetFieldInfo(FieldSymbol fieldSymbol);
}

internal sealed class RuntimeSymbolResolver : IRuntimeSymbolResolver
{
    private readonly CodeGenerator _codeGenerator;

    public RuntimeSymbolResolver(CodeGenerator codeGenerator)
    {
        _codeGenerator = codeGenerator ?? throw new ArgumentNullException(nameof(codeGenerator));
    }

    public Type GetType(TypeSymbol typeSymbol, bool treatUnitAsVoid = false, RuntimeTypeUsage usage = RuntimeTypeUsage.Signature)
        => TypeSymbolExtensionsForCodeGen.ResolveType(typeSymbol, _codeGenerator, treatUnitAsVoid, usage);

    public MethodInfo GetMethodInfo(MethodSymbol methodSymbol)
        => MethodSymbolCodeGenResolver.GetClrMethodInfo(methodSymbol, _codeGenerator);

    public ConstructorInfo GetConstructorInfo(MethodSymbol constructorSymbol)
        => MethodSymbolCodeGenResolver.GetClrConstructorInfo(constructorSymbol, _codeGenerator);

    public FieldInfo GetFieldInfo(FieldSymbol fieldSymbol)
        => FieldSymbolCodeGenResolver.GetClrFieldInfo(fieldSymbol, _codeGenerator);
}
