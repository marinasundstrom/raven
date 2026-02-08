using System;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal interface IRuntimeSymbolResolver
{
    Type GetType(ITypeSymbol typeSymbol, bool treatUnitAsVoid = false, RuntimeTypeUsage usage = RuntimeTypeUsage.Signature);
    MethodInfo GetMethodInfo(IMethodSymbol methodSymbol);
    ConstructorInfo GetConstructorInfo(IMethodSymbol constructorSymbol);
}

internal sealed class RuntimeSymbolResolver : IRuntimeSymbolResolver
{
    private readonly CodeGenerator _codeGenerator;

    public RuntimeSymbolResolver(CodeGenerator codeGenerator)
    {
        _codeGenerator = codeGenerator ?? throw new ArgumentNullException(nameof(codeGenerator));
    }

    public Type GetType(ITypeSymbol typeSymbol, bool treatUnitAsVoid = false, RuntimeTypeUsage usage = RuntimeTypeUsage.Signature)
    {
        if (usage == RuntimeTypeUsage.MethodBody)
            return TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoidForMethodBody(typeSymbol, _codeGenerator);

        if (treatUnitAsVoid)
            return TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(typeSymbol, _codeGenerator);

        return TypeSymbolExtensionsForCodeGen.GetClrType(typeSymbol, _codeGenerator);
    }

    public MethodInfo GetMethodInfo(IMethodSymbol methodSymbol)
        => MethodSymbolCodeGenResolver.GetClrMethodInfo(methodSymbol, _codeGenerator);

    public ConstructorInfo GetConstructorInfo(IMethodSymbol constructorSymbol)
        => MethodSymbolCodeGenResolver.GetClrConstructorInfo(constructorSymbol, _codeGenerator);
}
