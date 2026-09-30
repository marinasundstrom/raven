using System;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal interface IRuntimeSymbolResolver
{
    Type GetType(ITypeSymbol typeSymbol, bool treatUnitAsVoid = false, RuntimeTypeUsage usage = RuntimeTypeUsage.Signature);
    MethodInfo GetMethodInfo(IMethodSymbol methodSymbol);
    ConstructorInfo GetConstructorInfo(IMethodSymbol constructorSymbol);
    FieldInfo GetFieldInfo(IFieldSymbol fieldSymbol);
}

internal sealed class RuntimeSymbolResolver : IRuntimeSymbolResolver
{
    private readonly CodeGenerator _codeGenerator;

    public RuntimeSymbolResolver(CodeGenerator codeGenerator)
    {
        _codeGenerator = codeGenerator ?? throw new ArgumentNullException(nameof(codeGenerator));
    }

    public Type GetType(ITypeSymbol typeSymbol, bool treatUnitAsVoid = false, RuntimeTypeUsage usage = RuntimeTypeUsage.Signature)
        => TypeSymbolExtensionsForCodeGen.ResolveType(typeSymbol, _codeGenerator, treatUnitAsVoid, usage);

    public MethodInfo GetMethodInfo(IMethodSymbol methodSymbol)
        => MethodSymbolCodeGenResolver.GetClrMethodInfo(methodSymbol, _codeGenerator);

    public ConstructorInfo GetConstructorInfo(IMethodSymbol constructorSymbol)
        => MethodSymbolCodeGenResolver.GetClrConstructorInfo(constructorSymbol, _codeGenerator);

    public FieldInfo GetFieldInfo(IFieldSymbol fieldSymbol)
        => FieldSymbolCodeGenResolver.GetClrFieldInfo(fieldSymbol, _codeGenerator);
}
