using System;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal enum RuntimeTypeUsage
{
    Signature,
    MethodBody
}

internal sealed class RuntimeTypeMap
{
    private readonly CodeGenerator _codeGen;

    public RuntimeTypeMap(CodeGenerator codeGen)
    {
        _codeGen = codeGen ?? throw new ArgumentNullException(nameof(codeGen));
    }

    public bool TryResolveTypeParameter(ITypeParameterSymbol symbol, RuntimeTypeUsage usage, out Type type)
    {
        if (symbol is null)
            throw new ArgumentNullException(nameof(symbol));

        return _codeGen.TryResolveRuntimeTypeParameter(symbol, usage, out type);
    }
}

