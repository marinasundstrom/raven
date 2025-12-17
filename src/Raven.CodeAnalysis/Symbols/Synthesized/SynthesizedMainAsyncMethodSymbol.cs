using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

sealed class SynthesizedMainAsyncMethodSymbol : SourceMethodSymbol, IMethodSymbol
{
    public SynthesizedMainAsyncMethodSymbol(
        SynthesizedProgramClassSymbol type,
        Location[] location,
        SyntaxReference[] declaringSyntaxReferences,
        bool returnsInt)
        : base(
            "MainAsync",
            EntryPointSignature.ResolveAsyncReturnType(type.Compilation, type.ContainingAssembly, returnsInt),
            parameters: [],
            type,
            type,
            type.ContainingNamespace,
            location,
            declaringSyntaxReferences,
            isStatic: true,
            methodKind: MethodKind.Ordinary,
            isAsync: true)
    {
        ReturnsInt = returnsInt;
        SetParameters([new SourceParameterSymbol("args", EntryPointSignature.CreateStringArrayType(type.ContainingAssembly), this, type, type.ContainingNamespace, location, declaringSyntaxReferences)]);
    }

    public override bool IsStatic => true;

    public override bool IsImplicitlyDeclared => true;

    public bool ReturnsInt { get; }
    
}
