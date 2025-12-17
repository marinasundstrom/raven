using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

sealed partial class SynthesizedMainMethodSymbol : SourceMethodSymbol, IMethodSymbol
{

    public SynthesizedMainMethodSymbol(
        SynthesizedProgramClassSymbol type,
        Location[] location,
        SyntaxReference[] declaringSyntaxReferences,
        bool containsExecutableCode,
        bool returnsInt,
        IMethodSymbol? asyncImplementation)
        : base(
            "Main",
            returnType: EntryPointSignature.ResolveReturnType(type.Compilation, returnsInt),
            parameters: [],
            type,
            type,
            type.ContainingNamespace,
            location,
            declaringSyntaxReferences,
            isStatic: true,
            methodKind: MethodKind.Ordinary)
    {
        AsyncImplementation = asyncImplementation;
        ContainsExecutableCode = containsExecutableCode;

        SetParameters([new SourceParameterSymbol("args", EntryPointSignature.CreateStringArrayType(type.ContainingAssembly), this, type, type.ContainingNamespace, location, declaringSyntaxReferences)]);
    }

    public override bool IsStatic => true;

    public override bool IsImplicitlyDeclared => true;

    public bool ContainsExecutableCode { get; }

    public IMethodSymbol? AsyncImplementation { get; }
}
