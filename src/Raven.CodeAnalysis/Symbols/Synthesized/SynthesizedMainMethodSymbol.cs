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
        SynthesizedMainAsyncMethodSymbol? asyncImplementation)
        : base(
            "Main",
            returnType: ResolveReturnType(type, returnsInt),
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

        SetParameters([new SourceParameterSymbol("args", CreateStringArrayType(type), this, type, type.ContainingNamespace, location, declaringSyntaxReferences)]);
    }

    public override bool IsStatic => true;

    public override bool IsImplicitlyDeclared => true;

    public bool ContainsExecutableCode { get; }

    public SynthesizedMainAsyncMethodSymbol? AsyncImplementation { get; }

    private static ITypeSymbol ResolveReturnType(SynthesizedProgramClassSymbol type, bool returnsInt)
    {
        var compilation = type.Compilation;

        if (returnsInt)
            return type.Compilation.GetSpecialType(SpecialType.System_Int32);

        return compilation.GetSpecialType(SpecialType.System_Unit);
    }

    private static ITypeSymbol CreateStringArrayType(SynthesizedProgramClassSymbol type)
    {
        var assembly = type.ContainingAssembly;
        var arrayType = assembly.GetTypeByMetadataName("System.Array");
        var stringType = assembly.GetTypeByMetadataName("System.String");

        return new ArrayTypeSymbol(arrayType, stringType, arrayType.ContainingSymbol, null, arrayType.ContainingNamespace, Array.Empty<Location>());
    }
}
