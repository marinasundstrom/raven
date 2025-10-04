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
            ResolveReturnType(type, returnsInt),
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
        SetParameters([new SourceParameterSymbol("args", CreateStringArrayType(type), this, type, type.ContainingNamespace, location, declaringSyntaxReferences)]);
    }

    public override bool IsStatic => true;

    public override bool IsImplicitlyDeclared => true;

    private static ITypeSymbol ResolveReturnType(SynthesizedProgramClassSymbol type, bool returnsInt)
    {
        var compilation = type.Compilation;
        var assembly = type.ContainingAssembly;

        if (returnsInt)
        {
            if (assembly.GetTypeByMetadataName("System.Threading.Tasks.Task`1") is INamedTypeSymbol taskOfT)
            {
                var intType = compilation.GetSpecialType(SpecialType.System_Int32);
                if (intType.TypeKind != TypeKind.Error)
                    return taskOfT.Construct(intType);
            }
        }

        if (assembly.GetTypeByMetadataName("System.Threading.Tasks.Task") is { } taskType)
            return taskType;

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
