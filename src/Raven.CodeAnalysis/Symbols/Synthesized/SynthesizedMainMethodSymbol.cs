using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

sealed partial class SynthesizedMainMethodSymbol : SourceMethodSymbol, IMethodSymbol
{

    public SynthesizedMainMethodSymbol(
        SynthesizedProgramClassSymbol type,
        Location[] location,
        SyntaxReference[] declaringSyntaxReferences)
        : base(
            "Main",
            returnType: ResolveTaskReturnType(type) ?? type.ContainingAssembly.GetTypeByMetadataName("System.Void"),
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
        var arrayType = type.ContainingAssembly.GetTypeByMetadataName("System.Array");
        var stringType = type.ContainingAssembly.GetTypeByMetadataName("System.String");
        var stringArrayType = new ArrayTypeSymbol(arrayType, stringType, arrayType.ContainingSymbol, null, arrayType.ContainingNamespace, []);

        SetParameters([new SourceParameterSymbol("args", stringArrayType, this, type, type.ContainingNamespace, location, declaringSyntaxReferences)]);
    }

    public override bool IsStatic => true;

    public override bool IsImplicitlyDeclared => true;

    private static ITypeSymbol? ResolveTaskReturnType(SynthesizedProgramClassSymbol type)
    {
        var assembly = type.ContainingAssembly;
        var taskType = assembly.GetTypeByMetadataName("System.Threading.Tasks.Task");

        if (taskType is not null)
            return taskType;

        return null;
    }
}