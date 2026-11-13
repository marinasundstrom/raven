using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation : SourceTypeDeclarationServices
{
    void SourceTypeDeclarationServices.EnsureSourceDeclarationsComplete() => EnsureSourceDeclarationsComplete();

    SpecialType SourceTypeDeclarationServices.GetSourceTypeSpecialType(NamedTypeSymbol type)
    {
        if (GetSourceCoreSpecialType(type) is { } sourceSpecial && sourceSpecial != SpecialType.None)
            return sourceSpecial;
        if (type.Name == "Object" && type.MetadataName == "Object" && type.TypeKind == TypeKind.Class &&
            type.ContainingType is null && type.ContainingNamespace?.ToDisplayString() == "System" &&
            UsesSourceObjectRoot == true)
            return SpecialType.System_Object;
        if (type.ContainingType is null && Options.TargetPlatform == TargetPlatform.NeoCLR &&
            Options.MetadataImportOptions?.AsyncAssemblyName == type.ContainingAssembly?.Name)
            return (type.ContainingNamespace?.ToMetadataName() + "." + type.MetadataName) switch
            {
                "System.Runtime.CompilerServices.AsyncStateMachine" => SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine,
                "System.Concurrency.Tasks.Task`1" => SpecialType.System_Threading_Tasks_Task_T,
                "System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1" => SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T,
                _ => SpecialType.None
            };
        return SpecialType.None;
    }

    NamedTypeSymbol? SourceTypeDeclarationServices.GetArrayShape(NamedTypeSymbol sourceType, ArrayTypeSymbol array)
    {
        if (Options.TargetPlatform != TargetPlatform.NeoCLR || Options.MetadataImportOptions?.UseSourceCore != true ||
            sourceType.SpecialType != SpecialType.System_Array || array.Rank != 1 ||
            Options.RuntimeIterationContract is not { ArrayShapeTypeName: { } name } contract)
            return null;
        var shape = GetTypeByMetadataName(name);
        return shape is { TypeKind: TypeKind.Class, Arity: 1 } && shape.ContainingAssembly?.Name == contract.AssemblyName
            ? shape.Construct(array.ElementType) as NamedTypeSymbol : null;
    }
}
