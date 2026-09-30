using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NeoClrCallableDefinitionBuilder(AssemblyBuilder assembly, TypeBuilder? owner = null)
    : ICallableDefinitionBuilder<MethodBuilder>
{
    public MethodBuilder DefineMethod(string metadataName, PrimitiveCallableSignature signature)
        => owner is null
            ? assembly.AddFunction(metadataName, ToMetadata(signature))
            : owner.AddMethod(metadataName, ToMetadata(signature));
    internal static PrimitiveMethodSignature ToMetadata(PrimitiveCallableSignature signature)
        => new(Map(signature.ReturnType), signature.ParameterTypes.Select(Map));

    private static PrimitiveType Map(SpecialType type) => type switch
    {
        SpecialType.System_Int32 => PrimitiveType.Int32,
        SpecialType.System_Boolean => PrimitiveType.Boolean,
        SpecialType.System_Void => PrimitiveType.Void,
        _ => throw new InvalidOperationException("Unsupported primitive signature")
    };
}
