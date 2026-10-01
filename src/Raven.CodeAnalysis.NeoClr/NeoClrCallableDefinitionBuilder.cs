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
        => new(NeoClrTypeMapper.Instance.Map(signature.ReturnType), signature.ParameterTypes.Select(NeoClrTypeMapper.Instance.Map));
}
