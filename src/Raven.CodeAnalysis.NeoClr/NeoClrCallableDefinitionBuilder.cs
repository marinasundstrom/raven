using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NeoClrCallableDefinitionBuilder(AssemblyBuilder assembly, TypeBuilder? owner = null)
    : ICallableDefinitionBuilder<MethodBuilder>
{
    public MethodBuilder DefineMethod(string metadataName, Int32CallableSignature signature)
        => owner is null
            ? assembly.AddFunction(metadataName, signature.ParameterCount, signature.ReturnsValue)
            : owner.AddMethod(metadataName, signature.ParameterCount, signature.ReturnsValue);
}
