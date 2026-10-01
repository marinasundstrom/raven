using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NeoClrCallableDefinitionBuilder(AssemblyBuilder assembly, TypeBuilder? owner = null)
    : ICallableDefinitionBuilder<MethodBuilder>
{
    public MethodBuilder DefineMethod(string metadataName, SourceCallablePlan plan)
    {
        var visibility = plan.Visibility switch
        {
            Accessibility.Public => MethodVisibility.Public,
            Accessibility.Internal => MethodVisibility.Internal,
            Accessibility.Private => MethodVisibility.Private,
            _ => throw new InvalidOperationException("Unsupported native callable visibility")
        };
        return owner is null
            ? assembly.AddFunction(plan.Namespace, metadataName, ToMetadata(plan.Signature), visibility)
            : plan.Symbol.IsStatic ? owner.AddMethod(metadataName, ToMetadata(plan.Signature), visibility)
            : owner.AddInstanceMethod(metadataName, ToMetadata(plan.Signature), visibility);
    }
    internal static PrimitiveMethodSignature ToMetadata(PrimitiveCallableSignature signature)
        => new(NeoClrTypeMapper.Instance.Map(signature.ReturnType), signature.ParameterTypes.Select(NeoClrTypeMapper.Instance.Map));
}
