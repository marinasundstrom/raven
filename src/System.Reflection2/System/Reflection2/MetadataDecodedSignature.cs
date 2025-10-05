namespace System.Reflection2;

using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;

internal readonly struct MetadataDecodedSignature
{
    public MetadataDecodedSignature(MethodSignature<System.Type> signature, MetadataCustomModifiers returnModifiers, MetadataCustomModifiers[] parameterModifiers)
    {
        Signature = signature;
        ReturnCustomModifiers = returnModifiers;
        ParameterCustomModifiers = parameterModifiers;
    }

    public MethodSignature<System.Type> Signature { get; }

    public MetadataCustomModifiers ReturnCustomModifiers { get; }

    public MetadataCustomModifiers[] ParameterCustomModifiers { get; }
}
