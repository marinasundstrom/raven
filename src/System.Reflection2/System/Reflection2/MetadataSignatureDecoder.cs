namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;

internal static class MetadataSignatureDecoder
{
    public static (MetadataCustomModifiers ReturnModifiers, MetadataCustomModifiers[] ParameterModifiers) DecodeMethodCustomModifiers(
        MetadataModule module,
        MethodDefinition definition,
        MetadataType declaringTypeContext,
        IReadOnlyList<Type>? genericTypeParameters,
        IReadOnlyList<Type>? genericMethodParameters)
    {
        var provider = new MetadataCustomModifierRecordingTypeProvider(module, genericTypeParameters, genericMethodParameters, declaringTypeContext);
        var reader = module.Reader;
        var blobReader = reader.GetBlobReader(definition.Signature);
        var header = blobReader.ReadSignatureHeader();
        if (header.IsGeneric)
        {
            blobReader.ReadCompressedInteger();
        }

        var parameterCount = blobReader.ReadCompressedInteger();
        var decoder = new SignatureDecoder<Type, MetadataType?>(provider, reader, declaringTypeContext);

        provider.BeginCapture();
        decoder.DecodeType(ref blobReader);
        var returnModifiers = provider.EndCapture();

        if (parameterCount == 0)
        {
            return (returnModifiers, Array.Empty<MetadataCustomModifiers>());
        }

        var parameterModifiers = new MetadataCustomModifiers[parameterCount];
        for (var i = 0; i < parameterCount; i++)
        {
            if (blobReader.RemainingBytes > 0)
            {
                var peek = blobReader;
                if (peek.ReadSignatureTypeCode() == SignatureTypeCode.Sentinel)
                {
                    blobReader.ReadSignatureTypeCode();
                }
            }

            provider.BeginCapture();
            decoder.DecodeType(ref blobReader);
            parameterModifiers[i] = provider.EndCapture();
        }

        return (returnModifiers, parameterModifiers);
    }
}
