using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

// Definition handles and constructed MemberRefs are private backend details.
internal sealed record NeoClrFieldReference(FieldBuilder? Definition, ConstructedFieldReference? Construction = null, ImportedFieldReference? Import = null, ImportedConstructedFieldReference? ImportedConstruction = null, PrimitiveType? IntrinsicStorage = null)
{
    internal void EmitAddress(IILGenerator body) => Emit(body, OpCode.Ldflda);
    internal void Emit(IILGenerator body, bool store) => Emit(body, store ? OpCode.Stfld : OpCode.Ldfld);
    private void Emit(IILGenerator body, OpCode code)
    {
        if (IntrinsicStorage is { } primitive)
        {
            if (primitive == PrimitiveType.String)
            {
                if (code != OpCode.Ldfld) throw new NotSupportedException("Runtime String storage is immutable and has no field address.");
                return; // The reference receiver itself is the intrinsic storage value.
            }
            if (code == OpCode.Ldflda) return; // The checked receiver already is the scalar address.
            body.Emit(code == OpCode.Stfld ? OpCode.Stobj : OpCode.Ldobj, (SignatureType)primitive);
        }
        else if (ImportedConstruction is { } constructedImport) body.Emit(code, constructedImport);
        else if (Import is { } imported) body.Emit(code, imported);
        else if (Construction is { } reference) body.Emit(code, reference);
        else body.Emit(code, Definition!);
    }
}
