using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

// Definition handles and constructed MemberRefs are private backend details.
internal sealed record NeoClrFieldReference(FieldBuilder Definition, ConstructedFieldReference? Construction = null)
{
    internal void Emit(MethodBuilder body, bool store)
    {
        var code = store ? OpCode.Stfld : OpCode.Ldfld;
        if (Construction is { } reference) body.Emit(code, reference);
        else body.Emit(code, Definition);
    }
}
