using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

// The three native call encodings stay private to the backend. Shared code only
// associates a compiler symbol with this handle; it never inspects metadata builders.
internal abstract record NeoClrCallableReference
{
    internal abstract void EmitCall(MethodBuilder body);

    internal static NeoClrCallableReference Create(ConstructedMethodReference method) => new Constructed(method);
    private sealed record Constructed(ConstructedMethodReference Method) : NeoClrCallableReference
    {
        internal override void EmitCall(MethodBuilder body)
        {
            if (Method.Definition.IsConstructor) body.NewObject(Method);
            else body.Call(Method);
        }
    }
    internal static NeoClrCallableReference Create(ImportedConstructedMethodReference method) => new ImportedConstructed(method);
    private sealed record ImportedConstructed(ImportedConstructedMethodReference Method) : NeoClrCallableReference
    {
        internal override void EmitCall(MethodBuilder body) => body.Emit(Method.Definition.IsConstructor ? OpCode.Newobj : Method.Definition.RequiresVirtualDispatch ? OpCode.Callvirt : OpCode.Call, Method);
    }
    internal static NeoClrCallableReference Create(ImportedGenericMethodReference method) => new ImportedGeneric(method);
    private sealed record ImportedGeneric(ImportedGenericMethodReference Method) : NeoClrCallableReference
    {
        internal override void EmitCall(MethodBuilder body) => body.Call(Method);
    }

    internal static NeoClrCallableReference Create(GenericMethodInstance method) => new Generic(method);
    private sealed record Generic(GenericMethodInstance Method) : NeoClrCallableReference
    {
        internal override void EmitCall(MethodBuilder body) => body.Call(Method);
    }

    internal static NeoClrCallableReference Create(MethodBuilder method) => new Defined(method);
    internal static NeoClrCallableReference Create(ImportedMethodReference method) => new Imported(method);
    internal static NeoClrCallableReference Create(NativeFunctionDefinition method) => new Native(method);

    private sealed record Defined(MethodBuilder Method) : NeoClrCallableReference
    {
        internal override void EmitCall(MethodBuilder body) => body.Emit(Method.IsConstructor ? OpCode.Newobj : OpCode.Call, Method);
    }

    private sealed record Imported(ImportedMethodReference Method) : NeoClrCallableReference
    {
        internal override void EmitCall(MethodBuilder body) => body.Emit(Method.IsConstructor ? OpCode.Newobj : Method.RequiresVirtualDispatch ? OpCode.Callvirt : OpCode.Call, Method);
    }

    private sealed record Native(NativeFunctionDefinition Method) : NeoClrCallableReference
    {
        internal override void EmitCall(MethodBuilder body) => body.Emit(OpCode.Call, Method);
    }
}
