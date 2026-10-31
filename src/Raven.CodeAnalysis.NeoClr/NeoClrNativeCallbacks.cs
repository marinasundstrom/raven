using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

// The managed Function keeps its logical unit result. C adapters discard it for
// callback parameters and materialize it when invoking returned no-result functions.
// This also handles callbacks whose original generic return parameter closes over unit.
internal sealed class NeoClrNativeCallbacks(AssemblyBuilder assembly, Func<SignatureType> unitType)
{
    private readonly Dictionary<SignatureType, (MethodBuilder Constructor, MethodBuilder Invoke)> adapters = [];
    private int importCount;

    internal void SetImport(MethodBuilder method, NativeImportDefinition import)
    {
        var source = method.Signature;
        var parameters = source.ParameterTypes.ToArray();
        var adapted = new Dictionary<int, (MethodBuilder Constructor, MethodBuilder Invoke)>();
        foreach (var index in import.CallbackParameters.Keys)
        {
            var input = parameters[index];
            if (input.FunctionSignature is not { NoResult: false } shape || shape.ReturnType != unitType())
                continue;
            parameters[index] = SignatureType.Function(new MethodSignature(PrimitiveType.Void, shape.ParameterTypes));
            adapted.Add(index, Adapter(input, parameters[index]));
        }
        var nativeResult = source.ReturnType;
        (MethodBuilder Constructor, MethodBuilder Invoke)? resultAdapter = null;
        if (import.FunctionReturn is not null && nativeResult.FunctionSignature is { NoResult: false } resultShape &&
            resultShape.ReturnType == unitType())
        {
            nativeResult = SignatureType.Function(new MethodSignature(PrimitiveType.Void, resultShape.ParameterTypes));
            resultAdapter = Adapter(nativeResult, source.ReturnType);
        }
        if (adapted.Count == 0 && resultAdapter is null)
        {
            method.SetNativeImport(import);
            return;
        }

        // Keep the source-facing declaration callable across assemblies with its exact
        // language signature; isolate the projected ABI in an internal foreign thunk.
        var native = assembly.AddFunction("Raven.Generated", "NativeImport" + importCount++,
            new MethodSignature(nativeResult, parameters), MethodVisibility.Internal);
        native.SetNativeImport(import);
        var body = method.GetILGenerator();
        for (var i = 0; i < parameters.Length; i++)
        {
            body.LoadArgument(i);
            if (adapted.TryGetValue(i, out var adapter))
            {
                body.NewObject(adapter.Constructor);
                body.BindFunction(parameters[i], adapter.Invoke);
            }
        }
        body.Call(native);
        if (resultAdapter is { } returned)
        {
            body.NewObject(returned.Constructor);
            body.BindFunction(source.ReturnType, returned.Invoke);
        }
        body.Return();
    }

    private (MethodBuilder Constructor, MethodBuilder Invoke) Adapter(SignatureType input, SignatureType output)
    {
        if (adapters.TryGetValue(input, out var existing)) return existing;
        var owner = assembly.AddClass("Raven.Generated", "NativeUnitCallback" + adapters.Count, TypeVisibility.Internal);
        var callback = owner.AddField("callback", input, FieldVisibility.Private, isReadOnly: true);
        var constructor = owner.AddConstructor(new MethodSignature(PrimitiveType.Void, [input]));
        var body = constructor.GetILGenerator();
        body.LoadArgument(0);
        body.LoadArgument(1);
        body.StoreField(callback);
        body.Return();
        var invoke = owner.AddInstanceMethod("Invoke", new MethodSignature(output.FunctionSignature!.ReturnType, input.FunctionSignature!.ParameterTypes));
        body = invoke.GetILGenerator();
        body.LoadArgument(0);
        body.LoadField(callback);
        for (var i = 0; i < input.FunctionSignature.ParameterTypes.Count; i++) body.LoadArgument(i + 1);
        body.InvokeFunction(input);
        if (output.FunctionSignature.NoResult) body.Emit(OpCode.Pop);
        else body.LoadDefault(output.FunctionSignature.ReturnType);
        body.Return();
        return adapters[input] = (constructor, invoke);
    }
}
