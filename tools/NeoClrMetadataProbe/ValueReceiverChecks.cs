using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text.Json;

using RuntimeAssemblyContainer = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

using AssemblyBuilder = NeoCLR.Metadata.Experimental.Model.AssemblyBuilder;

namespace NeoClrMetadataProbe;

internal static class ValueReceiverChecks
{
    internal static async Task Run(string root, string output, string runtime, bool constructors = false)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var corePath = Path.Combine(root, "api-docs/reference/NeoCLR.CoreProbe.dll");
        var name = AssemblyName.GetAssemblyName(corePath);
        var core = new AssemblyIdentity(name.Name!, name.Version!, name.CultureName ?? "", Convert.ToHexString(name.GetPublicKeyToken() ?? []));
        var library = new AssemblyBuilder(new("ValueReceiverLibrary", new Version(1, 0, 0, 0)), core);
        var number = library.AddValueType("Example", "Number");
        var field = number.AddField("Value", PrimitiveType.Int32, FieldVisibility.Public);
        var numberConstructor = number.AddConstructor([PrimitiveType.Int32]);
        numberConstructor.LoadArgument(0); numberConstructor.LoadArgument(1); numberConstructor.StoreField(field); numberConstructor.Return();
        var ops = library.AddType("Example", "Operations");
        var create = ops.AddMethod("Create", new MethodSignature(number, []));
        var local = create.DeclareLocal(number); create.LoadDefault(number); create.StoreLocal(local);
        create.LoadLocalAddress(local); create.LoadConstant(40); create.StoreField(field); create.LoadLocal(local); create.Return();
        var read = ops.AddMethod("Read", new MethodSignature(PrimitiveType.Int32, [number]));
        read.LoadArgument(0); read.LoadField(field); read.Return();
        var get = number.AddInstanceMethod("TryGet", new MethodSignature(PrimitiveType.Boolean, [SignatureType.ByReference(PrimitiveType.Int32)], outParameters: [0]));
        get.LoadArgument(0); get.LoadArgument(0); get.LoadField(field); get.LoadConstant(2); get.Emit(OpCode.Add); get.StoreField(field);
        get.LoadArgument(1); get.LoadArgument(0); get.LoadField(field); get.StoreObject(PrimitiveType.Int32); get.Emit(OpCode.Ldc_Bool, true); get.Return();
        var box = library.AddGenericValueType("Example", "Box", ["T"]);
        var t = SignatureType.TypeParameter(0); var payload = box.AddField("Value", t, FieldVisibility.Public);
        var boxConstructor = box.AddConstructor(new MethodSignature(PrimitiveType.Void, [t]));
        boxConstructor.LoadArgument(0); boxConstructor.LoadArgument(1); boxConstructor.StoreField(payload); boxConstructor.Return();
        var set = box.AddInstanceMethod("Set", new MethodSignature(PrimitiveType.Void, [t]));
        set.LoadArgument(0); set.LoadArgument(1); set.StoreField(payload); set.Return();
        var tryGet = box.AddInstanceMethod("TryGet", new MethodSignature(PrimitiveType.Boolean, [SignatureType.ByReference(t)], outParameters: [0]));
        tryGet.LoadArgument(1); tryGet.LoadArgument(0); tryGet.LoadField(payload); tryGet.StoreObject(t); tryGet.Emit(OpCode.Ldc_Bool, true); tryGet.Return();
        var factory = ops.AddMethod("MakeBox", new MethodSignature(box.MakeGenericInstance(PrimitiveType.Int32), []));
        factory.LoadDefault(factory.Signature.ReturnType); factory.Return();
        var libraryImage = RuntimeAssemblyContainer.WriteBinary(library.WriteNativeAssembly(), core);
        var libraryPath = Path.Combine(output, "ValueReceiverLibrary.dll"); File.WriteAllBytes(libraryPath, libraryImage);
        var reference = MetadataReference.CreateFromImage(libraryImage);
        var dependency = new NeoClrMetadataDependency(reference, RuntimeAssemblyContainer.ReadCliProjection(libraryImage), core);
        var source = """
            import Example.*
            func Advance(ref value: Number) -> int {
                if !value.TryGet(out var output) {
                    return 1
                }
                return output
            }
            func Main() -> int {
                var value = Operations.Create()
                if !value.TryGet(out var result) {
                    return 1
                }
                if Operations.Read(value) != 42 {
                    return 2
                }
                if Advance(ref value) != 44 {
                    return 4
                }
                if Operations.Read(value) != 44 {
                    return 5
                }
                var box = Operations.MakeBox()
                box.Set(result)
                if !box.TryGet(out var copy) {
                    return 3
                }
                return copy
            }
            """;
        if (constructors) source = source.Replace("Operations.Create()", "Number(40)").Replace("Operations.MakeBox()", "Box<int>(0)");
        var compilation = Compilation.Create("ValueReceiverApp", [SyntaxTree.ParseText(source)],
            [MetadataReference.CreateFromFile(corePath), reference], CompilationOptions.NeoCLR);
        using var image = new MemoryStream();
        var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, new(new("ValueReceiverApp", new Version(1, 0, 0, 0)), core, [dependency]));
        if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics));
        var appPath = Path.Combine(output, "ValueReceiverApp.dll"); File.WriteAllBytes(appPath, image.ToArray());
        File.WriteAllText(Path.ChangeExtension(appPath, ".rvn"), source);
        using var rejected = new MemoryStream(); rejected.WriteByte(73);
        var missing = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, rejected, new(new("ValueReceiverApp", new Version(1, 0, 0, 0)), core, []));
        if (missing.Success || !rejected.ToArray().SequenceEqual(new byte[] { 73 })) throw new Exception("missing dependency must reject without output");
        foreach (var command in new[] { "verify", "run" })
        {
            var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
            foreach (var argument in new[] { command, appPath, "--module", libraryPath }) start.ArgumentList.Add(argument);
            using var process = Process.Start(start)!; var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
            await process.WaitForExitAsync(); var text = await stdout + await stderr;
            if (process.ExitCode != (command == "verify" ? 0 : 42)) throw new Exception(command + ": " + text);
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            verified = true, result = 42,
            coreSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(corePath))),
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            constructors,
            scope = "Raven imported value-receiver mutation and generic value-owner out calls; no union lowering or native System mapping"
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
    }
}
