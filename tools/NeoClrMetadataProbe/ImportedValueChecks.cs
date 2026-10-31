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

internal static class ImportedValueChecks
{
    internal static async Task Run(string root, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var corePath = Path.Combine(root, "api-docs/reference/NeoCLR.CoreProbe.dll");
        var name = AssemblyName.GetAssemblyName(corePath);
        var core = new AssemblyIdentity(name.Name!, name.Version!, name.CultureName ?? "", Convert.ToHexString(name.GetPublicKeyToken() ?? []));
        var library = new AssemblyBuilder(new("ImportedValueLibrary", new Version(1, 0, 0, 0)), core);
        var number = library.AddValueType("Example", "Number");
        var field = number.AddField("Value", PrimitiveType.Int32, FieldVisibility.Public);
        var ops = library.AddType("Example", "Operations");
        var create = ops.AddMethod("Create", new MethodSignature(number, []));
        var local = create.DeclareLocal(number); create.LoadDefault(number); create.StoreLocal(local);
        create.LoadLocalAddress(local); create.LoadConstant(42); create.StoreField(field); create.LoadLocal(local); create.Return();
        var read = ops.AddMethod("Read", new MethodSignature(PrimitiveType.Int32, [number]));
        read.LoadArgument(0); read.LoadField(field); read.Return();
        var libraryImage = RuntimeAssemblyContainer.WriteBinary(library.WriteNativeAssembly(), core);
        var libraryPath = Path.Combine(output, "ImportedValueLibrary.dll"); File.WriteAllBytes(libraryPath, libraryImage);
        var reference = MetadataReference.CreateFromImage(libraryImage);
        var dependency = new NeoClrMetadataDependency(reference, RuntimeAssemblyContainer.ReadCliProjection(libraryImage), core);
        const string source = """
            import Example.*
            func Forward(value: Number) -> Number => value
            func Main() -> int {
                let value = Operations.Create()
                let forwarded = Forward(value)
                return Operations.Read(forwarded)
            }
            """;
        var compilation = Compilation.Create("ImportedValueApp", [SyntaxTree.ParseText(source)],
            [MetadataReference.CreateFromFile(corePath), reference], CompilationOptions.NeoCLR);
        using var image = new MemoryStream();
        var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, new(new("ImportedValueApp", new Version(1, 0, 0, 0)), core, [dependency]));
        if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics));
        var appPath = Path.Combine(output, "ImportedValueApp.dll"); File.WriteAllBytes(appPath, image.ToArray());
        File.WriteAllText(Path.ChangeExtension(appPath, ".rvn"), source);
        using var rejected = new MemoryStream(); rejected.WriteByte(73);
        var missing = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, rejected, new(new("ImportedValueApp", new Version(1, 0, 0, 0)), core, []));
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
            scope = "Raven consumer of metadata-produced native value library; imported return/parameter/local/forwarding; missing dependency rejection; no imported instance members or union lowering"
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
    }
}
