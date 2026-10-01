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

internal static class ImportedInterfaceChecks
{
    internal static async Task Run(string root, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var corePath = Path.Combine(root, "api-docs/reference/NeoCLR.CoreProbe.dll");
        var name = AssemblyName.GetAssemblyName(corePath);
        var core = new AssemblyIdentity(name.Name!, name.Version!, name.CultureName ?? "", Convert.ToHexString(name.GetPublicKeyToken() ?? []));
        var library = new AssemblyBuilder(new("ImportedInterfaceLibrary", new Version(1, 0, 0, 0)), core);
        var contract = library.AddGenericInterface("Example", "Value", ["T"]);
        contract.AddInterfaceMethod("Echo", new MethodSignature(SignatureType.TypeParameter(0), [SignatureType.TypeParameter(0)]));
        var implementation = library.AddClass("Example", "Concrete");
        implementation.AddInterfaceImplementation(contract.MakeGenericInstance(PrimitiveType.Int32));
        var ctor = implementation.AddConstructor(Array.Empty<PrimitiveType>()); ctor.Return();
        var echo = implementation.AddInstanceMethod("Echo", new MethodSignature(PrimitiveType.Int32, [PrimitiveType.Int32]));
        echo.LoadArgument(1); echo.Return();
        var factory = library.AddType("Example", "Factory");
        var create = factory.AddMethod("Create", new MethodSignature(contract.MakeGenericInstance(PrimitiveType.Int32), []));
        create.NewObject(ctor); create.Return();
        var concrete = factory.AddMethod("CreateConcrete", new MethodSignature(implementation, []));
        concrete.NewObject(ctor); concrete.Return();
        var libraryImage = RuntimeAssemblyContainer.WriteBinary(library.WriteNativeAssembly(), core);
        var libraryPath = Path.Combine(output, "ImportedInterfaceLibrary.dll"); File.WriteAllBytes(libraryPath, libraryImage);
        var reference = MetadataReference.CreateFromImage(libraryImage);
        var dependency = new NeoClrMetadataDependency(reference, RuntimeAssemblyContainer.ReadCliProjection(libraryImage), core);
        const string source = """
            import Example.*
            func Invoke(value: Value<int>) -> int => value.Echo(42)
            func Main() -> int {
                let first = Invoke(Factory.Create())
                return Factory.CreateConcrete().Echo(first)
            }
            """;
        var compilation = Compilation.Create("ImportedInterfaceApp", [SyntaxTree.ParseText(source)],
            [MetadataReference.CreateFromFile(corePath), reference], CompilationOptions.NeoCLR);
        using var image = new MemoryStream();
        var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, new(new("ImportedInterfaceApp", new Version(1, 0, 0, 0)), core, [dependency]));
        if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics));
        var appPath = Path.Combine(output, "ImportedInterfaceApp.dll"); File.WriteAllBytes(appPath, image.ToArray());
        File.WriteAllText(Path.ChangeExtension(appPath, ".rvn"), source);
        using var rejected = new MemoryStream(); rejected.WriteByte(73);
        var missing = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, rejected, new(new("ImportedInterfaceApp", new Version(1, 0, 0, 0)), core, []));
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
            scope = "Raven generic interface consumer of a metadata-produced native library; constructed imported callvirt dispatch returns 42; missing dependency rejected; no general class virtual dispatch or union lowering"
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
    }
}
