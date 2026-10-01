using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text.Json;

using RuntimeAssemblyContainer = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class ExternalSignatureChecks
{
    internal static async Task Run(string root, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var corePath = Path.Combine(root, "api-docs/reference/NeoCLR.CoreProbe.dll");
        var coreReference = MetadataReference.CreateFromFile(corePath);
        var name = AssemblyName.GetAssemblyName(corePath);
        var core = new AssemblyIdentity(name.Name!, name.Version!, name.CultureName ?? "", Convert.ToHexString(name.GetPublicKeyToken() ?? []));
        const string librarySource = """
            public class Box<T> { }
            public interface View { }
            """;
        var libraryCompilation = Compilation.Create("ExternalSignatureLibrary", [SyntaxTree.ParseText(librarySource)], [coreReference],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var libraryPath = Emit(libraryCompilation, new(new("ExternalSignatureLibrary", new Version(1, 0, 0, 0)), core, []), librarySource);
        var reference = MetadataReference.CreateFromFile(libraryPath);
        var definition = RuntimeAssemblyContainer.ReadCliProjection(File.ReadAllBytes(libraryPath));
        const string appSource = """
            class Order { var Number: int = 42 }
            func Forward<T>(value: Box<T>?) -> Box<T>? => value
            func Accept(value: Box<Order>?) -> int => 42
            func Main() -> int {
                let value = default(Box<Order>)
                let alias = Forward<Order>(value)
                Forward<Order>(alias)
                let views: View?[] = [default(View)]
                if views.Length != 1 { return 1 }
                return Accept(alias)
            }
            """;
        var appCompilation = Compilation.Create("ExternalSignatureApp", [SyntaxTree.ParseText(appSource)], [coreReference, reference], CompilationOptions.NeoCLR);
        var appPath = Emit(appCompilation, new(new("ExternalSignatureApp", new Version(1, 0, 0, 0)), core,
            [new NeoClrMetadataDependency(reference, definition, core)]), appSource);
        await Command("verify", 0);
        await Command("run", 42);
        Reject([]);
        // The compiler reference and registered dependency snapshot must agree.
        var mismatched = Compilation.Create("ExternalSignatureLibrary", [SyntaxTree.ParseText("public interface View { }")],
            [coreReference], CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var mismatchImage = new MemoryStream();
        var mismatchResult = NeoClrCompilationEmitter.EmitMetadataAssembly(mismatched, mismatchImage,
            new(new("ExternalSignatureLibrary", new Version(1, 0, 0, 0)), core, []));
        if (!mismatchResult.Success) throw new Exception(string.Join("; ", mismatchResult.Diagnostics));
        Reject([new NeoClrMetadataDependency(reference, RuntimeAssemblyContainer.ReadCliProjection(mismatchImage.ToArray()), core)]);

        void Reject(NeoClrMetadataDependency[] dependencies)
        {
            using var untouched = new MemoryStream();
            untouched.WriteByte(73);
            var rejected = NeoClrCompilationEmitter.EmitMetadataAssembly(appCompilation, untouched,
                new(new("ExternalSignatureApp", new Version(1, 0, 0, 0)), core, dependencies));
            if (rejected.Success || !rejected.Diagnostics.Any(d => d.Id == "NEOMETA001") || untouched.Position != 1 || !untouched.ToArray().SequenceEqual(new byte[] { 73 }))
                throw new Exception("missing dependency/type must reject without output");
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            bindingProfile = "CompilationOptions.NeoCLR; projected CLI declarations; no host core",
            coreSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(corePath))),
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            librarySha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(libraryPath))),
            applicationSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(appPath))),
            verified = true,
            result = 42,
            coverage = new[] { "separate Raven library and application", "external generic signatures with consumer-owned arguments", "method generic forwarding", "discarded nominal result", "external interface arrays", "unregistered dependency rejection", "missing type rejection" },
            scope = "public top-level unconstrained reference signatures; imported constructors/member calls and translated System identity mapping remain unsupported"
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");

        string Emit(Compilation compilation, NeoClrEmitOptions options, string source)
        {
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, options);
            if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics));
            var path = Path.Combine(output, options.Identity.Name + ".dll");
            File.WriteAllBytes(path, image.ToArray());
            File.WriteAllText(Path.ChangeExtension(path, ".rvn"), source);
            return path;
        }
        async Task Command(string command, int expected)
        {
            var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
            foreach (var argument in new[] { command, appPath, "--module", libraryPath }) start.ArgumentList.Add(argument);
            using var process = Process.Start(start)!;
            var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
            await process.WaitForExitAsync();
            var text = await stdout; var error = await stderr;
            if (process.ExitCode != expected) throw new Exception(command + ": " + process.ExitCode + " " + text + error);
        }
    }
}
