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

internal static class VectorLibraryChecks
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
            public static class Vectors {
                public static func Create() -> int[] => [19, 23]
                public static func Identity(values: int[]) -> int[] => values
                public static func Identity(values: long[]) -> long[] => values
                public static func Identity(values: bool[]) -> bool[] => values
                public static func Identity(values: string[]) -> string[] => values
                public static func Set(values: int[], value: int) {
                    values[0] = value
                }
                public static func Sum(values: int[]) -> int {
                    var total = 0
                    for value in values {
                        total = total + value
                    }
                    return total
                }
            }
            """;
        var libraryCompilation = Compilation.Create("VectorLibrary", [SyntaxTree.ParseText(librarySource)], [coreReference],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var libraryPath = Emit(libraryCompilation, new(new("VectorLibrary", new Version(1, 0, 0, 0)), core, []), librarySource);
        var reference = MetadataReference.CreateFromFile(libraryPath);
        var definition = RuntimeAssemblyContainer.ReadCliProjection(File.ReadAllBytes(libraryPath));
        const string appSource = """
            func Main() -> int {
                let values = Vectors.Create()
                let alias = Vectors.Identity(values)
                Vectors.Set(alias, 20)
                let wide: long[] = [42L]
                let flags: bool[] = [true]
                let labels: string[] = ["vector"]
                if Vectors.Identity(wide)[0] != 42L { return 1 }
                if !Vectors.Identity(flags)[0] { return 2 }
                if Vectors.Identity(labels).Length != 1 { return 3 }
                if values[0] != 20 { return 4 }
                return Vectors.Sum(values) - 1
            }
            """;
        var appCompilation = Compilation.Create("VectorApp", [SyntaxTree.ParseText(appSource)], [coreReference, reference], CompilationOptions.NeoCLR);
        var appPath = Emit(appCompilation, new(new("VectorApp", new Version(1, 0, 0, 0)), core,
            [new NeoClrMetadataDependency(reference, definition, core)]), appSource);
        await Command("verify", 0);
        await Command("run", 42);
        Reject([]);
        // Keep the compiler reference but remove only the matching Int32[] overload
        // from the registered snapshot: another vector overload must not be selected.
        var mismatched = Compilation.Create("VectorLibrary", [SyntaxTree.ParseText(librarySource.Replace(
            "public static func Identity(values: int[]) -> int[] => values", "", StringComparison.Ordinal))],
            [coreReference], CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var mismatchImage = new MemoryStream();
        var mismatchResult = NeoClrCompilationEmitter.EmitMetadataAssembly(mismatched, mismatchImage,
            new(new("VectorLibrary", new Version(1, 0, 0, 0)), core, []));
        if (!mismatchResult.Success) throw new Exception(string.Join("; ", mismatchResult.Diagnostics));
        Reject([new NeoClrMetadataDependency(reference, RuntimeAssemblyContainer.ReadCliProjection(mismatchImage.ToArray()), core)]);

        void Reject(NeoClrMetadataDependency[] dependencies)
        {
            using var untouched = new MemoryStream();
            untouched.WriteByte(73);
            var rejected = NeoClrCompilationEmitter.EmitMetadataAssembly(appCompilation, untouched,
                new(new("VectorApp", new Version(1, 0, 0, 0)), core, dependencies));
            if (rejected.Success || !rejected.Diagnostics.Any(d => d.Id == "NEOMETA001") || untouched.Position != 1 || !untouched.ToArray().SequenceEqual(new byte[] { 73 }))
                throw new Exception("missing dependency/overload must reject without output");
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
            coverage = new[] { "separate Raven library and application", "Int32/Int64/Boolean/String vector overloads", "returned array alias mutation", "void external call", "array iteration", "unregistered dependency rejection", "mismatched vector overload rejection" },
            scope = "static nongeneric primitive/vector imports; nominal/generic dependencies remain unsupported"
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
