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

internal static class GenericLibraryChecks
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
            public static class Algorithms {
                public static func Identity<T>(value: T) -> T => value
                public static func First<T>(values: T[]) -> T => values[0]
                public static func Set<T>(values: T[], value: T) {
                    values[0] = value
                }
                public static func Choose<T>(value: T) -> T => value
                public static func Choose<T, U>(value: T, ignored: U) -> T => value
            }
            """;
        var libraryCompilation = Compilation.Create("GenericLibrary", [SyntaxTree.ParseText(librarySource)], [coreReference],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var libraryPath = Emit(libraryCompilation, new(new("GenericLibrary", new Version(1, 0, 0, 0)), core, []), librarySource);
        var reference = MetadataReference.CreateFromFile(libraryPath);
        var definition = RuntimeAssemblyContainer.ReadCliProjection(File.ReadAllBytes(libraryPath));
        const string appSource = """
            class Order { var Number: int = 42 }
            class Relay<T> {
                func Send(value: T) -> T => Algorithms.Identity<T>(value)
            }
            func Forward<T>(value: T) -> T => Algorithms.Identity<T>(value)
            func Main() -> int {
                let order = Order()
                let orders: Order[] = [order]
                let same = Algorithms.First<Order>(orders)
                if same.Number != 42 { return 6 }
                same.Number = 7
                if order.Number != 7 { return 7 }
                let relay = Relay<Order>()
                if relay.Send(order).Number != 7 { return 9 }
                let empty = default(Box<int>)
                Algorithms.Identity<Box<int>?>(empty)
                let forwarded = Forward<Order>(order)
                if forwarded.Number != 7 { return 8 }
                let values: int[] = [19, 23]
                let alias = Algorithms.Identity<int[]>(values)
                Algorithms.Set<int>(alias, 42)
                let wide: long[] = [5000000000L]
                let flags: bool[] = [true]
                let labels: string[] = ["generic"]
                if Algorithms.First<long>(wide) != 5000000000L { return 1 }
                if !Algorithms.First<bool>(flags) { return 2 }
                Algorithms.First<string>(labels)
                if Algorithms.Identity<string[]>(labels).Length != 1 { return 3 }
                if Algorithms.Choose<int>(7) != 7 { return 4 }
                if Algorithms.Choose<int, bool>(8, true) != 8 { return 5 }
                return Algorithms.First<int>(values)
            }
            """;
        var appCompilation = Compilation.Create("GenericApp", [SyntaxTree.ParseText(appSource)], [coreReference, reference], CompilationOptions.NeoCLR);
        var appPath = Emit(appCompilation, new(new("GenericApp", new Version(1, 0, 0, 0)), core,
            [new NeoClrMetadataDependency(reference, definition, core)]), appSource);
        await Command("verify", 0);
        await Command("run", 42);
        Reject([]);
        // Keep the compiler reference but remove the matching generic First declaration
        // from the registered snapshot: a generic call must not bypass the snapshot contract.
        var mismatched = Compilation.Create("GenericLibrary", [SyntaxTree.ParseText(librarySource.Replace(
            "public static func First<T>(values: T[]) -> T => values[0]", "", StringComparison.Ordinal))],
            [coreReference], CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var mismatchImage = new MemoryStream();
        var mismatchResult = NeoClrCompilationEmitter.EmitMetadataAssembly(mismatched, mismatchImage,
            new(new("GenericLibrary", new Version(1, 0, 0, 0)), core, []));
        if (!mismatchResult.Success) throw new Exception(string.Join("; ", mismatchResult.Diagnostics));
        Reject([new NeoClrMetadataDependency(reference, RuntimeAssemblyContainer.ReadCliProjection(mismatchImage.ToArray()), core)]);

        void Reject(NeoClrMetadataDependency[] dependencies)
        {
            using var untouched = new MemoryStream();
            untouched.WriteByte(73);
            var rejected = NeoClrCompilationEmitter.EmitMetadataAssembly(appCompilation, untouched,
                new(new("GenericApp", new Version(1, 0, 0, 0)), core, dependencies));
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
            coverage = new[] { "separate Raven library and application", "primitive, vector and consumer-owned nominal generic arguments", "caller method/owner generic forwarding", "external constructed argument", "nominal alias mutation", "generic arity overloads", "returned array alias mutation", "void external call", "unregistered dependency rejection", "missing generic declaration rejection" },
            scope = "unconstrained static generic methods on nongeneric owners; generic owners and constrained imports remain unsupported"
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
