using System.Reflection;
using System.Security.Cryptography;
using System.Text.Json;
using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

// Reports the next native boundary using an explicit implementation seed, never consumer stubs.
internal static class LibrarySourceChecks
{
    internal static void Run(string root, string output, string seed, string nativeSystem)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var identity = AssemblyName.GetAssemblyName(seed);
        var core = new AssemblyIdentity(identity.Name!, identity.Version!, identity.CultureName ?? "",
            Convert.ToHexString(identity.GetPublicKeyToken() ?? []));
        var reference = MetadataReference.CreateFromFile(seed);
        var paths = new[] { "System/Disposable.rvn", "System/Collections/Iterator.rvn", "System/Collections/Iterable.rvn",
            "System/Collections/Collection.rvn", "System/Collections/Sequence.rvn", "System/Collections/MutableSequence.rvn",
            "System/Collections/List.rvn", "System/Collections/ArrayList.rvn" };
        var trees = paths.Select(p => SyntaxTree.ParseText(File.ReadAllText(Path.Combine(root, "runtime/raven/src", p)), path: p)).ToArray();
        var compilation = Compilation.Create("LibrarySource", trees, [reference],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
                .WithRuntimeSelfTypeContract(new("NeoCLR.CoreProbe", "System.Runtime.CompilerServices.Self")));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).Select(d => d.ToString()).ToArray();
        var bindingDiagnosticCount = errors.Length;
        object? cliBridgeEmission = null;
        var phase = "binding";
        long bytes = 0;
        if (errors.Length == 0)
        {
            using var cli = new MemoryStream();
            var cliResult = compilation.Emit(cli);
            cliBridgeEmission = new { success = cliResult.Success, bytes = cli.Length,
                diagnostics = cliResult.Diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error).Select(d => d.ToString()).ToArray() };
            using var image = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, new(new("LibrarySource", new Version(1, 0, 0, 0)), core, [new NeoClrMetadataDependency(reference, AssemblyDefinition.ReadAssembly(File.ReadAllBytes(seed), expectedExtended: false), core, NativeLibraryDefinition.ReadAssembly(File.ReadAllBytes(nativeSystem)))], reference, bootstrapReference: reference));
            errors = emitted.Diagnostics.Select(d => d.ToString()).ToArray();
            phase = emitted.Success ? "emitted" : "emission";
            bytes = image.Length;
            if (emitted.Success) File.WriteAllBytes(Path.Combine(output, "LibrarySource.dll"), image.ToArray());
            else if (bytes != 0) throw new Exception("failed emission wrote bytes");
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new {
            nativeSystemSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(nativeSystem))),
            seedSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(seed))),
            sources = paths.Select(p => new { path = "runtime/raven/src/" + p, sha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(Path.Combine(root, "runtime/raven/src", p)))) }),
            phase, bytes, bindingDiagnosticCount, cliBridgeEmission, diagnostics = errors,
            scope = "unchanged collection source hierarchy and ArrayList implementation; explicit authoring seed; emission inventory only, no runtime execution"
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine(phase + ": " + string.Join("; ", errors));
    }
}
