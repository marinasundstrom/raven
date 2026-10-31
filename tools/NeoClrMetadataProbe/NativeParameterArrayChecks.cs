using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeParameterArrayChecks
{
    internal static void Run(string corePath, string directory)
    {
        Directory.CreateDirectory(directory);
        var core = NeoClrReferenceCatalog.ReadNative(corePath, []);
        var markers = new AssemblyBuilder(new("NativeMarkers", new(1, 0, 0, 0)), core.CoreIdentity);
        var marker = markers.AddClass("System", "ParamArrayAttribute");
        marker.AddConstructor(new MethodSignature(PrimitiveType.Void, [])).GetILGenerator().Fail("Metadata marker must never execute");
        var markerPath = Path.Combine(directory, "NativeMarkers.dll");
        File.WriteAllBytes(markerPath, RuntimeAssemblyContainer.WriteLibraryBinary(markers));
        var catalog = NeoClrReferenceCatalog.ReadNative(corePath, [markerPath]);
        var options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeTypeOfContract(null).WithTargetCoreAssemblyName(core.CoreIdentity.Name)
            .WithRuntimeUnitContract(new(core.CoreIdentity.Name, "System.Void"))
            .WithMetadataImportOptions(new MetadataImportOptions(core.CoreIdentity.Name)
                .WithObjectAssemblyName(core.CoreIdentity.Name).WithNativeMetadata());
        var source = "namespace ParameterArrays\npublic class Operations { static func First(params values: int[]) -> int => values[0] }";
        byte[] Emit(string name, string text, NeoClrReferenceCatalog references, CompilationOptions settings)
        {
            var compilation = Compilation.Create(name, [SyntaxTree.ParseText(text)], references.References.ToArray(), settings);
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
            using var image = new MemoryStream();
            var result = compilation.Emit(image, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
                new(new(name, new(1, 0, 0, 0)), core.CoreIdentity, references.Dependencies))));
            if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
            return image.ToArray();
        }
        var providerPath = Path.Combine(directory, "ParameterProvider.dll");
        var bytes = Emit("ParameterProvider", source, catalog, options);
        File.WriteAllBytes(providerPath, bytes);
        var emitted = AssemblyDefinition.ReadNativeAssembly(bytes);
        if (emitted.MainModule.AssemblyReferences.Any(a => a.Identity.Name == "NeoCLR.CoreProbe") ||
            emitted.MainModule.Types.Single(t => t.Name == "Operations").Methods.Single(m => m.Name == "First").ParameterArrayIndex != 0)
            throw new Exception("Native parameter-array metadata or dependency ownership was lost.");
        var consumerCatalog = NeoClrReferenceCatalog.ReadNative(corePath, [markerPath, providerPath]);
        var consumer = "func Main() -> int => ParameterArrays.Operations.First(42, 7)";
        File.WriteAllBytes(Path.Combine(directory, "Consumer.dll"), Emit("Consumer", consumer, consumerCatalog,
            options.WithOutputKind(OutputKind.ConsoleApplication)));
        var completionSource = "func Main() -> int { System. }";
        var tree = SyntaxTree.ParseText(completionSource);
        var completion = Compilation.Create("Completion", [tree], catalog.References.ToArray(), options);
        var names = new CompletionService().GetCompletions(completion, tree, completionSource.IndexOf("System.", StringComparison.Ordinal) + 7)
            .Select(item => item.DisplayText).ToArray();
        if (!names.Contains("ParamArrayAttribute") || names.Any(name => name.StartsWith("Action", StringComparison.Ordinal)))
            throw new Exception("Native marker completion missing or CLI symbols leaked.");
        Console.WriteLine("PASS native-only params provider/consumer emission, expanded call binding, marker metadata and System completion");
    }
}
