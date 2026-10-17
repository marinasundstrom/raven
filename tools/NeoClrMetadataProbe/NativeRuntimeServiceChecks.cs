using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeRuntimeServiceChecks
{
    internal static void Run(string corePath, string directory)
    {
        Directory.CreateDirectory(directory);
        var catalog = NeoClrReferenceCatalog.ReadNative(corePath, []);
        var options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeTypeOfContract(null).WithTargetCoreAssemblyName(catalog.CoreIdentity.Name)
            .WithRuntimeUnitContract(new(catalog.CoreIdentity.Name, "System.Void"))
            .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name)
                .WithObjectAssemblyName(catalog.CoreIdentity.Name).WithNativeMetadata());
        byte[] Emit(string name, string text, NeoClrReferenceCatalog references, bool success = true)
        {
            var compilation = Compilation.Create(name, [SyntaxTree.ParseText(text)], references.References.ToArray(), options);
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
            using var image = new MemoryStream();
            var result = compilation.Emit(image, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
                new(new(name, new(1, 0, 0, 0)), catalog.CoreIdentity, references.Dependencies))));
            if (result.Success != success) throw new Exception(string.Join("\n", result.Diagnostics));
            if (!success && image.Length != 0) throw new Exception("Rejected service published output.");
            return image.ToArray();
        }
        var source = "module neoCLR.Runtime\nimport System.Runtime.CompilerServices.*\n[MethodImpl(4096)]\ninternal extern func GetDefaultTaskQueue<T>() -> T;\n";
        var authored = Emit("SourceServices", source, catalog);
        File.WriteAllBytes(Path.Combine(directory, "SourceServices.dll"), authored);
        var definition = AssemblyDefinition.ReadNativeAssembly(authored).MainModule.Methods.Single(m => m.Name == "GetDefaultTaskQueue");
        if (definition.GenericArity != 1 || definition.ImplementationAttributes != 0x1000)
            throw new Exception("Native generic service declaration lost its contract.");
        Emit("RejectedService", source.Replace("4096", "0"), catalog, false);
        Emit("RejectedService", source.Replace("internal extern", "public extern"), catalog, false);
        var graph = new AssemblyBuilder(new("ExportedServices", new(1, 0, 0, 0)), catalog.CoreIdentity);
        graph.AddFunction("neoCLR.Runtime", "GetDefaultTaskQueue", new(SignatureType.MethodParameter(0), [], ["T"])).SetInternalCall();
        var provider = Path.Combine(directory, "ExportedServices.dll");
        File.WriteAllBytes(provider, RuntimeAssemblyContainer.WriteLibraryBinary(graph));
        var imported = NeoClrReferenceCatalog.ReadNative(corePath, [provider]);
        File.WriteAllBytes(Path.Combine(directory, "Consumer.dll"), Emit("Consumer",
            "func ReadQueue<T>() -> T => neoCLR.Runtime.GetDefaultTaskQueue<T>()", imported));
        var completionSource = "func Main() -> int { neoCLR.Runtime. }";
        var tree = SyntaxTree.ParseText(completionSource);
        var completion = Compilation.Create("Completion", [tree], imported.References.ToArray(), options);
        var names = new CompletionService().GetCompletions(completion, tree,
            completionSource.IndexOf("neoCLR.Runtime.", StringComparison.Ordinal) + "neoCLR.Runtime.".Length)
            .Select(item => item.DisplayText).ToArray();
        if (!names.Any(name => name.StartsWith("GetDefaultTaskQueue", StringComparison.Ordinal)))
            throw new Exception("Native generic runtime service absent from completion.");
        Console.WriteLine("PASS native generic runtime-service declaration/import, invalid flag/visibility rejection and completion");
    }
}
