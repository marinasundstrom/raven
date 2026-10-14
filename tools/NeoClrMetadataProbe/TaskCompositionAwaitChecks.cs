using System.Text;
using System.Text.Json.Nodes;

using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class TaskCompositionAwaitChecks
{
    internal static void Run(string inputs, string output)
    {
        var coreBytes = File.ReadAllBytes(Path.Combine(inputs, "Core.dll"));
        var core = AssemblyDefinition.ReadAssembly(coreBytes, expectedExtended: false);
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(coreBytes);
        var library = NeoClrMetadataReference.ReadAssembly(File.ReadAllBytes(Path.Combine(inputs, "System.Runtime.dll")), bootstrap);
        var seed = NativeLibraryDefinition.ReadAssembly(File.ReadAllBytes(Path.Combine(inputs, "System.runtime.neox")));
        var options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeTypeOfContract(null).WithMetadataImportOptions(new MetadataImportOptions(core.Identity.Name).WithObjectAssemblyName(library.Definition.Name).WithAsyncAssemblyName(library.Definition.Name));
        NeoClrEmitOptions EmitOptions(string name, NeoClrMetadataReference? provider = null, bool enabled = true) => new(new(name, new(1, 0, 0, 0)), core.Identity,
            new[] { new NeoClrMetadataDependency(bootstrap.Reference, core, core.Identity, seed), new NeoClrMetadataDependency(library, core.Identity) }
                .Concat(provider is null ? [] : new[] { new NeoClrMetadataDependency(provider, core.Identity) }),
            bootstrapReference: bootstrap.Reference, allowExperimentalCompositionCalls: enabled);
        var alternatives = new[] { "Response", "HttpError", "JsonError" };
        var source = "import System.Concurrency.Tasks.*\n" + string.Join("\n", alternatives.Select(name => $"public class {name} {{}}"));
        foreach (var name in alternatives)
        {
            source += $$"""

                public func Produce{{name}}() -> Task<{{name}}> {
                    let promise = Promise<{{name}}>()
                    _ = promise.Complete({{name}}())
                    return promise.Task
                }
                public func Pending{{name}}() -> Task<{{name}}> {
                    let promise = Promise<{{name}}>()
                    TaskQueue.Default.Post(() => {
                        _ = promise.Complete({{name}}())
                    })
                    return promise.Task
                }
                public func Consume{{name}}(value: {{name}}) -> int { return 42 }
                """;
        }
        var producer = Compilation.Create("TaskCompositionProvider", [SyntaxTree.ParseText(source)], [bootstrap.Reference, library], options);
        using var nominal = new MemoryStream();
        var emitted = NeoClrCompilationEmitter.Emit(producer, nominal, EmitOptions("TaskCompositionProvider"));
        Check(emitted.Success, string.Join("\n", emitted.Diagnostics));
        Directory.CreateDirectory(output);
        File.WriteAllBytes(Path.Combine(output, "NominalProvider.json"), nominal.ToArray());
        var root = JsonNode.Parse(nominal.ToArray())!;
        var names = alternatives.Select(n => root["types"]!.AsArray()
            .Single(t => t!["name"]!.GetValue<string>().EndsWith("_" + Convert.ToHexString(Encoding.UTF8.GetBytes(n)), StringComparison.Ordinal))!["name"]!.GetValue<string>()).ToArray();
        JsonNode Union() => new JsonObject { ["Union"] = new JsonArray(names.Select(n => (JsonNode)new JsonObject { ["Named"] = n }).ToArray()) };
        void Rewrite(JsonNode? node)
        {
            if (node is JsonObject obj)
            {
                if (obj["Constructed"] is JsonObject construction && construction["arguments"] is JsonArray arguments)
                    for (int i = 0; i < arguments.Count; i++)
                        if (arguments[i]?["Named"]?.GetValue<string>() is { } bound && names.Contains(bound)) arguments[i] = Union();
                if (obj["name"]?.GetValueKind() == System.Text.Json.JsonValueKind.String &&
                    obj["name"]!.GetValue<string>().EndsWith(".M_436F6D706C657465", StringComparison.Ordinal) && obj["parameters"] is JsonArray parameters)
                    for (int i = 0; i < parameters.Count; i++)
                        if (parameters[i]?["Named"]?.GetValue<string>() is { } bound && names.Contains(bound)) parameters[i] = Union();
                foreach (var item in obj.ToArray()) Rewrite(item.Value);
            }
            else if (node is JsonArray array) foreach (var child in array) Rewrite(child);
        }
        foreach (var function in root["functions"]!.AsArray()) Rewrite(function);
        foreach (var type in root["types"]!.AsArray()) Rewrite(type);
        for (int i = 0; i < alternatives.Length; i++)
        {
            var consume = root["functions"]!.AsArray().Single(f => f!["origin"]?["name"]?.GetValue<string>() == "Consume" + alternatives[i])!;
            consume["parameters"]![0] = Union();
            // Verify the actual delivered alternative, not merely successful continuation.
            consume["body"] = new JsonArray(
                new JsonObject { ["op"] = "ldarg", ["arg"] = 0 },
                new JsonObject { ["op"] = "castclass", ["arg"] = new JsonObject { ["Named"] = names[i] } },
                new JsonObject { ["op"] = "ref.isnull" },
                new JsonObject { ["op"] = "brfalse", ["arg"] = 5 },
                new JsonObject { ["op"] = "fault", ["arg"] = "Await lost its payload" },
                new JsonObject { ["op"] = "ldc.i4", ["arg"] = 42 },
                new JsonObject { ["op"] = "ret" });
        }
        var image = NativeModuleContainer.WriteLibraryBinary(Encoding.UTF8.GetBytes(root.ToJsonString()));
        File.WriteAllBytes(Path.Combine(output, "TaskCompositionProvider.neox"), image);
        var reference = NeoClrMetadataReference.ReadExperimentalCompositionAssembly(image, bootstrap);
        var localConsumer = Compilation.Create("TaskCompositionLocal", [SyntaxTree.ParseText("func Main() -> int { let task = ProduceResponse(); let copy = task; return 42 }")],
            [bootstrap.Reference, library, reference], options.WithOutputKind(OutputKind.ConsoleApplication));
        using var localOutput = new MemoryStream();
        var localResult = NeoClrCompilationEmitter.Emit(localConsumer, localOutput, EmitOptions("TaskCompositionLocal", reference));
        Check(localResult.Success, string.Join("\n", localResult.Diagnostics));
        File.WriteAllBytes(Path.Combine(output, "TaskCompositionLocal.neox"), NativeModuleContainer.WriteLibraryBinary(localOutput.ToArray()));
        foreach (var alternative in alternatives)
            foreach (var pending in new[] { false, true })
            {
                var name = "Await" + (pending ? "Pending" : "Completed") + alternative;
                var body = $$"""
                import System.Concurrency.Tasks.*
                async func Main() -> Task<int> {
                    let task = {{(pending ? "Pending" : "Produce")}}{{alternative}}()
                    let value = await task
                    let another = await PendingResponse()
                    if ConsumeResponse(another) != 42 { return 1 }
                    return Consume{{alternative}}(value)
                }
                """;
                var consumer = Compilation.Create(name, [SyntaxTree.ParseText(body)], [bootstrap.Reference, library, reference], options.WithOutputKind(OutputKind.ConsoleApplication));
                Check(!consumer.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("\n", consumer.GetDiagnostics()));
                using var result = new MemoryStream();
                var emission = NeoClrCompilationEmitter.Emit(consumer, result, EmitOptions(name, reference));
                Check(emission.Success, string.Join("\n", emission.Diagnostics));
                var emittedImage = NativeModuleContainer.WriteLibraryBinary(result.ToArray());
                _ = AssemblyDefinition.ReadNativeAssembly(emittedImage, true);
                File.WriteAllBytes(Path.Combine(output, name + ".neox"), emittedImage);
                File.WriteAllText(Path.Combine(output, name + ".rvn"), body);
                using var denied = new MemoryStream();
                denied.WriteByte(42);
                Check(!NeoClrCompilationEmitter.Emit(consumer, denied, EmitOptions(name, reference, false)).Success &&
                    denied.ToArray().SequenceEqual(new byte[] { 42 }), "default gate preserves destination");
                Check(!NeoClrCompilationEmitter.EmitMetadataAssembly(consumer, denied, EmitOptions(name, reference)).Success &&
                    denied.ToArray().SequenceEqual(new byte[] { 42 }), "CLI gate preserves destination");
            }
        Console.WriteLine("PASS real Task composition source locals, completed/pending await emission and metadata round-trip");
    }
    private static void Check(bool success, string detail) { if (!success) throw new Exception(detail); }
}
