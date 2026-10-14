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
        NeoClrEmitOptions EmitOptions(string name, NeoClrMetadataReference? provider = null) => new(new(name, new(1, 0, 0, 0)), core.Identity,
            new[] { new NeoClrMetadataDependency(bootstrap.Reference, core, core.Identity, seed), new NeoClrMetadataDependency(library, core.Identity) }
                .Concat(provider is null ? [] : new[] { new NeoClrMetadataDependency(provider, core.Identity) }),
            bootstrapReference: bootstrap.Reference, allowExperimentalCompositionCalls: true);
        var producer = Compilation.Create("TaskCompositionProvider", [SyntaxTree.ParseText("""
            import System.Concurrency.Tasks.*
            public class Response {}
            public class HttpError {}
            public class JsonError {}
            public func Produce() -> Task<Response> {
                let promise = Promise<Response>()
                _ = promise.Complete(Response())
                return promise.Task
            }
            public func Consume(value: Response) -> int { return 42 }
            """)], [bootstrap.Reference, library], options);
        using var nominal = new MemoryStream();
        var emitted = NeoClrCompilationEmitter.Emit(producer, nominal, EmitOptions("TaskCompositionProvider"));
        Check(emitted.Success, string.Join("\n", emitted.Diagnostics));
        Directory.CreateDirectory(output);
        File.WriteAllBytes(Path.Combine(output, "NominalProvider.json"), nominal.ToArray());
        var root = JsonNode.Parse(nominal.ToArray())!;
        var names = new[] { "Response", "HttpError", "JsonError" }.Select(n => root["types"]!.AsArray()
            .Single(t => t!["name"]!.GetValue<string>().EndsWith("_" + Convert.ToHexString(Encoding.UTF8.GetBytes(n)), StringComparison.Ordinal))!["name"]!.GetValue<string>()).ToArray();
        JsonNode Union() => new JsonObject { ["Union"] = new JsonArray(names.Select(n => (JsonNode)new JsonObject { ["Named"] = n }).ToArray()) };
        void Rewrite(JsonNode? node)
        {
            if (node is JsonObject obj)
            {
                if (obj["Constructed"] is JsonObject construction && construction["arguments"] is JsonArray arguments)
                    for (int i = 0; i < arguments.Count; i++)
                        if (arguments[i]?["Named"]?.GetValue<string>() == names[0]) arguments[i] = Union();
                if (obj["name"]?.GetValueKind() == System.Text.Json.JsonValueKind.String &&
                    obj["name"]!.GetValue<string>().EndsWith(".M_436F6D706C657465", StringComparison.Ordinal) && obj["parameters"] is JsonArray parameters)
                    for (int i = 0; i < parameters.Count; i++)
                        if (parameters[i]?["Named"]?.GetValue<string>() == names[0]) parameters[i] = Union();
                foreach (var item in obj.ToArray()) Rewrite(item.Value);
            }
            else if (node is JsonArray array) foreach (var child in array) Rewrite(child);
        }
        foreach (var function in root["functions"]!.AsArray()) Rewrite(function);
        var consume = root["functions"]!.AsArray().Single(f => f!["name"]!.GetValue<string>().EndsWith("F_436F6E73756D65", StringComparison.Ordinal))!;
        consume["parameters"]![0] = Union();
        var image = NativeModuleContainer.WriteLibraryBinary(Encoding.UTF8.GetBytes(root.ToJsonString()));
        File.WriteAllBytes(Path.Combine(output, "TaskCompositionProvider.neox"), image);
        var reference = NeoClrMetadataReference.ReadExperimentalCompositionAssembly(image, bootstrap);
        var localConsumer = Compilation.Create("TaskCompositionLocal", [SyntaxTree.ParseText("func Main() -> int { let task = Produce(); let copy = task; return 42 }")],
            [bootstrap.Reference, library, reference], options.WithOutputKind(OutputKind.ConsoleApplication));
        using var localOutput = new MemoryStream();
        var localResult = NeoClrCompilationEmitter.Emit(localConsumer, localOutput, EmitOptions("TaskCompositionLocal", reference));
        Check(localResult.Success, string.Join("\n", localResult.Diagnostics));
        File.WriteAllBytes(Path.Combine(output, "TaskCompositionLocal.neox"), NativeModuleContainer.WriteLibraryBinary(localOutput.ToArray()));
        var consumer = Compilation.Create("TaskCompositionAwait", [SyntaxTree.ParseText("""
            import System.Concurrency.Tasks.*
            async func Main() -> Task<int> {
                let task = Produce()
                let value = await task
                return Consume(value)
            }
            """)], [bootstrap.Reference, library, reference], options.WithOutputKind(OutputKind.ConsoleApplication));
        Check(!consumer.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("\n", consumer.GetDiagnostics()));
        using var result = new MemoryStream();
        var emission = NeoClrCompilationEmitter.Emit(consumer, result, EmitOptions("TaskCompositionAwait", reference));
        Check(!emission.Success && result.Length == 0 && emission.Diagnostics.Any(d => d.ToString().Contains("unsupported instance field assignment", StringComparison.Ordinal)),
            "await must reach the documented state-machine storage boundary: " + string.Join("\n", emission.Diagnostics));
        Console.WriteLine("PASS real Task composition source local; await binds and reaches state-machine storage boundary");
    }
    private static void Check(bool success, string detail) { if (!success) throw new Exception(detail); }
}
