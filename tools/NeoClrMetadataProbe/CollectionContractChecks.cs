using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text.Json;

using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class CollectionContractChecks
{
    internal static async Task Run(string root, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        string[] paths = ["System/Disposable.rvn", "System/Collections/Iterator.rvn", "System/Collections/Iterable.rvn", "System/Collections/Collection.rvn", "System/Collections/Sequence.rvn"];
        var sources = paths.Select(p => File.ReadAllText(Path.Combine(root, "runtime/raven/src", p))).ToArray();
        const string consumer = """
            public interface Counted : System.Collections.Sequence<int> { }
            public interface Cursor : System.Collections.Iterator<int> { }
            public class EmptyCursor : Cursor {
                func MoveNext() -> bool => false
                val Current: int => 0
                func Dispose() { }
            }
            public class Provider : Counted {
                val Count: int => 40
                val self[index: int]: int => index + 2
                func GetIterator() -> System.Collections.Iterator<int> => EmptyCursor()
            }
            func Main() -> int {
                let collection: Counted = Provider()
                return collection.Count + collection[0]
            }
            """;
        var reports = new List<object>();
        foreach (var target in new[] { false, true })
        foreach (var reverse in new[] { false, true })
        {
            var corePath = target ? Path.Combine(root, "api-docs/reference/NeoCLR.CoreProbe.dll") : typeof(object).Assembly.Location;
            var reference = MetadataReference.CreateFromFile(corePath);
            var identity = AssemblyName.GetAssemblyName(corePath);
            var core = new AssemblyIdentity(identity.Name!, identity.Version!, "", Convert.ToHexString(identity.GetPublicKeyToken() ?? []));
            var options = target ? CompilationOptions.NeoCLR.WithOutputKind(OutputKind.ConsoleApplication)
                .WithRuntimeSelfTypeContract(new("NeoCLR.CoreProbe", "System.Runtime.CompilerServices.Self")) : new CompilationOptions(OutputKind.ConsoleApplication);
            var trees = paths.Select((p, i) => SyntaxTree.ParseText(sources[i], path: p)).Append(SyntaxTree.ParseText(consumer, path: "Main.rvn")).ToArray();
            if (reverse) Array.Reverse(trees);
            var name = "CollectionContracts" + (target ? "Target" : "Host") + (reverse ? "Reverse" : "Forward");
            var compilation = Compilation.Create(name, trees, [reference], options);
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (errors.Length != 0) throw new Exception(string.Join("; ", errors.Select(d => d.ToString())));
            using var native = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, native, new(new(name, new Version(1, 0, 0, 0)), core, []));
            if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics.Select(d => d + " at " + d.Location.SourceTree?.GetText().ToString(d.Location.SourceSpan))));
            var projection = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.ReadCliProjection(native.ToArray());
            if (projection.MainModule.Types.Single(t => t.Name == "Collection`1").Methods.Single().Name != "get_Count") throw new Exception("collection projection lost");
            var indexer = projection.MainModule.Types.Single(t => t.Name == "Sequence`1").Properties.Single();
            if (indexer.GetMethod is null || indexer.SetMethod is not null || indexer.GetMethod.IsStatic) throw new Exception("sequence indexer association lost");
            var path = Path.Combine(output, name + ".dll"); File.WriteAllBytes(path, native.ToArray());
            foreach (var command in new[] { "verify", "run" })
            {
                var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                start.ArgumentList.Add(command); start.ArgumentList.Add(path);
                using var process = Process.Start(start)!; var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                await process.WaitForExitAsync(); var text = await stdout + await stderr;
                if (process.ExitCode != (command == "verify" ? 0 : 42)) throw new Exception(command + ": " + text);
            }
            if (!target)
            {
                using var cli = new MemoryStream(); var emitted = compilation.Emit(cli);
                if (!emitted.Success) throw new Exception(string.Join("; ", emitted.Diagnostics));
                var loaded = Assembly.Load(cli.ToArray());
                if (!Equals(42, loaded.EntryPoint!.Invoke(null, null))) throw new Exception("CLR inherited dispatch");
                if (loaded.GetType("Counted")!.GetInterfaces().Length != 3) throw new Exception("CLR inherited interface chain");
            }
            reports.Add(new { name, target, reverse, nativeResult = 42, cliExecution = !target, bytes = native.Length, coreSha256 = Hash(corePath) });
        }
        File.WriteAllText(Path.Combine(output, "Main.rvn"), consumer);
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            sources = paths.Select((p, i) => new { path = "runtime/raven/src/" + p, sha256 = Convert.ToHexString(SHA256.HashData(System.Text.Encoding.UTF8.GetBytes(sources[i]))) }),
            runtimeSha256 = Hash(runtime),
            consumer,
            cases = reports,
            scope = "unchanged sequence interfaces with same-assembly consumer; inherited generic property and indexer dispatch, not collection storage implementation or full class library"
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS unchanged sequence contracts and inherited property/indexer dispatch: CLR/native 42, both source orders and native target");
    }
    static string Hash(string path) => Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path)));
}
