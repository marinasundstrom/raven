using System.Diagnostics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text.Json;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

// Exploratory report: current failures are evidence, never required test expectations.
internal static class ReadinessInventory
{
    internal static async Task Run(string root, string output, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh"); Directory.CreateDirectory(output);
        var library = Path.Combine(root, "runtime/raven/src");
        var samples = Path.Combine(root, "docs/experiments/raven-target/samples");
        var corePath = Path.Combine(root, "api-docs/reference/NeoCLR.CoreProbe.dll");
        var targetReference = MetadataReference.CreateFromFile(corePath);
        var hostReference = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
        var reports = new List<object>();
        var contracts = new[] { "System/Disposable.rvn", "System/Collections/Iterator.rvn", "System/Collections/Iterable.rvn" };
        foreach (var (label, files) in new (string, string[])[] {
            ("iterator-contracts", contracts),
            ("collection-contract", [..contracts, "System/Collections/Collection.rvn"]),
            ("sequence-contract", [..contracts, "System/Collections/Collection.rvn", "System/Collections/Sequence.rvn"]),
            ("language", ["System/Globalization/Language.rvn"]),
            ("array-list", ["System/Collections/ArrayList.rvn"]),
            ("option", ["System/Option.rvn"]),
            ("result", ["System/Result.rvn"]),
            ("math", ["System/Math/Functions.rvn"]),
            ("gc", ["System/Runtime/GC.rvn"])
        })
        {
            await Attempt(label, files.Select(f => Path.Combine(library, f)).ToArray(), false, false);
            await Attempt(label, files.Select(f => Path.Combine(library, f)).ToArray(), false, true);
        }
        foreach (var name in new[] { "application-interfaces", "application-inheritance", "application-types", "application-delegates", "application-iterable", "application-order-collections",
            "library-arrays", "library-math", "library-option", "library-result", "library-async-default-queue", "library-strings" })
            await Attempt(name, [Path.Combine(samples, name + ".rvn")], true, true);
        await Attempt("whole-library", Directory.GetFiles(library, "*.rvn", SearchOption.AllDirectories).Order().ToArray(), false, true);
        Save();

        void Save() => File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            targetReference = "api-docs/reference/NeoCLR.CoreProbe.dll",
            targetReferenceSha256 = Hash(corePath),
            runtimeSha256 = Hash(runtime),
            targetProfile = "CompilationOptions.NeoCLR plus native Self marker; CLI declaration snapshot, not a native symbol importer",
            hostProfile = "Existing direct-emitter .NET primitive bootstrap; no native library dependencies",
            methodology = "Unchanged sources; grouped foundational contracts, individual library units, selected applications and entire library. Binding failures stop emission. Target configuration rejection is recorded, not bypassed. No application with failed emission is run. Ordinary CLI bridge emission is a nonexecuted control using a reference-only core. Full diagnostics saved per case; report retains first 12.",
            cases = reports
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        async Task Attempt(string label, string[] paths, bool executable, bool target)
        {
            var id = label + (target ? "-target" : "-host");
            var kind = executable ? OutputKind.ConsoleApplication : OutputKind.DynamicallyLinkedLibrary;
            var options = target ? CompilationOptions.NeoCLR.WithOutputKind(kind).WithRuntimeSelfTypeContract(new("NeoCLR.CoreProbe", "System.Runtime.CompilerServices.Self")) : new CompilationOptions(kind);
            var reference = target ? targetReference : hostReference;
            var identity = AssemblyName.GetAssemblyName(target ? corePath : typeof(object).Assembly.Location);
            var core = new AssemblyIdentity(identity.Name!, identity.Version!, identity.CultureName ?? "", Convert.ToHexString(identity.GetPublicKeyToken() ?? []));
            string phase = "binding"; string[] diagnostics = []; object? execution = null; object? cliBridgeEmission = null; long bytes = 0;
            try
            {
                var compilation = Compilation.Create("Inventory" + reports.Count, paths.Select(p => SyntaxTree.ParseText(File.ReadAllText(p), path: Path.GetRelativePath(root, p))).ToArray(), [reference], options);
                diagnostics = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).Select(d => d.ToString()).ToArray();
                if (diagnostics.Length == 0)
                {
                    phase = "emission"; using var image = new MemoryStream();
                    var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image, new(new(compilation.AssemblyName!, new Version(1, 0, 0, 0)), core, []));
                    diagnostics = emitted.Diagnostics.Select(d => d.ToString()).ToArray(); bytes = image.Length;
                    if (target)
                    {
                        try
                        {
                            using var cli = new MemoryStream();
                            var result = compilation.Emit(cli);
                            var cliDiagnostics = result.Diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error).Select(d => d.ToString()).ToArray();
                            cliBridgeEmission = new { success = result.Success, bytes = cli.Length, diagnostics = cliDiagnostics.Take(12).ToArray() };
                            File.WriteAllLines(Path.Combine(output, id + ".cli-diagnostics.txt"), cliDiagnostics);
                            if (result.Success) File.WriteAllBytes(Path.Combine(output, id + ".cli.dll"), cli.ToArray());
                        }
                        catch (Exception e) { cliBridgeEmission = new { exception = e.GetType().Name + ": " + e.Message }; }
                    }
                    if (emitted.Success)
                    {
                        phase = "emitted"; var path = Path.Combine(output, id + ".dll"); File.WriteAllBytes(path, image.ToArray());
                        var verification = await Command("verify", path);
                        var run = executable && verification.ExitCode == 0 ? await Command("run", path) : null;
                        execution = new { verification, run };
                    }
                }
            }
            catch (Exception e) { phase = "exception"; diagnostics = [e.GetType().Name + ": " + e.Message]; }
            File.WriteAllLines(Path.Combine(output, id + ".diagnostics.txt"), diagnostics);
            reports.Add(new
            {
                id,
                profile = target ? "target" : "host",
                sources = paths.Select(p => new { path = Path.GetRelativePath(root, p), sha256 = Hash(p) }),
                phase,
                bytes,
                diagnosticCount = diagnostics.Length,
                diagnostics = diagnostics.Take(12).ToArray(),
                cliBridgeEmission,
                execution
            });
            Save(); Console.WriteLine($"{id}: {phase}; {diagnostics.Length} diagnostics");
        }
        async Task<CommandResult> Command(string command, string path)
        {
            var start = new ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
            start.ArgumentList.Add(command); start.ArgumentList.Add(path);
            using var process = Process.Start(start)!; var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
            using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(30));
            try { await process.WaitForExitAsync(timeout.Token); }
            catch (OperationCanceledException) { process.Kill(entireProcessTree: true); throw new TimeoutException("runtime " + command + " exceeded 30 seconds"); }
            return new(process.ExitCode, await stdout, await stderr);
        }
    }
    private sealed record CommandResult(int ExitCode, string Stdout, string Stderr);
    private static string Hash(string path) => Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path)));
}
