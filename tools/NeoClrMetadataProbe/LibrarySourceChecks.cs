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
    internal static void Run(string root, string output, string seed, string nativeSystem, string? consumer = null, string[]? sourcePaths = null)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var identity = AssemblyName.GetAssemblyName(seed);
        var core = new AssemblyIdentity(identity.Name!, identity.Version!, identity.CultureName ?? "",
            Convert.ToHexString(identity.GetPublicKeyToken() ?? []));
        var reference = MetadataReference.CreateFromFile(seed);
        var paths = sourcePaths ?? new[] { "System/Disposable.rvn", "System/Collections/Iterator.rvn", "System/Collections/Iterable.rvn",
            "System/Collections/Collection.rvn", "System/Collections/Sequence.rvn", "System/Collections/MutableSequence.rvn",
            "System/Collections/List.rvn", "System/Collections/ArrayList.rvn" };
        var trees = paths.Select(p => SyntaxTree.ParseText(File.ReadAllText(Path.Combine(root, "runtime/raven/src", p)), path: p)).ToArray();
        if (consumer is not null) trees = [.. trees, SyntaxTree.ParseText(consumer, path: "Consumer.rvn")];
        var compilation = Compilation.Create("LibrarySource", trees, [reference],
            CompilationOptions.NeoCLR.WithOutputKind(consumer is null ? OutputKind.DynamicallyLinkedLibrary : OutputKind.ConsoleApplication)
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
            consumer, phase, bytes, bindingDiagnosticCount, cliBridgeEmission, diagnostics = errors,
            scope = "unchanged library source units; explicit authoring seed; execution, when requested, is recorded separately in execution.json"
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine(phase + ": " + string.Join("; ", errors));
    }
    internal static async Task RunRuntime(string root, string output, string seed, string nativeSystem, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var cases = new[] {
            (Name: "growth-copy-iterator", Source: """
                import System.Option.*

                func Main() -> int {
                    let values = System.Collections.ArrayList<int>()
                    var index = 1
                    while index <= 6 {
                        values.Add(index)
                        index = index + 1
                    }
                    if values.Count != 6 { return 1 }
                    if values.Capacity < 6 { return 2 }
                    let copy = values.Copy()
                    values[0] = 9
                    if copy[0] != 1 { return 3 }
                    let iterator = copy.GetIterator()
                    var sum = 0
                    while iterator.MoveNext() { sum = sum + iterator.Current }
                    iterator.Dispose()
                    if sum != 21 { return 4 }
                    let filtered = copy.FindAll(value => value > 3)
                    if filtered.Count != 3 { return 5 }
                    if filtered[0] != 4 { return 6 }
                    if !copy.Exists(value => value == 4) { return 7 }
                    if !copy.TrueForAll(value => value > 0) { return 8 }
                    let first = match copy.Find(value => value == 4) {
                        Some(let found) => found
                        None => -1
                    }
                    if first != 4 { return 9 }
                    let absent = match copy.Find(value => value == 99) {
                        Some(let found) => false
                        None => true
                    }
                    if !absent { return 10 }
                    let last = match copy.FindLast(value => value > 3) {
                        Some(let found) => found
                        None => -1
                    }
                    if last != 6 { return 11 }
                    let firstIndex = match copy.FindIndex(value => value > 3) {
                        Some(let position) => position
                        None => -1
                    }
                    if firstIndex != 3 { return 12 }
                    let lastIndex = match copy.FindLastIndex(value => value > 3) {
                        Some(let position) => position
                        None => -1
                    }
                    if lastIndex != 5 { return 13 }
                    return 42
                }
                """, Message: (string?)null),
            (Name: "negative-capacity", Source: """
                func Main() -> int {
                    let values = System.Collections.ArrayList<int>(-1)
                    return values.Count
                }
                """, Message: "ArrayList capacity must be non-negative"),
            (Name: "invalid-index", Source: """
                func Main() -> int {
                    let values = System.Collections.ArrayList<int>()
                    return values[0]
                }
                """, Message: "ArrayList index out of range")
        };
        await ExecuteCases(root, output, seed, nativeSystem, runtime, cases);
    }

    internal static async Task RunComparers(string root, string output, string seed, string nativeSystem, string runtime)
    {
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var cases = new[] { (Name: "callback-policies", Source: """
            func Main() -> int {
                let ordering = System.Collections.FunctionComparer<int>((left, right) => left - right + 5)
                let policy: System.Collections.Comparer<int> = ordering
                if policy.Compare(7, 10) != 2 { return 1 }
                if ordering.Compare(3, 3) != 5 { return 2 }
                let equality = System.Collections.FunctionEqualityComparer<int>((left, right) => left % 10 == right % 10, value => value % 10)
                let equalPolicy: System.Collections.EqualityComparer<int> = equality
                if !equalPolicy.Equals(12, 22) { return 3 }
                if equalPolicy.Equals(12, 23) { return 4 }
                if equalPolicy.GetHashCode(32) != 2 { return 5 }
                return 42
            }
            """, Message: (string?)null) };
        await ExecuteCases(root, output, seed, nativeSystem, runtime, cases,
            ["System/Collections/Comparer.rvn", "System/Collections/EqualityComparer.rvn",
             "System/Collections/FunctionComparer.rvn", "System/Collections/FunctionEqualityComparer.rvn"]);
    }

    private static async Task ExecuteCases(string root, string output, string seed, string nativeSystem,
        string runtime, (string Name, string Source, string? Message)[] cases, string[]? sourcePaths = null)
    {
        var reports = new List<object>();
        foreach (var test in cases)
        {
            var directory = Path.Combine(output, test.Name);
            Run(root, directory, seed, nativeSystem, test.Source, sourcePaths);
            var image = Path.Combine(directory, "LibrarySource.dll");
            if (!File.Exists(image)) throw new Exception("source implementation did not emit: " + directory);
            foreach (var command in new[] { "verify", "run" })
            {
                var start = new System.Diagnostics.ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                foreach (var arg in new[] { command, image, "--system", nativeSystem }) start.ArgumentList.Add(arg);
                using var process = System.Diagnostics.Process.Start(start)!;
                var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                await process.WaitForExitAsync(); var text = await stdout + await stderr;
                var passed = command == "verify" ? process.ExitCode == 0 : test.Message is null ? process.ExitCode == 42 : process.ExitCode != 0 && text.Contains(test.Message, StringComparison.Ordinal);
                reports.Add(new { test.Name, command, passed, exitCode = process.ExitCode, text });
                File.WriteAllText(Path.Combine(output, "execution.json"), JsonSerializer.Serialize(new { runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))), cases = reports }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
                if (!passed) throw new Exception(test.Name + " " + command + ": " + text);
            }
        }
        Console.WriteLine("PASS unchanged library source consumers");
    }

}
