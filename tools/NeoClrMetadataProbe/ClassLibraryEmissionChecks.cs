using System.Security.Cryptography;
using System.Text.Json;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

// An exploratory acceptance report, not a test that requires current limitations to persist.
internal static class ClassLibraryEmissionChecks
{
    internal static void Run(string sourceRoot, string output)
    {
        sourceRoot = Path.GetFullPath(sourceRoot);
        output = Path.GetFullPath(output);
        if (Directory.Exists(output)) throw new IOException("output directory must be fresh");
        Directory.CreateDirectory(output);
        var host = typeof(object).Assembly.GetName();
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "",
            Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        var reference = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
        var reports = new List<object>();
        foreach (var relative in new[] { "System/Math/Functions.rvn", "System/Text/UnicodeScalar.rvn", "System/Runtime/GC.rvn", "System/Globalization/Language.rvn", "System/Collections/Comparer.rvn", "System/Collections/EqualityComparer.rvn", "System/Collections/ArrayList.rvn" })
        {
            var path = Path.Combine(sourceRoot, relative);
            var original = File.ReadAllText(path);
            Attempt(relative, original, original, "whole-file");
            if (relative == "System/Math/Functions.rvn")
            {
                var selected = SelectIntegerMath(original, path);
                Attempt(relative, original, selected, "Min-Max-Sign");
            }
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            bootstrap = "host core primitives only; no native class-library symbol loader",
            runtimeExecution = "not performed by this emission inventory",
            cases = reports
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");

        void Attempt(string relative, string original, string source, string selection)
        {
            var name = "ClassLibraryPart" + reports.Count;
            File.WriteAllText(Path.Combine(output, name + ".rvn"), source);
            var compilation = Compilation.Create(name,
                [SyntaxTree.ParseText(source, path: relative)], [reference],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            var semantic = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            using var image = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new(name, new Version(1, 0, 0, 0)), core, []));
            if (emitted.Success) File.WriteAllBytes(Path.Combine(output, name + ".dll"), image.ToArray());
            else if (image.Length != 0) throw new Exception("failed emission wrote output");
            var phase = semantic.Length != 0 ? "binding" : emitted.Success ? "emitted" : "emission";
            reports.Add(new
            {
                source = relative,
                selection,
                sourceSha256 = Convert.ToHexString(SHA256.HashData(System.Text.Encoding.UTF8.GetBytes(original))),
                selectedSha256 = Convert.ToHexString(SHA256.HashData(System.Text.Encoding.UTF8.GetBytes(source))),
                phase,
                success = emitted.Success,
                bytes = image.Length,
                diagnostics = emitted.Diagnostics.Select(d => d.ToString()).ToArray()
            });
            Console.WriteLine($"{relative} ({selection}): {phase}; {emitted.Diagnostics.Length} diagnostics");
        }
    }
    private static string SelectIntegerMath(string original, string path)
    {
        var tree = SyntaxTree.ParseText(original, path: path);
        var names = new[] { "Min", "Max", "Sign" };
        var functions = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>()
            .Where(f => names.Contains(f.Identifier.ValueText) &&
                f.ParameterList.Parameters.All(p => p.TypeAnnotation?.Type.ToString() == "int")).ToArray();
        if (functions.Length != names.Length) throw new InvalidDataException("Math selection changed; review the source selection");
        // Retain the original namespace line and exact declaration text. Only unrelated
        // declarations and their imports are excluded from this partial compilation.
        return original[..original.IndexOf('\n')] + "\n\n" +
            string.Join("\n", functions.Select(f => f.ToFullString()));
    }

    internal static async Task RunRuntime(string sourceRoot, string output, string runtime)
    {
        output = Path.GetFullPath(output);
        if (Directory.Exists(output)) throw new IOException("output directory must be fresh");
        Directory.CreateDirectory(output);
        var path = Path.Combine(sourceRoot, "System/Math/Functions.rvn");
        var original = File.ReadAllText(path);
        var source = SelectIntegerMath(original, path);
        File.WriteAllText(Path.Combine(output, "Math.rvn"), source);
        const string entry = """
            namespace System.Math
            func Main() -> int {
                if Min(-2147483648, 2147483647) != -2147483648 { return 1 }
                if Min(2147483647, -2147483648) != -2147483648 { return 2 }
                if Min(7, 7) != 7 { return 3 }
                if Max(-2147483648, 2147483647) != 2147483647 { return 4 }
                if Max(2147483647, -2147483648) != 2147483647 { return 5 }
                if Max(-7, -7) != -7 { return 6 }
                if Sign(-2147483648) != -1 { return 7 }
                if Sign(2147483647) != 1 { return 8 }
                if Sign(0) != 0 { return 9 }
                if Sign(-1) != -1 { return 10 }
                if Sign(1) != 1 { return 11 }
                return 42
            }
            """;
        File.WriteAllText(Path.Combine(output, "Main.rvn"), entry);
        var host = typeof(object).Assembly.GetName();
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        var reference = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
        foreach (var reverse in new[] { false, true })
        {
            var trees = new[] { SyntaxTree.ParseText(source, path: path), SyntaxTree.ParseText(entry, path: "Main.rvn") };
            if (reverse) Array.Reverse(trees);
            var name = "RealMath" + (reverse ? "Reverse" : "Forward");
            var compilation = Compilation.Create(name, trees, [reference], new CompilationOptions(OutputKind.ConsoleApplication));
            using var native = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, native,
                new(new(name, new Version(1, 0, 0, 0)), core, []));
            if (!result.Success) throw new Exception(string.Join("; ", result.Diagnostics));
            using (var declarations = JsonDocument.Parse(NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.Read(native.ToArray())))
            {
                foreach (var function in declarations.RootElement.GetProperty("functions").EnumerateArray())
                    if (function.GetProperty("namespace").GetString() != "System.Math" || function.GetProperty("owner").ValueKind != JsonValueKind.Null)
                        throw new Exception("Math namespace/owner not preserved");
            }
            var file = Path.Combine(output, name + ".dll"); File.WriteAllBytes(file, native.ToArray());
            foreach (var command in new[] { "verify", "run" })
            {
                var start = new System.Diagnostics.ProcessStartInfo(runtime) { RedirectStandardOutput = true, RedirectStandardError = true };
                start.ArgumentList.Add(command); start.ArgumentList.Add(file);
                using var process = System.Diagnostics.Process.Start(start)!;
                var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
                await process.WaitForExitAsync();
                var text = await stdout + await stderr;
                if (process.ExitCode != (command == "verify" ? 0 : 42)) throw new Exception(text);
            }
            using var cli = new MemoryStream();
            var dotnet = compilation.Emit(cli);
            if (!dotnet.Success) throw new Exception(string.Join("; ", dotnet.Diagnostics));
            if (!Equals(System.Reflection.Assembly.Load(cli.ToArray()).EntryPoint!.Invoke(null, null), 42)) throw new Exception("CLI Math result");
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            source = "runtime/raven/src/System/Math/Functions.rvn",
            sourceSha256 = Convert.ToHexString(SHA256.HashData(System.Text.Encoding.UTF8.GetBytes(original))),
            selection = "original Int32 Min/Max/Sign declarations and System.Math namespace",
            result = 42,
            cases = 11,
            bothFileOrders = true,
            cliExecution = true,
            nativeBinaryExecution = true,
            runtimeSha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(runtime))),
            limitations = "host core primitive bootstrap; selected declarations only; no full class-library build or native symbol importer"
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine("PASS actual Math source: 11 boundary checks, both file orders, CLI and binary native execution");
    }

}
