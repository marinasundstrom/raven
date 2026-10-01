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
        foreach (var relative in new[] { "System/Math/Functions.rvn", "System/Text/UnicodeScalar.rvn", "System/Runtime/GC.rvn" })
        {
            var path = Path.Combine(sourceRoot, relative);
            var original = File.ReadAllText(path);
            Attempt(relative, original, original, "whole-file");
            if (relative == "System/Math/Functions.rvn")
            {
                var tree = SyntaxTree.ParseText(original, path: path);
                var names = new[] { "Min", "Max", "Sign" };
                var functions = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>()
                    .Where(f => names.Contains(f.Identifier.ValueText) &&
                        f.ParameterList.Parameters.All(p => p.TypeAnnotation?.Type.ToString() == "int")).ToArray();
                if (functions.Length != names.Length) throw new InvalidDataException("Math selection changed; review the source selection");
                // Retain the original namespace line and exact declaration text. Only unrelated
                // declarations and their imports are excluded from this partial compilation.
                var selected = original[..original.IndexOf('\n')] + "\n\n" +
                    string.Join("\n", functions.Select(f => f.ToFullString()));
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
}
