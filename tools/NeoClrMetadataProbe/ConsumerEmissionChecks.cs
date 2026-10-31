using System.Security.Cryptography;
using System.Text;
using System.Text.Json;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

// Reports the actual consumer frontier without freezing unsupported features into tests.
internal static class ConsumerEmissionChecks
{
    internal static void Run(string sourcePath, string output)
    {
        sourcePath = Path.GetFullPath(sourcePath);
        output = Path.GetFullPath(output);
        if (Directory.Exists(output)) throw new IOException("output directory must be fresh");
        Directory.CreateDirectory(output);
        var original = File.ReadAllText(sourcePath);
        var tree = SyntaxTree.ParseText(original, path: sourcePath);
        var order = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()
            .Single(c => c.Identifier.ValueText == "Order");
        // This seed has a global-namespace Order with primitive-only declarations.
        // Fail visibly if it changes shape rather than silently discarding namespace context.
        if (order.Parent is not CompilationUnitSyntax) throw new InvalidDataException("Order selection now needs namespace context");
        var host = typeof(object).Assembly.GetName();
        var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
        var references = new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) };
        var reports = new List<object>();
        foreach (var (selection, source) in new[] { ("whole-consumer", original), ("Order-declaration", order.ToFullString()) })
        {
            var name = "OrderConsumer" + reports.Count;
            File.WriteAllText(Path.Combine(output, name + ".rvn"), source);
            var selected = SyntaxTree.ParseText(source, path: sourcePath);
            var compilation = Compilation.Create(name, [selected], references,
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            var bindingErrors = compilation.GetDiagnostics().Count(d => d.Severity == DiagnosticSeverity.Error);
            using var image = new MemoryStream();
            var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new(name, new Version(1, 0, 0, 0)), core, []));
            if (emitted.Success) File.WriteAllBytes(Path.Combine(output, name + ".dll"), image.ToArray());
            else if (image.Length != 0) throw new Exception("failed emission wrote output");
            var symbol = compilation.GetSemanticModel(selected).GetDeclaredSymbol(
                selected.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single(c => c.Identifier.ValueText == "Order")) as INamedTypeSymbol;
            var phase = bindingErrors > 0 ? "binding" : emitted.Success ? "emitted" : "emission";
            reports.Add(new
            {
                selection,
                phase,
                bindingErrors,
                success = emitted.Success,
                bytes = image.Length,
                selectedSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(source))),
                orderMembers = symbol?.GetMembers().Select(m => new { m.Name, kind = m.Kind.ToString(), m.IsStatic }).ToArray(),
                diagnosticCount = emitted.Diagnostics.Length,
                diagnostics = emitted.Diagnostics.Take(32).Select(d => d.ToString()).ToArray()
            });
            Console.WriteLine($"Order consumer ({selection}): {phase}, {bindingErrors} binding errors, {image.Length} bytes");
        }
        File.WriteAllText(Path.Combine(output, "validation.json"), JsonSerializer.Serialize(new
        {
            source = Path.GetFileName(sourcePath),
            sourceSha256 = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(original))),
            bootstrap = "host core primitives only; native collection/LINQ/union dependencies are not substituted",
            runtimeExecution = false,
            cases = reports
        }, new JsonSerializerOptions { WriteIndented = true }) + "\n");
    }
}
