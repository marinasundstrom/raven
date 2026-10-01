using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class MultiFileChecks
{
    internal static byte[][] Run(Compilation template, string source, NeoClrEmitOptions options, string directory)
    {
        var split = source.IndexOf("func Main", StringComparison.Ordinal);
        var helperText = source[..split];
        var mainText = source[split..];
        File.WriteAllText(Path.Combine(directory, "Helper.rvn"), helperText);
        File.WriteAllText(Path.Combine(directory, "Main.rvn"), mainText);
        var helper = SyntaxTree.ParseText(helperText, path: "Helper.rvn");
        var main = SyntaxTree.ParseText(mainText, path: "Main.rvn");
        Compilation Create(params SyntaxTree[] trees) => Compilation.Create(template.AssemblyName!, trees, [.. template.References], template.Options);
        var images = new List<byte[]>();
        foreach (var trees in new[] { new[] { helper, main }, new[] { main, helper } })
        {
            var compilation = Create(trees);
            using var output = new MemoryStream();
            var result = NeoClrCompilationEmitter.Emit(compilation, output, options);
            Check(result.Success, "multi-file emission: " + string.Join("; ", result.Diagnostics));
            images.Add(output.ToArray());
        }
        var badHelper = SyntaxTree.ParseText(helperText.Replace("value + 2", "value << 2"), path: "Helper.rvn");
        using var rejectedOutput = new MemoryStream();
        rejectedOutput.WriteByte(99);
        var rejected = NeoClrCompilationEmitter.Emit(Create(main, badHelper), rejectedOutput, options);
        Check(!rejected.Success && rejectedOutput.Position == 1 && rejectedOutput.ToArray().SequenceEqual(new byte[] { 99 }), "later-file rejection writes nothing");
        var diagnostic = rejected.Diagnostics.Single(d => d.Id == "NEOMETA001");
        Check(ReferenceEquals(diagnostic.Location.SourceTree, badHelper) && diagnostic.Location.GetLineSpan().Path == "Helper.rvn", "later-file diagnostic location");
        Check(badHelper.GetRoot().ToFullString().Substring(diagnostic.Location.SourceSpan.Start, diagnostic.Location.SourceSpan.Length) == "value << 2", "later-file expression span");
        Console.WriteLine("PASS multi-file declaration order and later-file diagnostics");
        return images.ToArray();
    }
    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
}
