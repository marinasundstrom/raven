using System.Text.Json;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class LibraryChecks
{
    internal static void Run(Compilation template, string source, NeoClrEmitOptions options, byte[] image)
    {
        using var native = JsonDocument.Parse(image);
        if (native.RootElement.GetProperty("entry").GetString() != "") throw new Exception("library has an entry point");
        foreach (var code in new[] {
            source.Replace("static func Multiply", "private static func Multiply"),
            source.Replace("public static class", "internal static class"),
            source.Replace("public static class", "public class"),
            "func Hidden(value: int) -> int { return value }"
        })
        {
            var tree = SyntaxTree.ParseText(code, path: "RejectedLibrary.rvn");
            var compilation = Compilation.Create(template.AssemblyName!, [tree], [.. template.References], template.Options);
            using var output = new MemoryStream(); output.WriteByte(17);
            var result = NeoClrCompilationEmitter.Emit(compilation, output, options);
            if (result.Success || !result.Diagnostics.Any(d => d.Id == "NEOMETA001" && ReferenceEquals(d.Location.SourceTree, tree)))
                throw new Exception("unsupported library shape not diagnosed: " + string.Join("; ", result.Diagnostics));
            if (output.Position != 1 || !output.ToArray().SequenceEqual(new byte[] { 17 })) throw new Exception("rejected library wrote output");
        }
        Console.WriteLine("PASS library output has no entry and unsupported visibility/type shapes are rejected");
    }
}
