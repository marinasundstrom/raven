using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

using RuntimeAssemblyContainer = NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer;

namespace NeoClrMetadataProbe;

internal static class PrimitiveSignatureChecks
{
    internal static async Task Run(AssemblyIdentity core, string output, Func<int, string[], Task<string>> command)
    {
        const string librarySource = """
            public static class Predicates {
                public static func Identity(value: string) -> string { value }
                public static func Wide(value: long) -> long {
                    return value
                }
                public static func Identity(value: bool) -> bool {
                    value
                }
                public static func Identity(value: int) -> int {
                    value
                }
                public static func Choose(value: int, selected: bool) -> int {
                    if Identity(selected) {
                        return Identity(value)
                    }
                    return 0
                }
            }
            """;
        var primitive = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
        var library = Compilation.Create("PrimitiveLibrary", [SyntaxTree.ParseText(librarySource)], [primitive],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var result = library.Emit(libraryImage, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
            new(new("PrimitiveLibrary", new Version(1, 0, 0, 0)), core, []))));
        if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
        var libraryPath = Path.Combine(output, "PrimitiveLibrary.dll");
        File.WriteAllBytes(libraryPath, libraryImage.ToArray());
        File.WriteAllText(Path.Combine(output, "PrimitiveLibrary.rvn"), librarySource);
        var reference = MetadataReference.CreateFromFile(libraryPath);
        var snapshot = RuntimeAssemblyContainer.ReadCliProjection(libraryImage.ToArray());
        const string source = """
            func Main() -> int {
                System.Console.WriteLine(Predicates.Identity("Imported 🌍"))
                Predicates.Choose((int)Predicates.Wide(4294967338L), Predicates.Identity(true)) + Predicates.Choose(7, Predicates.Identity(false))
            }
            """;
        var console = MetadataReference.CreateFromFile(typeof(Console).Assembly.Location);
        var consumer = Compilation.Create("PrimitiveConsumer", [SyntaxTree.ParseText(source)], [primitive, reference, console],
            new CompilationOptions(OutputKind.ConsoleApplication));
        using var consumerImage = new MemoryStream();
        result = consumer.Emit(consumerImage, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
            new(new("PrimitiveConsumer", new Version(1, 0, 0, 0)), core, [new(reference, snapshot, core)], console))));
        if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
        var path = Path.Combine(output, "PrimitiveConsumer.dll");
        File.WriteAllBytes(path, consumerImage.ToArray());
        File.WriteAllText(Path.Combine(output, "PrimitiveConsumer.rvn"), source);
        await command(0, ["verify", path, "--module", libraryPath]);
        var executed = await command(42, ["run", path, "--module", libraryPath]);
        if (executed.Trim() != "Imported 🌍") throw new Exception("imported String call result");
        foreach (var rejectedSource in new[] {
            "public static class Text { public static func Missing(value: string?) -> string? { value } }",
            "public static class Text { public static func Same(left: string, right: string) -> bool { left == right } }",
            "public static class Text { public static func Join(left: string, right: string) -> string { left + right } }"
        })
        {
            var rejected = Compilation.Create("RejectedText", [SyntaxTree.ParseText(rejectedSource)], [primitive],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            using var stream = new MemoryStream(); stream.WriteByte(77);
            var failure = NeoClrCompilationEmitter.EmitMetadataAssembly(rejected, stream,
                new(new("RejectedText", new Version(1, 0, 0, 0)), core, []));
            if (failure.Success || !failure.Diagnostics.Any(d => d.Id == "NEOMETA001") ||
                stream.Position != 1 || !stream.ToArray().SequenceEqual(new byte[] { 77 }))
                throw new Exception("unsupported String contract: " + string.Join("; ", failure.Diagnostics));
        }
        Console.WriteLine("PASS primitive/String overloads, Unicode native imports and unsupported String contract rejection");
    }
}
