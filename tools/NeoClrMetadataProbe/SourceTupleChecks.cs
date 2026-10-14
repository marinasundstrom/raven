using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class SourceTupleChecks
{
    internal static void Run(string inputs, string output)
    {
        Directory.CreateDirectory(output);
        var catalog = NeoClrReferenceCatalog.Read(Path.Combine(inputs, "Core.dll"), [Path.Combine(inputs, "System.Runtime.dll")], Path.Combine(inputs, "System.runtime.neox"));
        var options = CompilationOptions.NeoCLR.WithRuntimeTypeOfContract(null).WithOutputKind(OutputKind.ConsoleApplication)
            .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name).WithObjectAssemblyName("System.Runtime"));
        var source = """
            func Echo(value: (left: int, right: int)) -> (left: int, right: int) {
                return value
            }
            func Nested(value: ((int, int), int)) -> ((int, int), int) {
                return value
            }
            func Rename(value: (left: int, right: int)) -> (x: int, y: int) {
                return value
            }
            func Main() -> int {
                let pair = Echo((40, 2))
                let renamed = Rename(pair)
                let nested = Nested((pair, 0))
                let copy = nested
                return renamed.x + renamed.y + pair.left - pair.left + copy.Item1.Item1 - nested.Item1.Item1 + copy.Item2
            }
            """;
        var compilation = Compilation.Create("SourceTuple", [SyntaxTree.ParseText(source)], catalog.References.ToArray(), options);
        foreach (var enabled in new[] { false, true })
        {
            var emitOptions = new NeoClrEmitOptions(new("SourceTuple", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies,
                bootstrapReference: catalog.Bootstrap.Reference, allowExperimentalTuples: enabled);
            using var stream = new MemoryStream();
            var result = compilation.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(emitOptions, emitMetadataAssembly: true)));
            if (!enabled) { if (result.Success) throw new Exception("default tuple expression gate lost"); continue; }
            if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
            var image = stream.ToArray();
            File.WriteAllBytes(Path.Combine(output, "SourceTuple.neox"), image);
            var snapshot = AssemblyDefinition.ReadExperimentalTupleAssembly(image);
            var echo = snapshot.MainModule.Functions.Single(m => m.Name == "Echo");
            if (!echo.TryGetSignature(out var signature) || signature!.ReturnType.Tuple?.ElementTypes.Count != 2) throw new Exception("tuple signature erased");
            if (!signature.ReturnType.Tuple.ElementNames.SequenceEqual(new[] { "left", "right" })) throw new Exception("native tuple names erased");
            if (!snapshot.MainModule.Functions.Single(m => m.Name == "Rename").TryGetSignature(out var renamed) ||
                !renamed!.ReturnType.Tuple!.ElementNames.SequenceEqual(new[] { "x", "y" }) ||
                !renamed.ParameterTypes[0].Tuple!.ElementNames.SequenceEqual(new[] { "left", "right" }) ||
                renamed.ReturnType != renamed.ParameterTypes[0]) throw new Exception("renamed signature metadata or identity lost");
            if (snapshot.MainModule.Types.Any(t => t.Name.Contains("Tuple"))) throw new Exception("nominal tuple emitted");
        }
        foreach (var unsupported in new[]
        {
            "func Main() -> int { var pair = (40, 2); pair.Item1 = 41; return pair.Item1 }",
            "func Echo<T>(value: (T, int)) -> (T, int) { return value }\nfunc Main() -> int { return 42 }"
        })
        {
            var rejected = Compilation.Create("SourceTuple", [SyntaxTree.ParseText(unsupported)], catalog.References.ToArray(), options);
            using var stream = new MemoryStream();
            var emitOptions = new NeoClrEmitOptions(new("SourceTuple", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies,
                bootstrapReference: catalog.Bootstrap.Reference, allowExperimentalTuples: true);
            var result = rejected.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(emitOptions, emitMetadataAssembly: true)));
            if (result.Success || stream.Length != 0 || !result.Diagnostics.Any(d => d.Id.StartsWith("NEOMETA")))
                throw new Exception("unsupported tuple storage did not fail atomically: " + string.Join("\n", result.Diagnostics));
        }
        Console.WriteLine("PASS source tuple construction, signature, reads and default gate");
    }
}
