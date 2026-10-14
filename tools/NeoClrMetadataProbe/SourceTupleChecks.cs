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
            .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name).WithObjectAssemblyName("System.Runtime").WithStructuralTuples());
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
            "func Change(value: (int, int)) { value.Item1 = 41 }\nfunc Main() -> int { return 42 }",
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
        var mutation = Compilation.Create("TupleMutation", [SyntaxTree.ParseText("""
            func Updated() -> (left: int, right: int) {
                var pair: (left: int, right: int) = (1, 2)
                let copy = pair
                pair.left = 40
                if copy.left != 1 { return (0, 0) }
                var nested = (pair, 9)
                let before = nested
                nested.Item1.right = pair.right + 1
                nested.Item2 = 0
                if before.Item1.right != 2 { return (0, 0) }
                if before.Item2 != 9 { return (0, 0) }
                return (nested.Item1.left, nested.Item1.right - 1)
            }
            func Main() -> int {
                let result = Updated()
                return result.left + result.right
            }
            """)], catalog.References.ToArray(), options);
        using (var stream = new MemoryStream())
        {
            var emitOptions = new NeoClrEmitOptions(new("TupleMutation", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies,
                bootstrapReference: catalog.Bootstrap.Reference, allowExperimentalTuples: true);
            var result = mutation.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(emitOptions, emitMetadataAssembly: true)));
            if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, "TupleMutation.neox"), stream.ToArray());
        }
        CheckCarrierFreeBinding(catalog, options, output);
        CheckImports(catalog, options, output);
        Console.WriteLine("PASS source tuple construction, signature, reads and default gate");
    }
    private static void CheckCarrierFreeBinding(NeoClrReferenceCatalog catalog, CompilationOptions options, string output)
    {
        // Nine elements prove that binding does not look up the nominal family's
        // arity-specific definitions (the supplied library has no Tuple`9).
        var compilation = Compilation.Create("CarrierFreeTuple", [SyntaxTree.ParseText("""
            func Main() -> int {
                var values = (1, 2, 3, 4, 5, 6, 7, 0, 0)
                let original = values
                values.Item9 = 14
                return values.Item1 + values.Item2 + values.Item3 + values.Item4 + values.Item5 + values.Item6 + values.Item7 + values.Item8 + values.Item9 + original.Item9
            }
            """)], catalog.References.ToArray(), options);
        using (var cliStream = new MemoryStream())
        {
            var cli = compilation.Emit(cliStream);
            if (cli.Success || cliStream.Length != 0 || !cli.Diagnostics.Any(d => d.Id == "RAVT003"))
                throw new Exception("structural tuples escaped through CLI emitter");
        }
        using var stream = new MemoryStream();
        var emitOptions = new NeoClrEmitOptions(new("CarrierFreeTuple", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies,
            bootstrapReference: catalog.Bootstrap.Reference, allowExperimentalTuples: true);
        var result = compilation.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(emitOptions, emitMetadataAssembly: true)));
        if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
        if (compilation.GetTypeByMetadataName("System.Tuple`9") is not null) throw new Exception("fixture unexpectedly has a nominal 9-tuple");
        File.WriteAllBytes(Path.Combine(output, "CarrierFreeTuple.neox"), stream.ToArray());
        var legacy = Compilation.Create("CarrierFreeTuple", [SyntaxTree.ParseText("func Main() -> int { return 42 }")], catalog.References.ToArray(),
            options.WithMetadataImportOptions(options.MetadataImportOptions!.WithStructuralTuples(false)));
        using (var mismatch = new MemoryStream())
        {
            var failedContract = legacy.Emit(mismatch, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(emitOptions, emitMetadataAssembly: true)));
            if (failedContract.Success || mismatch.Length != 0 || !failedContract.Diagnostics.Any(d => d.Id == "NEOMETA002")) throw new Exception("native tuples accepted nominal binding contract");
        }
        var immutable = Compilation.Create("ImmutableTuple", [SyntaxTree.ParseText("""
            func Main() -> int {
                let pair = (1, 2)
                pair.Item1 = 40
                return pair.Item1
            }
            """)], catalog.References.ToArray(), options);
        using var rejected = new MemoryStream();
        var rejectedOptions = new NeoClrEmitOptions(new("ImmutableTuple", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies,
            bootstrapReference: catalog.Bootstrap.Reference, allowExperimentalTuples: true);
        var failed = immutable.Emit(rejected, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(rejectedOptions, emitMetadataAssembly: true)));
        if (failed.Success || rejected.Length != 0 || !failed.Diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error))
            throw new Exception("immutable tuple root accepted");
    }

    private static void CheckImports(NeoClrReferenceCatalog catalog, CompilationOptions options, string output)
    {
        var producer = Compilation.Create("TupleLibrary", [SyntaxTree.ParseText("""
            public func RenameImported(value: (left: int, right: int)) -> (x: int, y: int) {
                return value
            }
            public func IdentityImported<T>(value: T) -> T {
                return value
            }
            public func TextImported() -> (message: string, number: int) {
                return ("hello", 42)
            }
            public func NestedImported(value: ((left: int, right: int), int)) -> ((x: int, y: int), int) {
                return value
            }
            """)], catalog.References.ToArray(), options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var producerOptions = new NeoClrEmitOptions(new("TupleLibrary", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies,
            bootstrapReference: catalog.Bootstrap.Reference, allowExperimentalTuples: true);
        using var producerStream = new MemoryStream();
        var produced = producer.Emit(producerStream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(producerOptions, emitMetadataAssembly: true)));
        if (!produced.Success) throw new Exception(string.Join("\n", produced.Diagnostics));
        var bytes = producerStream.ToArray();
        File.WriteAllBytes(Path.Combine(output, "TupleLibrary.neox"), bytes);
        try { NeoClrMetadataReference.ReadAssembly(bytes, catalog.Bootstrap); throw new Exception("ordinary reader accepted tuples"); }
        catch (InvalidDataException) { }
        var reference = NeoClrMetadataReference.ReadExperimentalTupleAssembly(bytes, catalog.Bootstrap);
        var consumer = Compilation.Create("TupleImportConsumer", [SyntaxTree.ParseText("""
            func Main() -> int {
                let pair = RenameImported((40, 2))
                let nested = NestedImported((pair, 0))
                let text = TextImported()
                return nested.Item1.x + pair.y + nested.Item2 + IdentityImported<int>(text.number) - 42
            }
            """)], [.. catalog.References, reference], options);
        foreach (var enabled in new[] { false, true })
        {
            var emitOptions = new NeoClrEmitOptions(new("TupleImportConsumer", new(1, 0, 0, 0)), catalog.CoreIdentity,
                [.. catalog.Dependencies, new NeoClrMetadataDependency(reference, catalog.CoreIdentity)],
                bootstrapReference: catalog.Bootstrap.Reference, allowExperimentalTuples: enabled);
            using var stream = new MemoryStream();
            var result = consumer.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(emitOptions, emitMetadataAssembly: true)));
            if (!enabled)
            {
                if (result.Success || stream.Length != 0 || !result.Diagnostics.Any(d => d.Id == "NEOMETA001"))
                    throw new Exception("tuple import gate lost: " + string.Join("\n", result.Diagnostics));
            }
            else
            {
                if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
                File.WriteAllBytes(Path.Combine(output, "TupleImportConsumer.neox"), stream.ToArray());
            }
        }
        var importedAssembly = (IAssemblySymbol)consumer.GetAssemblyOrModuleSymbol(reference)!;
        var rename = importedAssembly.GlobalNamespace.GetMembers("RenameImported").OfType<IMethodSymbol>().Single();
        if (rename.ReturnType is not ITupleTypeSymbol returned || rename.Parameters[0].Type is not ITupleTypeSymbol parameter ||
            returned.UnderlyingTupleType is not null || parameter.UnderlyingTupleType is not null ||
            !returned.TupleElements.Select(f => f.Name).SequenceEqual(new[] { "x", "y" }) ||
            !parameter.TupleElements.Select(f => f.Name).SequenceEqual(new[] { "left", "right" }) ||
            !SymbolEqualityComparer.Default.Equals(returned, parameter)) throw new Exception("imported tuple symbols lost names or structural equality");

    }

}
