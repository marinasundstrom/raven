using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class SourceCompositionChecks
{
    internal static void Run(string inputs, string output)
    {
        Directory.CreateDirectory(output);
        var corePath = Path.Combine(inputs, "Core.dll");
        var libraryPath = Path.Combine(inputs, "System.Runtime.dll");
        var seedPath = Path.Combine(inputs, "System.runtime.neox");
        foreach (var intersection in new[] { false, true })
        {
            var name = intersection ? "AuthoredIntersection" : "AuthoredUnion";
            var operation = intersection ? "&" : "|";
            var catalog = NeoClrReferenceCatalog.Read(corePath, [libraryPath], seedPath, allowExperimentalCompositions: true);
            var options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
                .WithRuntimeTypeOfContract(null)
                .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name)
                    .WithObjectAssemblyName("System.Runtime").WithStructuralCompositions());
            var source = $$"""
                public interface A {}
                public interface B {}
                public class Both : A, B {}
                public func Echo(value: A {{operation}} B) -> A {{operation}} B { return value }
                """;
            var producer = Compilation.Create(name, [SyntaxTree.ParseText(source)], catalog.References.ToArray(), options);
            var path = Path.Combine(output, name + ".neox");
            Emit(producer, catalog, name, path);
            var image = File.ReadAllBytes(path);
            var definition = AssemblyDefinition.ReadNativeAssembly(image, allowExperimentalCompositions: true);
            var echo = definition.MainModule.Functions.Single(m => m.Name == "Echo");
            Check(echo.TryGetSignature(out var signature) && signature!.ReturnType.Composition?.Kind ==
                (intersection ? ReferenceCompositionKind.Intersection : ReferenceCompositionKind.Union), "source composition was erased");
            Check(!definition.MainModule.Types.Any(t => t.Name.StartsWith("Union`")), "ad hoc nominal carrier emitted");
            try
            {
                _ = NeoClrReferenceCatalog.Read(corePath, [libraryPath, path], seedPath);
                throw new Exception("default catalog admitted structural NEOX");
            }
            catch (InvalidDataException) { }
            var consumerCatalog = NeoClrReferenceCatalog.Read(corePath, [libraryPath, path], seedPath, allowExperimentalCompositions: true);
            var consumer = Compilation.Create(name + "Consumer",
                [SyntaxTree.ParseText("func Main() -> int { let value = Echo(Both()); let copy = value; return 42 }")],
                consumerCatalog.References.ToArray(), options.WithOutputKind(OutputKind.ConsoleApplication));
            Emit(consumer, consumerCatalog, name + "Consumer", Path.Combine(output, name + "Consumer.neox"));
        }
        Console.WriteLine("PASS source union/intersection native signatures, catalog roundtrip, consumer emission and default gate");
    }

    private static void Emit(Compilation compilation, NeoClrReferenceCatalog catalog, string name, string path)
    {
        using var stream = new MemoryStream();
        var options = new NeoClrEmitOptions(new(name, new(1, 0, 0, 0)), catalog.CoreIdentity,
            catalog.Dependencies, bootstrapReference: catalog.Bootstrap.Reference, allowExperimentalCompositionCalls: true);
        var result = compilation.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(options, emitMetadataAssembly: true)));
        Check(result.Success, string.Join("\n", result.Diagnostics));
        File.WriteAllBytes(path, stream.ToArray());
    }
    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
}
