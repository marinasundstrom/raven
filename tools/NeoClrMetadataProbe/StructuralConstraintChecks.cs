using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class StructuralConstraintChecks
{
    internal static void Run(string inputs, string output)
    {
        Directory.CreateDirectory(output);
        var catalog = NeoClrReferenceCatalog.Read(Path.Combine(inputs, "Core.dll"), [Path.Combine(inputs, "System.Runtime.dll")], Path.Combine(inputs, "System.runtime.neox"), allowExperimentalCompositions: true);
        var options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.ConsoleApplication)
            .WithRuntimeTypeOfContract(null)
            .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name).WithObjectAssemblyName("System.Runtime").WithStructuralCompositions());
        var source = """
            public interface A {}
            public interface B {}
            public class OnlyA : A {}
            public class Both : A, B {}
            public class Neither {}
            public func Either<T>() -> int where T: A | B { return 40 }
            public func All<T>() -> int where T: A & B { return 2 }
            func Main() -> int { return Either<OnlyA>() + All<Both>() }
            """;
        var compilation = Compilation.Create("ConstraintSource", [SyntaxTree.ParseText(source)], catalog.References.ToArray(), options);
        var path = Path.Combine(output, "ConstraintSource.neox");
        Emit(compilation, catalog, "ConstraintSource", path);
        using (var disabled = new MemoryStream())
        {
            var defaultOptions = new NeoClrEmitOptions(new("Disabled", new(1, 0, 0, 0)), catalog.CoreIdentity,
                catalog.Dependencies, bootstrapReference: catalog.Bootstrap.Reference);
            var rejected = compilation.Emit(disabled, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(defaultOptions, emitMetadataAssembly: true)));
            Check(!rejected.Success, "default backend admitted structural constraints");
        }
        var snapshot = AssemblyDefinition.ReadNativeAssembly(File.ReadAllBytes(path), true);
        Check(snapshot.MainModule.Functions.Single(m => m.Name == "Either").TypeConstraints.Single().Bound.Composition?.Kind == ReferenceCompositionKind.Union, "union constraint erased");
        Check(snapshot.MainModule.Functions.Single(m => m.Name == "All").TypeConstraints.Single().Bound.Composition?.Kind == ReferenceCompositionKind.Intersection, "intersection constraint erased");
        var negative = Compilation.Create("Negative", [SyntaxTree.ParseText(source.Replace("All<Both>()", "All<OnlyA>()"))], catalog.References.ToArray(), options);
        Check(negative.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "invalid source constraint accepted");
        Console.WriteLine("PASS native source structural constraint emission and negative semantics");
    }

    private static void Emit(Compilation compilation, NeoClrReferenceCatalog catalog, string name, string path)
    {
        using var stream = new MemoryStream();
        var options = new NeoClrEmitOptions(new(name, new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies,
            bootstrapReference: catalog.Bootstrap.Reference, allowExperimentalCompositionCalls: true);
        var result = compilation.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(options, emitMetadataAssembly: true)));
        Check(result.Success, string.Join("\n", result.Diagnostics));
        File.WriteAllBytes(path, stream.ToArray());
    }
    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
}
