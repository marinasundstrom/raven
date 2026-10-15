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
            .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name).WithObjectAssemblyName("System.Runtime"));
        var traits = Compilation.Create("TraitSource", [SyntaxTree.ParseText("""
            interface ReadsA {
                func ReadA() -> int
            }
            interface ReadsB {
                func ReadB() -> int
            }
            class BothTraits : ReadsA, ReadsB {
                func ReadA() -> int { return 40 }
                func ReadB() -> int { return 2 }
            }
            func Read<T>(value: T) -> int where T: ReadsA & ReadsB {
                return value.ReadA() + value.ReadB()
            }
            func Main() -> int { return Read<BothTraits>(BothTraits()) }
            """)], catalog.References.ToArray(), options);
        Emit(traits, catalog, "TraitSource", Path.Combine(output, "TraitSource.neox"));
        var shapeOnly = Compilation.Create("ShapeOnly", traits.SyntaxTrees.Append(SyntaxTree.ParseText("""
            class ShapeOnly : ReadsB {
                func ReadA() -> int { return 40 }
                func ReadB() -> int { return 2 }
            }
            func RejectShape() -> int { return Read<ShapeOnly>(ShapeOnly()) }
            """)).ToArray(), catalog.References.ToArray(), options);
        Check(shapeOnly.GetDiagnostics().Any(d => d.Id == "RAV0320"), "member shape was accepted as nominal interface conformance");
        var traitLibraryPath = Path.Combine(output, "TraitLibrary.neox");
        var traitLibrary = Compilation.Create("TraitLibrary", [SyntaxTree.ParseText("""
            public interface ImportedReadsA {
                func ReadA() -> int
            }
            public interface ImportedReads<T> {
                func Read() -> T
            }
            public interface ImportedReadsB {
                func ReadB() -> int
            }
            public open class ExternalAnimal {}
            public class ExternalDuck : ExternalAnimal, ImportedReadsB {
                func ReadB() -> int { return 42 }
            }
            """)], catalog.References.ToArray(), options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        Emit(traitLibrary, catalog, "TraitLibrary", traitLibraryPath);
        var traitCatalog = NeoClrReferenceCatalog.Read(Path.Combine(inputs, "Core.dll"),
            [Path.Combine(inputs, "System.Runtime.dll"), traitLibraryPath], Path.Combine(inputs, "System.runtime.neox"), allowExperimentalCompositions: true);
        var externalBoundSource = """
            public class ExternalOnlyA : ImportedReadsA {
                func ReadA() -> int { return 40 }
            }
            public class ExternalShapeOnly {
                func ReadA() -> int { return 40 }
            }
            public func ExternalEither<T>() -> int where T: ImportedReadsA | ImportedReadsB { return 40 }
            public func ExternalClosed<T>() -> int where T: ImportedReads<int> | ImportedReadsB { return 2 }
            public class ExternalIntReader : ImportedReads<int> {
                func Read() -> int { return 42 }
            }
            """;
        var externalBoundLibrary = Compilation.Create("ExternalBoundLibrary", [SyntaxTree.ParseText(externalBoundSource)],
            traitCatalog.References.ToArray(), options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var externalBoundPath = Path.Combine(output, "ExternalBoundLibrary.neox");
        Emit(externalBoundLibrary, traitCatalog, "ExternalBoundLibrary", externalBoundPath);
        var externalBoundCatalog = NeoClrReferenceCatalog.Read(Path.Combine(inputs, "Core.dll"),
            [Path.Combine(inputs, "System.Runtime.dll"), traitLibraryPath, externalBoundPath],
            Path.Combine(inputs, "System.runtime.neox"), allowExperimentalCompositions: true);
        var externalBoundConsumer = Compilation.Create("ExternalBoundConsumer", [SyntaxTree.ParseText("""
            func Main() -> int { return ExternalEither<ExternalOnlyA>() + ExternalClosed<ExternalIntReader>() }
            """)], externalBoundCatalog.References.ToArray(), options);
        Emit(externalBoundConsumer, externalBoundCatalog, "ExternalBoundConsumer", Path.Combine(output, "ExternalBoundConsumer.neox"));
        foreach (var call in new[] { "ExternalEither<ExternalShapeOnly>()", "ExternalClosed<ExternalOnlyA>()" })
        {
            var invalid = Compilation.Create("InvalidExternalBound", [SyntaxTree.ParseText($"func Main() -> int {{ return {call} }}")],
                externalBoundCatalog.References.ToArray(), options);
            Check(invalid.GetDiagnostics().Any(d => d.Id == "RAV0320"), "external union bound admitted an invalid argument: " + call);
        }
        var intersectionLibrary = Compilation.Create("ExternalIntersectionLibrary", [SyntaxTree.ParseText("""
            public class BothExternal : ImportedReads<int>, ImportedReadsB {
                func Read() -> int { return 40 }
                func ReadB() -> int { return 2 }
            }
            public class ShapeOnlyExternal : ImportedReadsB {
                func Read() -> int { return 40 }
                func ReadB() -> int { return 2 }
            }
            public func ReadExternal<T>(value: T) -> int where T: ImportedReads<int> & ImportedReadsB {
                return value.Read() + value.ReadB()
            }
            public func CheckAnimal<T>() -> int where T: ExternalAnimal & ImportedReadsB { return 0 }
            """)], traitCatalog.References.ToArray(), options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var intersectionPath = Path.Combine(output, "ExternalIntersectionLibrary.neox");
        Emit(intersectionLibrary, traitCatalog, "ExternalIntersectionLibrary", intersectionPath);
        var intersectionImage = File.ReadAllBytes(intersectionPath);
        var traitSnapshot = AssemblyDefinition.ReadNativeAssembly(File.ReadAllBytes(traitLibraryPath));
        var resolver = new SnapshotResolver(traitSnapshot);
        var directIntersection = NeoClrMetadataReference.ReadExperimentalIntersectionAssembly(
            intersectionImage, catalog.Bootstrap, resolver);
        Check(directIntersection.Definition.MainModule.Functions.Single(m => m.Name == "ReadExternal")
            .TypeConstraints.Single().Bound.Composition?.Kind == ReferenceCompositionKind.Intersection,
            "intersection-only reader erased the external bound");
        foreach (var read in new Action[] {
            () => NeoClrMetadataReference.ReadExperimentalIntersectionAssembly(intersectionImage, catalog.Bootstrap),
            () => NeoClrMetadataReference.ReadExperimentalIntersectionAssembly(
                File.ReadAllBytes(externalBoundPath), catalog.Bootstrap, resolver) })
        {
            try
            {
                read();
                throw new Exception("intersection-only reader admitted an unresolved intersection or a union");
            }
            catch (InvalidDataException) { }
        }
        Console.WriteLine("PASS resolved intersection-only reader and profile rejection");
        // Deliberately put the dependent first: selected snapshots resolve independent of input order.
        var intersectionCatalog = NeoClrReferenceCatalog.Read(Path.Combine(inputs, "Core.dll"),
            [intersectionPath, Path.Combine(inputs, "System.Runtime.dll"), traitLibraryPath],
            Path.Combine(inputs, "System.runtime.neox"), allowExperimentalCompositions: true);
        var intersectionConsumer = Compilation.Create("ExternalIntersectionConsumer", [SyntaxTree.ParseText("""
            func Main() -> int { return ReadExternal<BothExternal>(BothExternal()) + CheckAnimal<ExternalDuck>() }
            """)], intersectionCatalog.References.ToArray(), options);
        Emit(intersectionConsumer, intersectionCatalog, "ExternalIntersectionConsumer", Path.Combine(output, "ExternalIntersectionConsumer.neox"));
        var invalidIntersection = Compilation.Create("InvalidExternalIntersection", [SyntaxTree.ParseText("""
            func Main() -> int { return ReadExternal<ShapeOnlyExternal>(ShapeOnlyExternal()) }
            """)], intersectionCatalog.References.ToArray(), options);
        Check(invalidIntersection.GetDiagnostics().Any(d => d.Id == "RAV0320"), "external intersection accepted member-only conformance");
        try
        {
            _ = NeoClrReferenceCatalog.Read(Path.Combine(inputs, "Core.dll"), [intersectionPath, Path.Combine(inputs, "System.Runtime.dll")],
                Path.Combine(inputs, "System.runtime.neox"), allowExperimentalCompositions: true);
            throw new Exception("missing intersection dependency accepted");
        }
        catch (InvalidDataException) { }
        var externalTraits = Compilation.Create("ExternalTraits", [SyntaxTree.ParseText("""
            interface LocalA : ImportedReadsA {}
            interface LocalB : ImportedReadsB {}
            class BothTraits : LocalA, LocalB {
                func ReadA() -> int { return 40 }
                func ReadB() -> int { return 2 }
            }
            func Read<T>(value: T) -> int where T: LocalA & LocalB {
                return value.ReadA() + value.ReadB()
            }
            func Main() -> int { return Read<BothTraits>(BothTraits()) }
            """)], traitCatalog.References.ToArray(), options);
        Emit(externalTraits, traitCatalog, "ExternalTraits", Path.Combine(output, "ExternalTraits.neox"));
        var constructedTraits = Compilation.Create("ConstructedTraits", [SyntaxTree.ParseText("""
            interface LocalA : ImportedReads<int> {}
            interface LocalB : ImportedReadsB {}
            class BothTraits : LocalA, LocalB {
                func Read() -> int { return 40 }
                func ReadB() -> int { return 2 }
            }
            func Read<T>(value: T) -> int where T: LocalA & LocalB {
                return value.Read() + value.ReadB()
            }
            func Main() -> int { return Read<BothTraits>(BothTraits()) }
            """)], traitCatalog.References.ToArray(), options);
        Emit(constructedTraits, traitCatalog, "ConstructedTraits", Path.Combine(output, "ConstructedTraits.neox"));
        var source = """
            public interface A {}
            public interface B {}
            public class OnlyA : A {}
            public class Both : A, B {}
            public class Neither {}
            public class Response<T> {}
            public func Reply<T>() -> int where T: Response<A> | B { return 0 }
            public func Either<T>() -> int where T: A | B { return 40 }
            public func All<T>() -> int where T: A & B { return 2 }
            public func Forward<T>() -> int where T: A | B { return Either<T>() }
            public func ForwardAll<T>() -> int where T: A & B { return All<T>() }
            func Main() -> int { return Forward<OnlyA>() + ForwardAll<Both>() + Reply<Response<A>>() }
            """;
        var compilation = Compilation.Create("ConstraintSource", [SyntaxTree.ParseText(source)], catalog.References.ToArray(), options);
        var path = Path.Combine(output, "ConstraintSource.neox");
        Emit(compilation, catalog, "ConstraintSource", path);
        var snapshot = AssemblyDefinition.ReadNativeAssembly(File.ReadAllBytes(path), true);
        Check(snapshot.MainModule.Functions.Single(m => m.Name == "Either").TypeConstraints.Single().Bound.Composition?.Kind == ReferenceCompositionKind.Union, "union constraint erased");
        Check(snapshot.MainModule.Functions.Single(m => m.Name == "All").TypeConstraints.Single().Bound.Composition?.Kind == ReferenceCompositionKind.Intersection, "intersection constraint erased");
        var negative = Compilation.Create("Negative", [SyntaxTree.ParseText(source.Replace("ForwardAll<Both>()", "ForwardAll<OnlyA>()"))], catalog.References.ToArray(), options);
        Check(negative.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "invalid source constraint accepted");
        var weakForwarding = Compilation.Create("WeakForwarding", [SyntaxTree.ParseText(source.Replace("return Either<T>()", "return All<T>()"))], catalog.References.ToArray(), options);
        Check(weakForwarding.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "weak forwarded constraint accepted");
        var producerSource = source.Replace("func Main() -> int { return Forward<OnlyA>() + ForwardAll<Both>() + Reply<Response<A>>() }", "");
        var producer = Compilation.Create("ConstraintLibrary", [SyntaxTree.ParseText(producerSource)], catalog.References.ToArray(), options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var producerPath = Path.Combine(output, "ConstraintLibrary.neox");
        Emit(producer, catalog, "ConstraintLibrary", producerPath);
        var importedCatalog = NeoClrReferenceCatalog.Read(Path.Combine(inputs, "Core.dll"), [Path.Combine(inputs, "System.Runtime.dll"), producerPath], Path.Combine(inputs, "System.runtime.neox"), allowExperimentalCompositions: true);
        var consumer = Compilation.Create("ConstraintConsumer", [SyntaxTree.ParseText("func Main() -> int { return Forward<OnlyA>() + ForwardAll<Both>() + Reply<Response<A>>() }")], importedCatalog.References.ToArray(), options);
        Emit(consumer, importedCatalog, "ConstraintConsumer", Path.Combine(output, "ConstraintConsumer.neox"));
        foreach (var call in new[] { "Either<Neither>()", "All<OnlyA>()", "Reply<Response<B>>()" })
        {
            var invalid = Compilation.Create("InvalidConsumer", [SyntaxTree.ParseText($"func Main() -> int {{ return {call} }}")], importedCatalog.References.ToArray(), options);
            Check(invalid.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "imported constraint was ignored: " + call);
        }
        try
        {
            _ = NeoClrReferenceCatalog.Read(Path.Combine(inputs, "Core.dll"), [Path.Combine(inputs, "System.Runtime.dll"), producerPath], Path.Combine(inputs, "System.runtime.neox"));
            throw new Exception("default catalog admitted constrained library");
        }
        catch (InvalidDataException) { }
        Console.WriteLine("PASS native constrained library import and consumer emission");
        Console.WriteLine("PASS native source structural constraint emission and negative semantics");
    }

    private static void Emit(Compilation compilation, NeoClrReferenceCatalog catalog, string name, string path)
    {
        using var stream = new MemoryStream();
        var options = new NeoClrEmitOptions(new(name, new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies,
            bootstrapReference: catalog.Bootstrap.Reference);
        var result = compilation.Emit(stream, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(options, emitMetadataAssembly: true)));
        Check(result.Success, string.Join("\n", result.Diagnostics));
        File.WriteAllBytes(path, stream.ToArray());
    }
    private sealed class SnapshotResolver(AssemblyDefinition snapshot) : IAssemblyResolver
    {
        public AssemblyDefinition? Resolve(AssemblyIdentity identity) => snapshot.Identity.Equals(identity) ? snapshot : null;
    }

    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
}
