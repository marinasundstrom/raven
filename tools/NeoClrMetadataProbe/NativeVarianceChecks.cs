using System.Reflection;
using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeVarianceChecks
{
    internal static void Run(string corePath, string output)
    {
        Directory.CreateDirectory(output);
        var coreName = AssemblyName.GetAssemblyName(corePath);
        var core = new AssemblyIdentity(coreName.Name!, coreName.Version!);
        var coreReference = MetadataReference.CreateFromFile(corePath);
        const string librarySource = """
            namespace Variance
            public interface Animal { val Value: int { get } }
            public class Dog : Animal { public val Value: int => 42 }
            public interface Producer<out T> { func Get() -> T }
            public interface Consumer<in T> { func Accept(value: T) -> int }
            public interface Fixed<T> { func Get() -> T }
            public class Source : Producer<Dog> { public func Get() -> Dog => Dog() }
            public class Sink : Consumer<Animal> { public func Accept(value: Animal) -> int => value.Value }
            public func MakeProducer() -> Producer<Dog> => Source()
            public func MakeConsumer() -> Consumer<Animal> => Sink()
            """;
        var library = Compilation.Create("VarianceLibrary", [SyntaxTree.ParseText(librarySource)], [coreReference],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(library, image, new(new("VarianceLibrary", new(1, 0, 0, 0)), core, []));
        Check(emitted.Success, "library: " + string.Join("; ", emitted.Diagnostics));
        File.WriteAllBytes(Path.Combine(output, "VarianceLibrary.dll"), image.ToArray());
        File.WriteAllText(Path.Combine(output, "VarianceLibrary.rvn"), librarySource);
        var reference = NeoClrMetadataReference.ReadAssembly(image.ToArray());
        const string source = """
            import Variance.*
            func Main() -> int {
                let producer: Producer<Animal> = MakeProducer()
                let consumer: Consumer<Dog> = MakeConsumer()
                let animal = producer.Get()
                if animal.Value != 42 { return 1 }
                return consumer.Accept(Dog())
            }
            """;
        var compilation = Compilation.Create("VarianceConsumer", [SyntaxTree.ParseText(source)], [coreReference, reference], CompilationOptions.NeoCLR);
        Check(!compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "consumer: " + string.Join("; ", compilation.GetDiagnostics()));
        var symbols = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(reference)!;
        Check(symbols.GetTypeByMetadataName("Variance.Producer`1")!.TypeParameters.Single().Variance == VarianceKind.Out, "producer annotation");
        Check(symbols.GetTypeByMetadataName("Variance.Consumer`1")!.TypeParameters.Single().Variance == VarianceKind.In, "consumer annotation");
        Check(symbols.GetTypeByMetadataName("Variance.Fixed`1")!.TypeParameters.Single().Variance == VarianceKind.None, "default invariant");
        using var consumerImage = new MemoryStream();
        var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, consumerImage,
            new(new("VarianceConsumer", new(1, 0, 0, 0)), core, [new(reference, core)]));
        Check(result.Success, "consumer emission: " + string.Join("; ", result.Diagnostics));
        File.WriteAllBytes(Path.Combine(output, "VarianceConsumer.dll"), consumerImage.ToArray());
        File.WriteAllText(Path.Combine(output, "VarianceConsumer.rvn"), source);
        foreach (var invalid in new[]
        {
            "import Variance.*\nfunc Bad(p: Producer<Animal>) -> Producer<Dog> => p",
            "import Variance.*\nfunc Bad(c: Consumer<Dog>) -> Consumer<Animal> => c",
            "import Variance.*\nfunc Bad(p: Fixed<Dog>) -> Fixed<Animal> => p",
            "import Variance.*\nfunc Bad(p: Producer<int>) -> Producer<Animal> => p"
        })
        {
            var rejected = Compilation.Create("Bad", [SyntaxTree.ParseText(invalid)], [coreReference, reference],
                CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
            Check(rejected.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "unsafe variance accepted: " + invalid);
        }
        var unsafeSource = Compilation.Create("UnsafeVariance", [SyntaxTree.ParseText("public interface Unsafe<out T> { func Put(value: T) -> int }")],
            [coreReference], CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var unsafeImage = new MemoryStream();
        var unsafeResult = NeoClrCompilationEmitter.EmitMetadataAssembly(unsafeSource, unsafeImage,
            new(new("UnsafeVariance", new(1, 0, 0, 0)), core, []));
        Check(!unsafeResult.Success && unsafeImage.Length == 0, "unsafe declaration emitted");
        Console.WriteLine("PASS native variance declarations, imported annotations, reference calls and negative conversions");
    }
    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
}
