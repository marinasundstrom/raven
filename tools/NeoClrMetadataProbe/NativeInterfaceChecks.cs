using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeInterfaceChecks
{
    internal static void Run(MetadataReference coreReference, AssemblyIdentity core, string output)
    {
        const string librarySource = """
            namespace Contracts
            public interface Value {
                func Get(offset: int) -> int
                val Current: int { get }
            }
            public interface Derived : Value { }
            public class First : Derived {
                public func Get(offset: int) -> int => 19 + offset
                public val Current: int => 19
            }
            public class Second : Derived {
                public func Get(offset: int) -> int => 23 + offset
                public val Current: int => 23
            }
            public func FirstValue() -> Derived => First()
            public func SecondValue() -> Derived => Second()
            """;
        var library = Compilation.Create("NativeInterfaceLibrary", [SyntaxTree.ParseText(librarySource)], [coreReference],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(library, libraryImage,
            new(new("NativeInterfaceLibrary", new Version(1, 0, 0, 0)), core, []));
        Check(emitted.Success, string.Join("; ", emitted.Diagnostics));
        File.WriteAllBytes(Path.Combine(output, "NativeInterfaceLibrary.dll"), libraryImage.ToArray());
        File.WriteAllText(Path.Combine(output, "NativeInterfaceLibrary.rvn"), librarySource);
        var reference = NeoClrMetadataReference.ReadAssembly(libraryImage.ToArray());
        const string source = """
            import Contracts.*
            func Main() -> int {
                let first = FirstValue()
                let second = SecondValue()
                return first.Get(0) + second.Current
            }
            """;
        foreach (var references in new MetadataReference[][] { [coreReference, reference], [reference, coreReference] })
        {
            var compilation = Compilation.Create("NativeInterfaceConsumer", [SyntaxTree.ParseText(source)], references, CompilationOptions.NeoCLR);
            Check(!compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("; ", compilation.GetDiagnostics()));
            var assembly = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(reference)!;
            var contract = assembly.GetTypeByMetadataName("Contracts.Value")!;
            var derived = assembly.GetTypeByMetadataName("Contracts.Derived")!;
            var first = assembly.GetTypeByMetadataName("Contracts.First")!;
            Check(contract.TypeKind == TypeKind.Interface && contract.BaseType is null && contract.IsAbstract, "interface classification");
            Check(ReferenceEquals(derived.Interfaces.Single(), contract) && ReferenceEquals(first.Interfaces.Single(), derived) &&
                first.AllInterfaces.Contains(contract), "canonical interface inheritance and implementation");
            var method = contract.GetMembers("Get").OfType<IMethodSymbol>().Single();
            Check(method.IsAbstract && method.IsVirtual && method.ContainingType == contract, "abstract interface method");
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new("NativeInterfaceConsumer", new Version(1, 0, 0, 0)), core, [new(reference, reference.Definition, core)]));
            Check(result.Success, string.Join("; ", result.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, "NativeInterfaceConsumer.dll"), image.ToArray());
        }
        File.WriteAllText(Path.Combine(output, "NativeInterfaceConsumer.rvn"), source);
        Console.WriteLine("PASS direct native interface symbols and dispatch emission");
    }
    private static void Check(bool value, string message) { if (!value) throw new Exception(message); }
}
