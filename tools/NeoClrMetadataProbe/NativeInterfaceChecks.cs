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
        const string storageSource = """
            namespace Contracts
            public class Storage {
                public field Current: Value
                public field Items: Value[]
                public init(current: Value, items: Value[]) {
                    self.Current = current
                    self.Items = items
                }
                public func Read() -> int => Current.Current
            }
            """;
        var storageCompilation = Compilation.Create("NativeInterfaceStorageLibrary", [SyntaxTree.ParseText(storageSource)], [coreReference, reference],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var storageImage = new MemoryStream();
        var storageResult = NeoClrCompilationEmitter.EmitMetadataAssembly(storageCompilation, storageImage,
            new(new("NativeInterfaceStorageLibrary", new Version(1, 0, 0, 0)), core, [new(reference, reference.Definition, core)]));
        Check(storageResult.Success, string.Join("; ", storageResult.Diagnostics));
        File.WriteAllBytes(Path.Combine(output, "NativeInterfaceStorageLibrary.dll"), storageImage.ToArray());
        File.WriteAllText(Path.Combine(output, "NativeInterfaceStorageLibrary.rvn"), storageSource);
        var storageReference = NeoClrMetadataReference.ReadAssembly(storageImage.ToArray());
        const string source = """
            import Contracts.*
            func Main() -> int {
                let first = FirstValue()
                let second = SecondValue()
                let values: Value[] = [first, second]
                let storage = Storage(first, values)
                let original = storage.Current
                storage.Current = second
                storage.Items[0] = storage.Current
                if values[0].Current != 23 { return 1 }
                if original.Current != 19 { return 2 }
                return original.Get(0) + storage.Read()
            }
            """;
        foreach (var references in new MetadataReference[][] { [coreReference, reference, storageReference], [storageReference, reference, coreReference] })
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
            var storageAssembly = (IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(storageReference)!;
            var storage = storageAssembly.GetTypeByMetadataName("Contracts.Storage")!;
            Check(ReferenceEquals(storage.GetMembers("Current").OfType<IFieldSymbol>().Single().Type, contract) &&
                storage.GetMembers("Items").OfType<IFieldSymbol>().Single().Type is IArrayTypeSymbol array && ReferenceEquals(array.ElementType, contract) &&
                ReferenceEquals(storage.InstanceConstructors.Single().Parameters[0].Type, contract), "canonical external interface storage types");
            var method = contract.GetMembers("Get").OfType<IMethodSymbol>().Single();
            Check(method.IsAbstract && method.IsVirtual && method.ContainingType == contract, "abstract interface method");
            var invalid = Compilation.Create("InvalidInterfaceConversion", [SyntaxTree.ParseText("import Contracts.*\nfunc Wrong(value: Storage) { let invalid: Value = value }")],
                references, CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
            Check(invalid.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "unrelated native class must not convert to interface");
            var invalidReturn = Compilation.Create("InvalidInterfaceReturn", [SyntaxTree.ParseText("import Contracts.*\nfunc Wrong(value: Storage) -> Value => value")],
                references, CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
            Check(invalidReturn.GetDiagnostics().Count(d => d.Id == "RAV1503") == 1,
                "unrelated native interface return must diagnose before emission");
            using var rejectedImage = new MemoryStream();
            var rejected = NeoClrCompilationEmitter.EmitMetadataAssembly(invalidReturn, rejectedImage,
                new(new("InvalidInterfaceReturn", new Version(1, 0, 0, 0)), core, [new(reference, reference.Definition, core), new(storageReference, storageReference.Definition, core)]));
            Check(!rejected.Success && rejectedImage.Length == 0, "invalid interface return must not emit an assembly");
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new("NativeInterfaceConsumer", new Version(1, 0, 0, 0)), core, [new(reference, reference.Definition, core), new(storageReference, storageReference.Definition, core)]));
            Check(result.Success, string.Join("; ", result.Diagnostics));
            File.WriteAllBytes(Path.Combine(output, "NativeInterfaceConsumer.dll"), image.ToArray());
        }
        File.WriteAllText(Path.Combine(output, "NativeInterfaceConsumer.rvn"), source);
        Console.WriteLine("PASS direct native interface symbols and dispatch emission");
    }
    private static void Check(bool value, string message) { if (!value) throw new Exception(message); }
}
