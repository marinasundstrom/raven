using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class GenericBaseChecks
{
    internal static void Run(string corePath, string outputPath)
    {
        Directory.CreateDirectory(outputPath);
        var core = NeoClrReferenceCatalog.ReadNative(corePath, []);
        var options = CompilationOptions.NeoCLR.WithRuntimeTypeOfContract(null)
            .WithTargetCoreAssemblyName(core.CoreIdentity.Name)
            .WithRuntimeUnitContract(new(core.CoreIdentity.Name, "System.Void"))
            .WithMetadataImportOptions(new MetadataImportOptions(core.CoreIdentity.Name)
                .WithObjectAssemblyName(core.CoreIdentity.Name).WithNativeMetadata());
        const string library = """
            public abstract class Converter<T> {
                protected init() { }
                abstract func Read(value: T) -> T
            }
            """;
        var libraryPath = Path.Combine(outputPath, "GenericBase.dll");
        Emit("GenericBase", library, core, libraryPath);
        var imported = NeoClrReferenceCatalog.ReadNative(corePath, [libraryPath]);
        const string consumer = """
            public class PayloadConverter : Converter<Payload> {
                init(): base() { }
                override func Read(value: Payload) -> Payload => value
            }
            public class Payload {
                init() { }
                val Value: int => 42
            }
            func Main() -> int {
                let converter: Converter<Payload> = PayloadConverter()
                let payload = Payload()
                return converter.Read(payload).Value
            }
            """;
        Emit("GenericBaseConsumer", consumer, imported, Path.Combine(outputPath, "GenericBaseConsumer.dll"));
        Console.WriteLine("PASS native imported generic base with an application-owned type argument");

        void Emit(string name, string source, NeoClrReferenceCatalog catalog, string destination)
        {
            var compilation = Compilation.Create(name, [SyntaxTree.ParseText(source)], catalog.References.ToArray(), options.WithOutputKind(name == "GenericBase" ? OutputKind.DynamicallyLinkedLibrary : OutputKind.ConsoleApplication));
            using var output = new MemoryStream();
            var result = compilation.Emit(output, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
                new(new(name, new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies))));
            if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
            _ = AssemblyDefinition.ReadNativeAssembly(output.ToArray());
            File.WriteAllBytes(destination, output.ToArray());
        }
    }
}
