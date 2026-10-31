using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class StaticInterfaceHelperChecks
{
    internal static void Run(string corePath, string outputPath)
    {
        var catalog = NeoClrReferenceCatalog.ReadNative(corePath, []);
        var options = CompilationOptions.NeoCLR.WithRuntimeTypeOfContract(null)
            .WithTargetCoreAssemblyName(catalog.CoreIdentity.Name)
            .WithRuntimeUnitContract(new(catalog.CoreIdentity.Name, "System.Void"))
            .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name)
                .WithObjectAssemblyName(catalog.CoreIdentity.Name).WithNativeMetadata());
        const string source = """
            public interface Value {
                static val Empty: Value => Implementation()
                static func Create() -> Value => Implementation()
                func Read() -> int
            }
            internal class Implementation : Value {
                func Read() -> int => 21
            }
            func Main() -> int => Value.Empty.Read() + Value.Create().Read()
            """;
        var compilation = Compilation.Create("StaticInterfaceHelpers", [SyntaxTree.ParseText(source)], catalog.References.ToArray(), options);
        using var output = new MemoryStream();
        var result = compilation.Emit(output, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
            new(new("StaticInterfaceHelpers", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies))));
        if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
        var contract = AssemblyDefinition.ReadNativeAssembly(output.ToArray()).MainModule.Types.Single(t => t.Name == "Value");
        if (contract.Properties.Single().Name != "Empty" || contract.Methods.Count != 3)
            throw new Exception("Static interface factory metadata lost");
        Directory.CreateDirectory(outputPath);
        File.WriteAllBytes(Path.Combine(outputPath, "StaticInterfaceHelpers.dll"), output.ToArray());
        var dependency = Path.Combine(outputPath, "StaticInterfaceHelpers.dll");
        var imported = NeoClrReferenceCatalog.ReadNative(corePath, [dependency]);
        var consumer = Compilation.Create("InterfaceHelperConsumer",
            [SyntaxTree.ParseText("func Main() -> int => Value.Empty.Read() + Value.Create().Read()")],
            imported.References.ToArray(), options);
        using var consumerOutput = new MemoryStream();
        var consumerResult = consumer.Emit(consumerOutput, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
            new(new("InterfaceHelperConsumer", new(1, 0, 0, 0)), imported.CoreIdentity, imported.Dependencies))));
        if (!consumerResult.Success) throw new Exception(string.Join("\n", consumerResult.Diagnostics));
        File.WriteAllBytes(Path.Combine(outputPath, "InterfaceHelperConsumer.dll"), consumerOutput.ToArray());
        Console.WriteLine("PASS native static interface helpers, implementation contract and cross-assembly consumer");
    }
}
