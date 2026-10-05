using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class SourceObjectRootChecks
{
    internal static void Run(string corePath)
    {
        var core = AssemblyDefinition.ReadAssembly(File.ReadAllBytes(corePath), expectedExtended: false);
        var reference = MetadataReference.CreateFromFile(corePath);
        var compilation = Compilation.Create("RootLibrary", [SyntaxTree.ParseText("""
            namespace System
            public abstract class Object {
                protected init() { }
                virtual func ToString() -> string => "root"
                virtual func Equals(other: Object?) -> bool => false
                virtual func GetHashCode() -> int => 0
            }
            public class Item {
                override func Equals(other: object?) -> bool => false
                static func Echo(value: object) -> System.Object => value
            }
            """)], [reference], CompilationOptions.NeoCLR
                .WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithRuntimeTypeOfContract(null)
                .WithMetadataImportOptions(new MetadataImportOptions(core.Identity.Name, null, null, useSourceObjectRoot: true)));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
        var root = compilation.GetSpecialType(SpecialType.System_Object);
        if (root.ContainingAssembly.Name != "RootLibrary" || root.BaseType is not null)
            throw new Exception("source Object root was not selected");
        using var output = new MemoryStream();
        output.WriteByte(42);
        var result = NeoClrCompilationEmitter.Emit(compilation, output, new NeoClrEmitOptions(
            new("RootLibrary", new(1, 0, 0, 0)), core.Identity, [], bootstrapReference: reference));
        if (result.Success || !result.Diagnostics.Any(d => d.Id == "NEOMETA002" && d.GetMessage().Contains("native root authoring")) ||
            !output.ToArray().SequenceEqual(new byte[] { 42 }) || output.Position != 1)
            throw new Exception("unsupported native root authoring did not fail before publication");
        Console.WriteLine("PASS source Object root semantic identity and native emission boundary");
    }
}
