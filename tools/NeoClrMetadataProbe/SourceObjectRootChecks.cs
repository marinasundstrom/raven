using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class SourceObjectRootChecks
{
    internal static void Run(string corePath, string? outputPath = null)
    {
        var core = AssemblyDefinition.ReadAssembly(File.ReadAllBytes(corePath), expectedExtended: false);
        var reference = MetadataReference.CreateFromFile(corePath);
        const string source = """
            namespace System
            public abstract class Object {
                protected init() { }
                virtual func ToString() -> string => "root"
                virtual func Equals(other: Object?) -> bool => false
                virtual func GetHashCode() -> int => 0
            }
            public class Item {
                override func Equals(other: object?) -> bool => true
                override func ToString() -> string => "source root override"
                override func GetHashCode() -> int => 93
                static func Display() -> string => Item().ToString()
                static func Echo(value: object) -> System.Object => value
            }
            """;
        var compilation = Compilation.Create("RootLibrary", [SyntaxTree.ParseText(source)], [reference], CompilationOptions.NeoCLR
                .WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithRuntimeTypeOfContract(null)
                .WithMetadataImportOptions(new MetadataImportOptions(core.Identity.Name, null, null, useSourceObjectRoot: true)));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
        var root = compilation.GetSpecialType(SpecialType.System_Object);
        if (root.ContainingAssembly.Name != "RootLibrary" || root.BaseType is not null)
            throw new Exception("source Object root was not selected");
        using var output = new MemoryStream();
        var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, output, new NeoClrEmitOptions(
            new("RootLibrary", new(1, 0, 0, 0)), core.Identity, [], bootstrapReference: reference));
        if (!result.Success)
            throw new Exception(string.Join("\n", result.Diagnostics.Select(d => d.ToString())));
        var image = output.ToArray();
        var metadata = AssemblyDefinition.ReadNativeAssembly(image);
        if (metadata.MainModule.Types.Single(t => t.Name == "Object").BaseType is not null)
            throw new Exception("emitted root retained a bootstrap base");
        if (outputPath is not null) File.WriteAllBytes(outputPath, image);
        void RejectBeforePublication(Compilation input, MetadataReference bootstrap)
        {
            using var sentinel = new MemoryStream();
            sentinel.WriteByte(42);
            var rejected = NeoClrCompilationEmitter.EmitMetadataAssembly(input, sentinel, new NeoClrEmitOptions(
                new("RootLibrary", new(1, 0, 0, 0)), core.Identity, [], bootstrapReference: bootstrap));
            if (rejected.Success || !sentinel.ToArray().SequenceEqual(new byte[] { 42 }) || sentinel.Position != 1)
                throw new Exception("invalid native root compilation published output");
        }
        RejectBeforePublication(compilation, MetadataReference.CreateFromFile(corePath));
        foreach (var unsupported in new[] {
            source.Replace("protected init()", "virtual func Extra() -> int => 1\n                protected init()"),
            source + "\npublic class Box<T> { }" })
        {
            var invalid = Compilation.Create("RootLibrary", [SyntaxTree.ParseText(unsupported)], [reference], compilation.Options);
            if (invalid.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error))
                throw new Exception("negative emission control did not bind");
            RejectBeforePublication(invalid, reference);
        }
        Console.WriteLine("PASS source Object root native emission and no-publication controls");
    }
}
