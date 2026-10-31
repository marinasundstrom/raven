using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class MethodCategoryImportChecks
{
    internal static void Run(string corePath)
    {
        var bytes = File.ReadAllBytes(corePath);
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(bytes);
        var core = AssemblyDefinition.ReadAssembly(bytes, false).Identity;
        const string source = """
            public open class Bound {}
            public static class Helpers {
                static func Keep<T>(value: T) -> T where T: struct => value
            }
            func Nominal<T>(value: T) -> T where T: Bound => value
            func Value<T>(value: T) -> T where T: struct => value
            func Reference<T>(value: T) -> T where T: class => value
            func Constructible<T>(value: T) -> T where T: new() => value
            """;
        var options = CompilationOptions.NeoCLR.WithTargetCoreAssemblyName(core.Name)
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithRuntimeTypeOfContract(null);
        var provider = Compilation.Create("Categories", [SyntaxTree.ParseText(source)], [bootstrap.Reference], options);
        using var image = new MemoryStream();
        var emitted = NeoClrCompilationEmitter.EmitMetadataAssembly(provider, image, new(new("Categories", new(1, 0, 0, 0)), core, []));
        Check(emitted.Success, "category emission: " + string.Join("; ", emitted.Diagnostics));
        var snapshot = AssemblyDefinition.ReadNativeAssembly(image.ToArray());
        Check(snapshot.MainModule.Functions.Single(f => f.Name == "Value").SpecialConstraints[0].HasFlag(TypeParameterConstraints.ValueType), "value flag erased");
        var reference = NeoClrMetadataReference.ReadAssembly(image.ToArray(), bootstrap);
        Compilation Create(string text) => Compilation.Create("Consumer", [SyntaxTree.ParseText(text)], [bootstrap.Reference, reference], options);
        var consumer = Create("""
            func ForwardBound<T>(value: T) -> T where T: Bound => Nominal<T>(value)
            func Forward<T>(value: T) -> T where T: struct => Constructible<T>(Value<T>(value))
            func ForwardReference<T>(value: T) -> T where T: class => Reference<T>(value)
            func Answer() -> int => Helpers.Keep<int>(Forward<int>(42))
            """);
        using var output = new MemoryStream();
        var result = NeoClrCompilationEmitter.EmitMetadataAssembly(consumer, output, new(new("Consumer", new(1, 0, 0, 0)), core, [new(reference, core)]));
        Check(result.Success, "category import/forward: " + string.Join("; ", result.Diagnostics));
        foreach (var invalid in new[] {
            "func Bad() -> string => Value<string>(\"bad\")",
            "func Bad() -> int => Reference<int>(42)",
            "func Bad() -> int => Nominal<int>(42)",
            "func Bad<T>(value: T) -> T => Value<T>(value)",
            "func Bad<T>(value: T) -> T => Constructible<T>(value)"
        })
            Check(Create(invalid).GetDiagnostics().Any(d => d.Id == "RAV0320"), "missing constraint diagnostic: " + invalid);
        Console.WriteLine("PASS native method category emission, import, forwarding and rejection");
    }
    private static void Check(bool condition, string message)
    {
        if (!condition) throw new Exception(message);
    }
}
