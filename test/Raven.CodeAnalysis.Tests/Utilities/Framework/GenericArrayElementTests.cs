using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class GenericArrayElementTests
{
    [Theory]
    [InlineData(false, "integer")]
    [InlineData(false, "reference")]
    [InlineData(false, "wide-value")]
    [InlineData(true, "integer")]
    [InlineData(true, "reference")]
    [InlineData(true, "wide-value")]
    public void GenericArrayAccessPreservesCompleteElements(bool targetMetadata, string kind)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        if (targetMetadata)
            options = options.WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
                .WithTargetCoreAssemblyName("System.Runtime");
        var source = """
            public class Buffer<T> {
                private field items: T[]
                public init(values: T[]) { items = values }
                public func Replace(value: T) -> T {
                    items[0] = value
                    return items[0]
                }
            }
            public class Example {
                public static func Single<T>(value: T) -> T[] { return [value] }
                public static func Replace<T>(items: T[], value: T) -> T {
                    items[0] = value
                    return items[0]
                }
                public static func First<T>(items: T[]) -> T {
                    for item in items { return item }
                    return items[0]
                }
            }
            """;
        var compilation = Compilation.Create("GenericArrayElements", [SyntaxTree.ParseText(source)],
            references.Select(MetadataReference.CreateFromFile).ToArray(), options);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        object value = kind switch { "integer" => 42, "reference" => "retained", _ => 12345678901234567890.1234m };
        var array = Array.CreateInstance(value.GetType(), 1);
        var type = loaded.Assembly.GetType("Example")!;
        var replace = type.GetMethod("Replace")!.MakeGenericMethod(value.GetType());
        Assert.Equal(value, replace.Invoke(null, [array, value]));
        Assert.Equal(value, array.GetValue(0));
        var first = type.GetMethod("First")!.MakeGenericMethod(value.GetType());
        Assert.Equal(value, first.Invoke(null, [array]));
        var single = type.GetMethod("Single")!.MakeGenericMethod(value.GetType());
        Assert.Equal(value, ((Array)single.Invoke(null, [value])!).GetValue(0));
        var bufferType = loaded.Assembly.GetType("Buffer`1")!.MakeGenericType(value.GetType());
        var buffer = Activator.CreateInstance(bufferType, [array]);
        Assert.Equal(value, bufferType.GetMethod("Replace")!.Invoke(buffer, [value]));
    }
}
