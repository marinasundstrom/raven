using System.IO;
using System.Linq;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public interface ImportedPropertyContract<T>
{
    T Current { get; set; }
}

public class ImportedInterfacePropertyTests
{
    [Fact]
    public void ConstructedExternalSetter_UsesSubstitutedParameterType()
    {
        var source = """
            import Raven.CodeAnalysis.Tests.*
            public class Counter : ImportedPropertyContract<int> {
                private var current: int = 0
                var ImportedPropertyContract<int>.Current: int {
                    get => current
                    set => current = value
                }
            }
            """;
        var references = TestMetadataReferences.Default
            .Append(MetadataReference.CreateFromFile(typeof(ImportedPropertyContract<>).Assembly.Location)).ToArray();
        var compilation = Compilation.Create("ImportedSetter", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var counter = loaded.Assembly.GetType("Counter", throwOnError: true)!;
        var contract = counter.GetInterfaces().Single();
        var instance = System.Activator.CreateInstance(counter);
        var property = contract.GetProperty("Current")!;
        property.SetValue(instance, 42);
        Assert.Equal(42, property.GetValue(instance));
        Assert.All(counter.GetInterfaceMap(contract).TargetMethods, method => Assert.True(method.IsPrivate));
    }
}
