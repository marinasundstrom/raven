using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class LocalFunctionReceiverTests
{
    [Theory]
    [InlineData("value")]
    [InlineData("self.value")]
    [InlineData("Value")]
    [InlineData("Read()")]
    public void LocalFunctionAfterReturnCapturesInstance(string expression)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Probe {
                private field value: int = 42
                private val Value: int => value
                private func Read() -> int => value
                func Run() -> int {
                    return Local()
                    func Local() -> int => {{expression}}
                }
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("LocalFunctionReceiver", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var function = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var capture = Assert.Single(compilation.GetSemanticModel(tree).GetCapturedVariables(function));
        Assert.Equal("Probe", Assert.IsAssignableFrom<ITypeSymbol>(capture).Name);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var type = loaded.Assembly.GetType("Probe", true)!;
        Assert.Equal(42, type.GetMethod("Run")!.Invoke(Activator.CreateInstance(type), null));
    }
}
