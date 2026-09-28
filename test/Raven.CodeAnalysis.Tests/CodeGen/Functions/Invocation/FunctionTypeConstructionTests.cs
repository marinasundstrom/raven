using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Operations;

namespace Raven.CodeAnalysis.Tests;

public class FunctionTypeConstructionTests
{
    [Theory]
    [InlineData("int")]
    [InlineData("() -> ()")]
    [InlineData("(int) -> int")]
    [InlineData("() -> int")]
    public void GenericConstructionWithFunctionArgumentInitializesField(string argument)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.Collections.Generic.*
            public class Holder {
                private field callbacks: List<{{argument}}>
                init() {
                    callbacks = List<{{argument}}>()
                }
                func Count() -> int => callbacks.Count
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("FunctionTypeConstruction", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        Assert.Equal(OperationKind.ObjectCreation, model.GetOperation(invocation)!.Kind);
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        output.Position = 0;
        using var loaded = TestAssemblyLoader.LoadFromStream(output, references);
        var type = loaded.Assembly.GetType("Holder", true)!;
        var instance = Activator.CreateInstance(type);
        Assert.Equal(0, type.GetMethod("Count", BindingFlags.Public | BindingFlags.Instance)!.Invoke(instance, null));
    }
    [Fact]
    public void GenericConstructionWithUnknownFunctionResultReportsError()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            let callbacks = List<() -> Missing>()
            """);
        var compilation = Compilation.Create("InvalidFunctionTypeConstruction", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var output = new MemoryStream();
        Assert.False(compilation.Emit(output).Success);
    }

}
