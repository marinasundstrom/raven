using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen.Functions.Invocation;

public class ValueTypeReceiverCodeGenTests
{
    [Fact]
    public void MutableStructInstanceMethod_OnLocal_PreservesMutation()
    {
        const string code = """
import Raven.CodeAnalysis.Tests.CodeGen.Functions.Invocation.*

class Harness {
    public static func Run() -> int {
        var counter = MutableReceiverFixture()
        counter.Increment()
        counter.Value
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        MetadataReference[] references =
        [
            .. TestMetadataReferences.Default,
            MetadataReference.CreateFromFile(typeof(MutableReceiverFixture).Assembly.Location)
        ];
        var compilation = Compilation.Create(
                "value_type_receiver_codegen",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runMethod = loaded.Assembly
            .GetType("Harness", throwOnError: true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;

        Assert.Equal(1, runMethod.Invoke(null, null));
    }
}

public struct MutableReceiverFixture
{
    public MutableReceiverFixture()
    {
        Value = 0;
    }

    public int Value { get; private set; }

    public void Increment() => Value++;
}
