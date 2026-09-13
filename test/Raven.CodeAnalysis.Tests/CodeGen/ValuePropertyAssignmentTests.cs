using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ValuePropertyAssignmentTests
{
    [Theory]
    [InlineData("var cell = default(Cell<int>)\ncell.Value = 42\nreturn cell.Value")]
    [InlineData("let cells: Cell<int>[] = [default(Cell<int>)]\ncells[0].Value = 42\nreturn cells[0].Value")]
    public void SetterMutatesTheOriginalValueStorage(string body)
    {
        var source = "struct Cell<T> { public var Value: T { get; set; } }\nclass Probe { func Run() -> int {\n" + body + "\n} }";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("ValueProperties", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var type = loaded.Assembly.GetType("Probe", true)!;
        var method = type.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance)!;
        Assert.Equal(42, method.Invoke(Activator.CreateInstance(type), null));
    }
}
