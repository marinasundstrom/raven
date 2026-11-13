using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class RefIndexerMutabilityTests
{
    [Theory]
    [InlineData("let values: System.Span<int> = [1, 2]", "values[0] = 42", "values[0]")]
    [InlineData("let values: System.Span<int> = stackalloc int[2]", "values[0] = 42", "values[0]")]
    [InlineData("let values = Raven.CodeAnalysis.Tests.WritableRefIndexer()", "values[0] = 42", "values[0]")]
    public void ReadonlyReceiverMayReturnWritableReference(string declaration, string assignment, string resultExpression)
    {
        var compilation = Create($"class Probe {{ static func Run() -> int {{ {declaration}\n{assignment}\nreturn {resultExpression} }} }}");
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("let values: System.ReadOnlySpan<int> = [1, 2]")]
    [InlineData("let values = Raven.CodeAnalysis.Tests.ReadonlyRefIndexer()")]
    public void ReadonlyReturnedReferenceRejectsAssignment(string declaration)
    {
        var compilation = Create($"class Probe {{ static func Run() {{ {declaration}\nvalues[0] = 42 }} }}");
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0131");
    }

    private static Compilation Create(string source)
        => Compilation.Create("RefIndexerMutability", [SyntaxTree.ParseText(source)],
            TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(WritableRefIndexer).Assembly.Location)).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
}

public readonly struct WritableRefIndexer
{
    private readonly int[] _values;

    public WritableRefIndexer() => _values = new int[1];

    public ref int this[int index] => ref _values[index];
}

public sealed class ReadonlyRefIndexer
{
    private readonly int[] _values = new int[1];

    public ref readonly int this[int index] => ref _values[index];
}
