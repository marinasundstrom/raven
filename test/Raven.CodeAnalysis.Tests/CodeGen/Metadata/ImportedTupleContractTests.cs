using Raven.CodeAnalysis.Syntax;
namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ImportedTupleContractTests
{
    [Theory]
    [InlineData("Location().Item1", 17)]
    [InlineData("Nested().Item2.Item1", 2)]
    [InlineData("Wide().Rest.Item1", 8)]
    [InlineData("Location().line + Size().width", 22)]
    [InlineData("Location().line", 17)]
    [InlineData("Size().width", 5)]
    [InlineData("Field.left", 1)]
    [InlineData("Property.top", 3)]
    [InlineData("Array()[0].index", 7)]
    [InlineData("Nested().inner.number", 2)]
    [InlineData("Wide().eighth", 8)]
    [InlineData("Fifteen().last", 15)]
    [InlineData("NestedWide().eighth.right", 9)]
    [InlineData("GenericWide<int>(42).eighth", 42)]
    [InlineData("GenericArray<int>(0)[0]", 12)]
    [InlineData("Map()[\"key\"][0].row", 27)]
    [InlineData("Maybe()!.value", 14)]
    [InlineData("FromCallback(func(p) => p.row)", 4)]
    [InlineData("FromRef", 29)]
    [InlineData("FromOut", 23)]
    [InlineData("Box()[(key: 11, fallback: 17)].column", 17)]
    [InlineData("MutableRead", 19)]
    [InlineData("CompoundRead", 12)]
    [InlineData("DeconstructWide", 8)]
    [InlineData("if Wide() is (_, _, _, _, _, _, _, 8) { 1 } else { 0 }", 1)]
    public void ImportedNamedTupleMemberBinds(string expression, int expected)
    {
        var body = expression switch
        {
            "DeconstructWide" => "{ let (_, _, _, _, _, _, _, last) = Wide(); return last }",
            "FromRef" => "{ var pair = Location(); Bump(ref pair); return pair.line }",
            "FromOut" => "{ ReadPair(out var pair); return pair.row }",
            "MutableRead" => "{ var data = Wide(); data.eighth = 19; return data.Rest.Item1 }",
            "CompoundRead" => "{ var data = Wide(); data.eighth += 4; return data.Rest.Item1 }",
            _ => "=> " + expression
        };
        var tree = SyntaxTree.ParseText($$"""
            import Raven.CodeAnalysis.Tests.TupleFixture.*
            class Probe { static func Read() -> int {{body}} }
            """);
        var references = TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(TupleFixture).Assembly.Location)).ToArray();
        var compilation = Compilation.Create("TupleConsumer", [tree], references, CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error);
        Assert.Empty(errors);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Read")!.Invoke(null, null));
    }
}
public static class TupleFixture
{
    public static (int line, int column) Location() => (17, 3);
    public static (int width, int height) Size() => (5, 8);
    public static readonly (int left, int right) Field = (1, 2);
    public static (int top, int bottom) Property => (3, 4);
    public static (int index, int length)[] Array() => [(7, 8)];
    public static (int outer, (int number, int count) inner) Nested() => (1, (2, 3));
    public static (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, int n, int last) Fifteen() => (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    public static ((int x, int y) first, int b, int c, int d, int e, int f, int g, (int left, int right) eighth) NestedWide() => ((1, 2), 2, 3, 4, 5, 6, 7, (8, 9));
    public static (int a, int b, int c, int d, int e, int f, int g, T eighth) GenericWide<T>(T value) => (1, 2, 3, 4, 5, 6, 7, value);
    public static ((int x, int y) first, int last) LeftNested() => ((1, 2), 3);
    public static ((int, int) values, (int number, int size) result) MixedNames() => ((1, 2), (3, 4));
    public static void Bump(ref (int line, int column) value) => value.line += 12;
    public static void ReadPair(out (int row, int column) value) => value = (23, 5);
    public static TupleIndex Box() => new();
    public static int[] GenericArray<T>(T ignored) => [12];
    public static Dictionary<string, (int row, int column)[]> Map() => new() { ["key"] = [(27, 8)] };
    public static (int value, int count)? Maybe() => (14, 2);
    public static int FromCallback(Func<(int row, int column), int> callback) => callback((4, 2));
    public static (int one, int two, int three, int four, int five, int six, int seven, int eighth) Wide() => (1, 2, 3, 4, 5, 6, 7, 8);
}

public class TupleIndex
{
    public (int row, int column) this[(int key, int fallback) key] => (key.key, key.fallback);
}
