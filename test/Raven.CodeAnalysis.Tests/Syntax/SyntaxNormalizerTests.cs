using System.IO;
using System.Linq;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class SyntaxNormalizerTests
{
    [Fact]
    public void NormalizeWhitespace_FormatsControlFlowAndSpacing()
    {
        const string source = "func  main( x:int,y :int)->int{if(x>y){return x}else{return y}}";
        var tree = SyntaxTree.ParseText(source);

        var normalized = tree.GetRoot().NormalizeWhitespace().ToFullString();

        var expected = """
func main(x: int, y: int) -> int {
    if (x > y) {
        return x
    } else {
        return y
    }
}
""".Replace("\n", "\r\n");

        Assert.Equal(expected, normalized);
    }

    [Fact]
    public void NormalizeWhitespace_FormatsTypeSyntaxes()
    {
        const string source = "val x:Result<int,string>?=Foo<int ,string >().Bar( a,b )";
        var tree = SyntaxTree.ParseText(source);

        var normalized = tree.GetRoot().NormalizeWhitespace().ToFullString();

        Assert.Equal("val x: Result<int, string>? = Foo<int, string>().Bar(a, b)", normalized);
    }

    [Fact]
    public void NormalizeWhitespace_IsIdempotent_ForSamples()
    {
        var samplesPath = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "../../../../samples"));

        var files = Directory
            .EnumerateFiles(samplesPath, "*.rav", SearchOption.AllDirectories)
            .Where(path => !path.Contains("/bugs/"))
            .ToArray();

        Assert.NotEmpty(files);

        foreach (var file in files)
        {
            var text = File.ReadAllText(file);
            var root = SyntaxTree.ParseText(text, path: file).GetRoot();

            var once = root.NormalizeWhitespace().ToFullString();
            var twice = SyntaxTree.ParseText(once, path: file).GetRoot().NormalizeWhitespace().ToFullString();

            Assert.Equal(once, twice);
        }
    }
}
