using System;
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
""";

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
    public void NormalizeWhitespace_HandlesRepresentativeSyntaxes()
    {
        var snippets = new[]
        {
            "import  System.Console.*\n\nfunc  Main( )->() {WriteLine(\"hi\")}",
            "val res= value match{.Ok(val x)=>x,.Error(val e)=>0}",
            "class Foo<T>{public init(x:T){self.x=x} val x:T}",
            "union Result<T, E> { Ok(value: T)\nError(error: E) }",
            "func f(x:int)->int{if(x>0){return x}else{return -x}}"
        };

        foreach (var snippet in snippets)
        {
            var tree = SyntaxTree.ParseText(snippet);
            if (tree.GetDiagnostics().Any())
            {
                continue;
            }

            var root = tree.GetRoot();
            var once = root.NormalizeWhitespace().ToFullString();

            Assert.False(string.IsNullOrWhiteSpace(once));

            var normalizedTree = SyntaxTree.ParseText(once);
            Assert.NotNull(normalizedTree.GetRoot());
        }
    }

    [Fact]
    public void NormalizeWhitespace_PreservesIndentationAcrossNewLineTerminators()
    {
        const string source = """
import System.Console.*

val foo = Foo()
foo.Bar()

class Foo {
  public Bar() -> () {
    val x = 2
    WriteLine(x)
  }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var normalized = tree.GetRoot().NormalizeWhitespace().ToFullString();

        var expected = """
import System.Console.*
val foo = Foo()
foo.Bar()
class Foo {
    public Bar() -> () {
        val x = 2
        WriteLine(x)
    }
}
""";

        Assert.Equal(expected, normalized);
        Assert.DoesNotContain(" \n", normalized);
    }
}
