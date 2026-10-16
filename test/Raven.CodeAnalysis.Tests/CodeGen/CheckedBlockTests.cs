using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class CheckedBlockTests
{
    [Theory]
    [InlineData("checked { return a + b }", null)]
    [InlineData("unchecked { return a + b }", int.MinValue)]
    [InlineData("checked { unchecked { return a + b } }", int.MinValue)]
    [InlineData("unchecked { checked { return a + b } }", null)]
    [InlineData("checked { return unchecked(a + b) }", int.MinValue)]
    [InlineData("checked { var x = a; x += b; return x }", null)]
    public void BlocksUseNearestContext(string body, object? expected)
    {
        using var loaded = Load($"class Runner {{ static func Run(a: int, b: int) -> int {{ {body} }} }}");
        var method = loaded.Assembly.GetType("Runner")!.GetMethod("Run")!;
        if (expected is null)
            Assert.IsType<OverflowException>(Assert.Throws<TargetInvocationException>(() => method.Invoke(null, [int.MaxValue, 1])).InnerException);
        else Assert.Equal(expected, method.Invoke(null, [int.MaxValue, 1]));
    }

    [Fact]
    public void NestedFunctionDoesNotInheritBlockContext()
    {
        using var loaded = Load("""
            class Runner {
                static func Run(a: int, b: int) -> int {
                    checked {
                        func Add(x: int, y: int) -> int => x + y
                        return Add(a, b)
                    }
                }
            }
            """);
        Assert.Equal(int.MinValue, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, [int.MaxValue, 1]));
    }

    [Theory]
    [InlineData("checked")]
    [InlineData("unchecked")]
    public void SyntaxRoundTripsAndRetainsBlock(string keyword)
    {
        var text = $"{keyword} {{ var x = 1 }}";
        var tree = SyntaxTree.ParseText(text);
        var statement = Assert.Single(tree.GetRoot().DescendantNodes().OfType<CheckedStatementSyntax>());
        Assert.Equal(keyword, statement.Keyword.Text);
        Assert.Single(statement.Block.Statements);
        Assert.Equal(text, tree.GetRoot().ToFullString());
        Assert.DoesNotContain(tree.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    private static TestAssemblyLoader.LoadedAssembly Load(string source)
    {
        var compilation = Compilation.Create("CheckedBlock_" + Guid.NewGuid().ToString("N"),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(TestMetadataReferences.Default);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        image.Position = 0;
        return TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
    }
}
