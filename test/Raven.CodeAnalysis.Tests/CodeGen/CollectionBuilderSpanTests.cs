using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class CollectionBuilderSpanTests
{
    [Theory]
    [InlineData("", 0)]
    [InlineData("AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)", 1)]
    [InlineData("AccessorDeclaration(SyntaxKind.GetAccessorDeclaration), AccessorDeclaration(SyntaxKind.SetAccessorDeclaration)", 2)]
    public void NonParamsSpanBuilderReceivesAllElements(string elements, int expected)
    {
        var compilation = CreateCompilation($"return AccessorList([{elements}]).Accessors.Count");
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void NonParamsSpanBuilderRejectsIncompatibleElement()
    {
        var compilation = CreateCompilation("return AccessorList([42]).Accessors.Count");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    private static Compilation CreateCompilation(string body)
    {
        var source = $$"""
            import Microsoft.CodeAnalysis.CSharp.*
            import Microsoft.CodeAnalysis.CSharp.SyntaxFactory.*
            class Probe {
                static func Run() -> int { {{body}} }
            }
            """;
        var references = TestMetadataReferences.Default.Concat([
            MetadataReference.CreateFromFile(typeof(Microsoft.CodeAnalysis.SyntaxNode).Assembly.Location),
            MetadataReference.CreateFromFile(typeof(Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree).Assembly.Location)]).ToArray();
        return Compilation.Create("SpanBuilder", [SyntaxTree.ParseText(source)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
    }
}
