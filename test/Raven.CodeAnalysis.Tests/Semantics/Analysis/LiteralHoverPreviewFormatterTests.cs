using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Analysis;

public sealed class LiteralHoverPreviewFormatterTests
{
    [Fact]
    public void TryCreatePreview_BinaryLiteral_ShowsEvaluatedValue()
    {
        var (semanticModel, token) = CreateModelAndSingleLiteralToken("val x = 0b101");

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(semanticModel, token, out var preview, out _);

        Assert.True(success);
        Assert.Equal("int = 5", preview);
    }

    [Fact]
    public void TryCreatePreview_HexLiteral_ShowsEvaluatedValue()
    {
        var (semanticModel, token) = CreateModelAndSingleLiteralToken("val x = 0x1F");

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(semanticModel, token, out var preview, out _);

        Assert.True(success);
        Assert.Equal("int = 31", preview);
    }

    [Fact]
    public void TryCreatePreview_StringLiteral_ShowsEscapedDecodedPreview()
    {
        var (semanticModel, token) = CreateModelAndSingleLiteralToken("val text = \"A\\nB\"");

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(semanticModel, token, out var preview, out _);

        Assert.True(success);
        Assert.Equal("string = \"A\\nB\"", preview);
    }

    [Fact]
    public void TryCreatePreview_EncodedStringLiteral_ShowsEncodingSuffix()
    {
        var (semanticModel, token) = CreateModelAndSingleLiteralToken("val text = \"Hej\"u8");

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(semanticModel, token, out var preview, out _);

        Assert.True(success);
        Assert.Contains("(utf8)", preview);
        Assert.Contains("\"Hej\"", preview);
    }

    private static (SemanticModel semanticModel, SyntaxToken token) CreateModelAndSingleLiteralToken(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var token = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<LiteralExpressionSyntax>()
            .Select(static literal => literal.Token)
            .Single();

        return (semanticModel, token);
    }
}
