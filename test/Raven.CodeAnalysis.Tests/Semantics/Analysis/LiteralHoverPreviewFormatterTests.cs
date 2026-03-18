using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
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

    [Fact]
    public void TryCreatePreview_AttachedMacroInitializer_UsesExpandedInitializerBinding()
    {
        const string code = """
class CounterViewModel {
    #[Observable]
    var Count: int = 0
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(ObservableMacroPlugin)));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var token = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<LiteralExpressionSyntax>()
            .Select(static literal => literal.Token)
            .Single();

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(semanticModel, token, out var preview, out _);

        Assert.True(success);
        Assert.Equal("int = 0", preview);
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

    private sealed class ObservableMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "ObservableMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ObservableMacro()];
    }

    private sealed class ObservableMacro : IAttachedDeclarationMacro
    {
        public string Name => "Observable";
        public MacroKind Kind => MacroKind.AttachedDeclaration;
        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var property = (PropertyDeclarationSyntax)context.TargetDeclaration;
            var backingFieldName = "_" + property.Identifier.ValueText;

            var backingField = SyntaxFactory.StoredPropertyDeclaration(
                SyntaxFactory.List<AttributeListSyntax>(),
                SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PrivateKeyword)),
                SyntaxFactory.Token(SyntaxKind.VarKeyword),
                SyntaxFactory.Identifier(backingFieldName),
                property.Type,
                property.Initializer);

            var replacement = property
                .WithAttributeLists(SyntaxFactory.List<AttributeListSyntax>())
                .WithAccessorList(SyntaxFactory.AccessorList(
                    SyntaxFactory.List<AccessorDeclarationSyntax>(
                    [
                        SyntaxFactory.AccessorDeclaration(
                            SyntaxKind.GetAccessorDeclaration,
                            SyntaxFactory.List<AttributeListSyntax>(),
                            SyntaxFactory.TokenList(),
                            SyntaxFactory.Token(SyntaxKind.GetKeyword),
                            SyntaxFactory.ArrowExpressionClause(SyntaxFactory.IdentifierName(backingFieldName))),
                        SyntaxFactory.AccessorDeclaration(
                            SyntaxKind.SetAccessorDeclaration,
                            SyntaxFactory.List<AttributeListSyntax>(),
                            SyntaxFactory.TokenList(),
                            SyntaxFactory.Token(SyntaxKind.SetKeyword),
                            SyntaxFactory.BlockStatement(SyntaxFactory.List<StatementSyntax>()))
                    ])))
                .WithExpressionBody(null)
                .WithInitializer(null)
                .WithTerminatorToken(SyntaxFactory.Token(SyntaxKind.None));

            return new MacroExpansionResult
            {
                IntroducedMembers = [backingField],
                ReplacementDeclaration = replacement
            };
        }
    }
}
