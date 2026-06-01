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
        Assert.Equal("0b101: int = 5", preview);
    }

    [Fact]
    public void TryCreatePreview_HexLiteral_ShowsEvaluatedValue()
    {
        var (semanticModel, token) = CreateModelAndSingleLiteralToken("val x = 0x1F");

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(semanticModel, token, out var preview, out _);

        Assert.True(success, $"token={token.Kind} parent={token.Parent?.GetType().Name} text={token.Text}");
        Assert.Equal("0x1F: int = 31", preview);
    }

    [Fact]
    public void TryCreatePreview_StringLiteral_ShowsEscapedDecodedPreview()
    {
        var (semanticModel, token) = CreateModelAndSingleLiteralToken("val text = \"A\\nB\"");

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(semanticModel, token, out var preview, out _);

        Assert.True(success);
        Assert.Equal("\"A\\nB\": string", preview);
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
        Assert.Equal("0: int", preview);
    }

    [Fact]
    public void TryCreatePreview_DefaultParameterValue_ShowsTypedDefault()
    {
        const string code = """
func Do(no: int = default) -> unit { }
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var defaultExpression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<DefaultExpressionSyntax>()
            .Single();
        var token = syntaxTree.GetRoot().FindToken(defaultExpression.Span.Start);
        Assert.Equal(SyntaxKind.DefaultKeyword, token.Kind);

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(
            semanticModel,
            token,
            out var preview,
            out var span);

        Assert.True(success);
        Assert.Equal("default(int) = 0", preview);
        Assert.Equal(defaultExpression.Span, span);
    }

    [Fact]
    public void TryCreatePreview_BooleanDefault_ShowsFalseValue()
    {
        const string code = """
val flag: bool = default
""";

        var (semanticModel, token, defaultExpression) = CreateDefaultPreviewModel(code);

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(
            semanticModel,
            token,
            out var preview,
            out var span);

        Assert.True(success);
        Assert.Equal("default(bool) = false", preview);
        Assert.Equal(defaultExpression.Span, span);
    }

    [Fact]
    public void TryCreatePreview_DoubleDefault_ShowsZeroValue()
    {
        const string code = """
val amount: double = default
""";

        var (semanticModel, token, defaultExpression) = CreateDefaultPreviewModel(code);

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(
            semanticModel,
            token,
            out var preview,
            out var span);

        Assert.True(success);
        Assert.Equal("default(double) = 0.0", preview);
        Assert.Equal(defaultExpression.Span, span);
    }

    [Fact]
    public void TryCreatePreview_StructDefault_KeepsDefaultExpression()
    {
        const string code = """
struct Point {
    val X: int
}

val point: Point = default
""";

        var (semanticModel, token, defaultExpression) = CreateDefaultPreviewModel(code);

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(
            semanticModel,
            token,
            out var preview,
            out var span);

        Assert.True(success);
        Assert.Equal("default(Point)", preview);
        Assert.Equal(defaultExpression.Span, span);
    }

    [Fact]
    public void TryCreatePreview_NullableReferenceDefault_ShowsNullValue()
    {
        const string code = """
import System.*

val resource: IDisposable? = default
""";

        var (semanticModel, token, defaultExpression) = CreateDefaultPreviewModel(code);

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(
            semanticModel,
            token,
            out var preview,
            out var span);

        Assert.True(success);
        Assert.Equal("default(IDisposable?) = null", preview);
        Assert.Equal(defaultExpression.Span, span);
    }

    [Fact]
    public void TryCreatePreview_NullableValueTypeDefault_ShowsNullValue()
    {
        const string code = """
val maybe: int? = default
""";

        var (semanticModel, token, defaultExpression) = CreateDefaultPreviewModel(code);

        var success = LiteralHoverPreviewFormatter.TryCreatePreview(
            semanticModel,
            token,
            out var preview,
            out var span);

        Assert.True(success);
        Assert.Equal("default(int?) = null", preview);
        Assert.Equal(defaultExpression.Span, span);
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

    private static (SemanticModel semanticModel, SyntaxToken token, DefaultExpressionSyntax defaultExpression) CreateDefaultPreviewModel(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var defaultExpression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<DefaultExpressionSyntax>()
            .Single();
        var token = syntaxTree.GetRoot().FindToken(defaultExpression.Span.Start);
        Assert.Equal(SyntaxKind.DefaultKeyword, token.Kind);

        return (semanticModel, token, defaultExpression);
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
