#if RAVEN_PORT
extern alias Port;
#endif
using Raven.CodeAnalysis.Text;
#if RAVEN_PORT
using ParsingFactory = Port::Raven.CodeAnalysis.Syntax.SyntaxFactory;
#else
using ParsingFactory = Raven.CodeAnalysis.Syntax.SyntaxFactory;
#endif

namespace Raven.CodeAnalysis.Syntax.Tests;

[Trait("Target", "Shared")]
public sealed class SyntaxFactoryParsingContractTests
{
    [Theory]
    [InlineData("expression", "1 + 2", SyntaxKind.AddExpression)]
    [InlineData("statement", "return 1", SyntaxKind.ReturnStatement)]
    [InlineData("type", "System.String", SyntaxKind.QualifiedName)]
    [InlineData("pattern", "> 1 and < 10", SyntaxKind.AndPattern)]
    public void StringAndSourceTextOverloadsAgree(string family, string source, SyntaxKind expected)
    {
        var text = SourceText.From(source);
        SyntaxNode fromString = family switch
        {
            "expression" => ParsingFactory.ParseExpression(source),
            "statement" => ParsingFactory.ParseStatement(source),
            "type" => ParsingFactory.ParseType(source),
            _ => ParsingFactory.ParsePattern(source)
        };
        SyntaxNode fromText = family switch
        {
            "expression" => ParsingFactory.ParseExpression(text),
            "statement" => ParsingFactory.ParseStatement(text),
            "type" => ParsingFactory.ParseType(text),
            _ => ParsingFactory.ParsePattern(text)
        };
        Assert.Equal(expected, fromString.Kind);
        Assert.Equal(expected, fromText.Kind);
        Assert.Equal(source, fromString.ToFullString());
        Assert.Equal(source, fromText.ToFullString());
    }

    [Fact]
    public void SourceTextFragmentOverloadsHonorOffset()
    {
        Assert.Equal("1 + 2", ParsingFactory.ParseExpression(SourceText.From("skip 1 + 2"), position: 5).ToFullString());
        Assert.Equal("return 1", ParsingFactory.ParseStatement(SourceText.From("skip return 1"), position: 5).ToFullString());
        Assert.Equal("System.String", ParsingFactory.ParseType(SourceText.From("skip System.String"), position: 5).ToFullString());
        Assert.Equal("> 1", ParsingFactory.ParsePattern(SourceText.From("skip > 1"), position: 5).ToFullString());
    }

    [Theory]
    [InlineData("class C {}", true)]
    [InlineData("class C {} class D {}", false)]
    [InlineData("let x = 1", false)]
    [InlineData("import System.*\nclass C {}", false)]
    [InlineData("", false)]
    public void MemberParsingRequiresExactlyOneDeclaration(string source, bool valid)
    {
        var member = ParsingFactory.ParseMemberDeclaration(source);
        var textMember = ParsingFactory.ParseMemberDeclaration(SourceText.From(source));
        Assert.Equal(valid, member is not null);
        Assert.Equal(valid, textMember is not null);
        if (valid)
        {
            Assert.Equal(SyntaxKind.ClassDeclaration, member!.Kind);
            Assert.Equal(source, member.ToFullString());
            Assert.Equal(source, textMember!.ToFullString());
        }
    }

    [Theory]
    [InlineData("class C {")]
    [InlineData("let value = (1 +")]
    public void CompilationUnitParsingRetainsDiagnosticsAndSourceOwnership(string source)
    {
        var root = ParsingFactory.ParseCompilationUnit(source);
        Assert.NotNull(root.SyntaxTree);
        Assert.Same(root, root.SyntaxTree.GetRoot());
        Assert.Equal(source, root.SyntaxTree.GetText().ToString());
        Assert.True(root.ContainsDiagnostics);
        Assert.NotEmpty(root.GetDiagnostics());
        Assert.All(root.GetDiagnostics(), diagnostic => Assert.Same(root.SyntaxTree, diagnostic.Location.SourceTree));
    }

    [Fact]
    public void DetachedExpressionDiagnosticsRetainOriginalSourceCoordinates()
    {
        const string source = "prefix\n[1";
        var expression = ParsingFactory.ParseExpression(SourceText.From(source), position: 7);
        Assert.Null(expression.SyntaxTree);
        var diagnostic = Assert.Single(expression.GetDiagnostics());
        Assert.True(expression.ContainsDiagnostics);
        Assert.Single(expression.GetDiagnostics(traverse: false));
        Assert.Equal(new TextSpan(source.Length, 0), diagnostic.Location.SourceSpan);
        Assert.Equal(new LinePosition(1, 2), diagnostic.Location.GetLineSpan().StartLinePosition);
        Assert.Null(diagnostic.Location.SourceTree);
    }

    [Fact]
    public void CompilationUnitOverloadsPreserveSource()
    {
        const string source = "import System.*\nclass C {}\n";
        var root = ParsingFactory.ParseCompilationUnit(source);
        var fromText = ParsingFactory.ParseCompilationUnit(SourceText.From(source));
        Assert.Single(root.Imports);
        Assert.Single(root.Members);
        Assert.Equal(source, root.ToFullString());
        Assert.Equal(source, fromText.ToFullString());
    }
}
