using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroFragmentCursorContractTests
{
    [Theory]
    [InlineData("Expression")]
    [InlineData("Statement")]
    [InlineData("Type")]
    [InlineData("Pattern")]
    [InlineData("MemberDeclaration")]
    public void FragmentParsingUsesTheWrapperAndPreservesResults(string kind)
    {
        var parser = new Parser();
        var inner = new Cursor();
        var stream = new MacroTokenStream(parser, inner);
        object result = kind switch
        {
            "Expression" => stream.ParseExpression(),
            "Statement" => stream.ParseStatement(),
            "Type" => stream.ParseType(),
            "Pattern" => stream.ParsePattern(),
            _ => stream.ParseMemberDeclaration()
        };
        Assert.Same(stream, parser.Stream);
        Assert.Equal(kind, parser.Kind);
        Assert.Same(parser.Result, result);
        Assert.Equal(1, inner.Reads);
        Assert.True(stream.IsEndOfFile);
    }

    [Fact]
    public void PeekForwardsOffsetWithoutAdvancingAndReadAdvances()
    {
        var inner = new Cursor();
        var stream = new MacroTokenStream(new Parser(), inner);
        Assert.False(stream.IsEndOfFile);
        Assert.Equal(inner.Token, stream.PeekToken(3));
        Assert.Equal(3, inner.LastOffset);
        Assert.Equal(0, inner.Reads);
        Assert.Equal(inner.Token, stream.ReadToken());
        Assert.True(stream.IsEndOfFile);
    }

    [Fact]
    public void ParserFailurePropagatesWithoutExtraReads()
    {
        var failure = new InvalidOperationException("parse failed");
        var parser = new Parser { Failure = failure };
        var inner = new Cursor();
        var stream = new MacroTokenStream(parser, inner);
        Assert.Same(failure, Assert.Throws<InvalidOperationException>(() => stream.ParseExpression()));
        Assert.Equal(0, inner.Reads);
    }

    [Fact]
    public void ConstructorPreservesValidationNamesAndOrder()
    {
        Assert.Equal("context", Assert.Throws<ArgumentNullException>(() => new MacroTokenStream(null!, null!)).ParamName);
        Assert.Equal("inner", Assert.Throws<ArgumentNullException>(() => new MacroTokenStream(new Parser(), null!)).ParamName);
        Assert.True(typeof(MacroTokenStream).IsSealed);
        Assert.Empty(typeof(MacroTokenStream).GetConstructors());
    }

    private sealed class Cursor : IMacroTokenStream
    {
        public SyntaxToken Token { get; } = SyntaxFactory.ParseExpression("42").GetFirstToken();
        public int Reads { get; private set; }
        public int LastOffset { get; private set; }
        public bool IsEndOfFile => Reads > 0;

        public SyntaxToken PeekToken(int offset = 0)
        {
            LastOffset = offset;
            return Token;
        }

        public SyntaxToken ReadToken()
        {
            Reads++;
            return Token;
        }
    }

    private sealed class Parser : MacroFragmentParser
    {
        public IMacroTokenStream? Stream { get; private set; }
        public string? Kind { get; private set; }
        public object? Result { get; private set; }
        public Exception? Failure { get; init; }

        public MacroSyntaxParseResult<ExpressionSyntax> ParseExpression(IMacroTokenStream stream)
            => Parse("Expression", stream, SyntaxFactory.ParseExpression("42"));

        public MacroSyntaxParseResult<StatementSyntax> ParseStatement(IMacroTokenStream stream)
            => Parse("Statement", stream, SyntaxFactory.ParseStatement("return 42"));

        public MacroSyntaxParseResult<TypeSyntax> ParseType(IMacroTokenStream stream)
            => Parse("Type", stream, SyntaxFactory.ParseType("int"));

        public MacroSyntaxParseResult<PatternSyntax> ParsePattern(IMacroTokenStream stream)
            => Parse("Pattern", stream, SyntaxFactory.ParsePattern("_"));

        public MacroSyntaxParseResult<MemberDeclarationSyntax> ParseMemberDeclaration(IMacroTokenStream stream)
            => Parse("MemberDeclaration", stream, SyntaxFactory.ParseCompilationUnit("class Example {}").Members[0]);

        private MacroSyntaxParseResult<TSyntax> Parse<TSyntax>(string kind, IMacroTokenStream stream, TSyntax syntax) where TSyntax : SyntaxNode
        {
            if (Failure is { } failure)
                throw failure;
            Stream = stream;
            Kind = kind;
            stream.ReadToken();
            var result = new MacroSyntaxParseResult<TSyntax>(syntax, new TextSpan(2, 3), 5, []);
            Result = result;
            return result;
        }
    }
}
