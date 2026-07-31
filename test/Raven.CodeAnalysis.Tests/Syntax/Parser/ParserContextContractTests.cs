using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ParserContextContractTests
{
    [Fact]
    public void SyntaxParser_UsesParseContextParenthesisState()
    {
        Assert.Equal(typeof(ParseContext), typeof(SyntaxParser).GetMethod(nameof(ParseContext.EnterParens))?.DeclaringType);
        Assert.Equal(typeof(ParseContext), typeof(SyntaxParser).GetMethod(nameof(ParseContext.ExitParens))?.DeclaringType);
        Assert.Equal(typeof(ParseContext), typeof(SyntaxParser).GetProperty(nameof(ParseContext.IsInsideParens))?.DeclaringType);
    }

    [Fact]
    public async Task LanguageParser_ConcurrentCallsUseIndependentLexers()
    {
        var parser = new LanguageParser("test.rvn", new ParseOptions());
        var sources = Enumerable.Range(0, 32)
            .Select(index => $"let value{index} = {index}")
            .ToArray();

        var tasks = sources
            .Select(source => Task.Run(() => parser.Parse(SourceText.From(source))))
            .ToArray();
        var results = await Task.WhenAll(tasks);

        for (var i = 0; i < results.Length; i++)
        {
            Assert.Empty(results[i].Diagnostics);
            Assert.Equal(sources[i], results[i].Root.CreateRed().ToFullString());
        }
    }
}
