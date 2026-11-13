using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Tests.Syntax;

[Trait("Target", "Shared")]
public sealed class TupleTypeRecoveryTests
{
    [Theory]
    [InlineData("(int First, int Second)", 4)]
    [InlineData("(int string)", 2)]
    [InlineData("(first: int second: string)", 2)]
    public void MissingSeparatorReportsDiagnosticAndPreservesElementList(string type, int count)
    {
        var context = new BaseParseContext(new Lexer(new StringReader(type)));
        var tuple = Assert.IsType<TupleTypeSyntax>(new NameSyntaxParser(context).ParseTypeName().CreateRed());
        Assert.NotEmpty(context.Diagnostics);
        Assert.Equal(count, tuple.Elements.Count);
        Assert.All(tuple.Elements, element => Assert.NotNull(element.Type));
        Assert.Equal(count - 1, tuple.Elements.GetSeparators().Count());
        Assert.Contains(tuple.Elements.GetSeparators(), separator => separator.IsMissing);
    }
}
