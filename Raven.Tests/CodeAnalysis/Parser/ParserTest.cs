using Raven.CodeAnalysis.Syntax;

using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Parser.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class ParserTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void ParseIfStatement()
    {
        var code = """
        if (foo)  {
            return 0;
        }
        """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var str = syntaxTree.GetSyntaxRoot().ToFullString();

        testOutputHelper.WriteLine(str);
    }
}