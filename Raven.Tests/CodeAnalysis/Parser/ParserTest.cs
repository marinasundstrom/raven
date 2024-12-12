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

    [Fact]
    public void ParseIfStatementWithElseClause()
    {
        var code = """
                   if (foo)  {
                       return 0;
                   } else {
                   
                       return 1;
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var str = syntaxTree.GetSyntaxRoot().ToFullString();

        testOutputHelper.WriteLine(str);
    }

    [Fact]
    public void ParseIfStatementWithElseClause2()
    {
        var code = """
                   if (foo)  {
                       return 0;
                   } else if (bar ) {
                       return 1;
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetSyntaxRoot();

        var str = root.ToFullString();

        testOutputHelper.WriteLine(str);

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation());
    }
}