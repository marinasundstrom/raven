using System.Text;

using Raven.CodeAnalysis.Syntax;

using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Parser.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class Test2(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void ParseIfStatement()
    {
        var code = """
                   if (foo)  {
                       return 0;
                   } else if (bar ) {
                       return 1;
                   }
                   """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetRoot();
        
        var str = root.ToFullString();

        testOutputHelper.WriteLine(str);

        testOutputHelper.WriteLine(root.GetSyntaxTreeRepresentation(includeSpans: true, includeTrivia: true, includeLocation: true));
    }
}