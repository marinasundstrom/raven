using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ImportDirectiveTriviaTests
{
    [Fact]
    public void MultilineCommentBeforeImports_DoesNotSplitWildcardDirective()
    {
        var code = """
                   /*

                   BUG: This seems to be because of the ordering of the declarations interface types can't be created or resolved:
                   I think this is a matter of fixing the compiler passes.
                   Also, make sure that similar issue is not occurring in other places.
                   */
                   import System.*;
                   import System.Console.*;

                   WriteLine(20)
                   """;

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();

        root.Imports.Count.ShouldBe(2);
        root.Imports[0].Name.ToString().ShouldBe("System.*");
        root.Imports[1].Name.ToString().ShouldBe("System.Console.*");
    }
}
