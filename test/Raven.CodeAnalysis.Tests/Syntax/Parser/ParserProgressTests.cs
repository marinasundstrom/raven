using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ParserProgressTests
{
    [Fact]
    public void ExpressionLoop_AllowsEndOfFileAfterExpression()
    {
        const string code = """
namespace System

import System.*

public union Option<T> {
    Some(value: T)
    None
}

public extension OptionExtensions<T> for Option<T> {
    public UnwrapOrDefault() -> T {
        if self is .Some(value) {
            return value
        }

        return default(T)
    }
}
""";

        var tree = SyntaxTree.ParseText(code);

        Assert.DoesNotContain(
            tree.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.ParserMadeNoProgress);
    }

    [Fact]
    public void InvalidStatements_StillInsertMissingNodes()
    {
        const string code = """
import System.Console.*
import System.Threading.Tasks.*

val value = 42;

WriteLine("Value: $value")
val result = await Task.Run(async () => { 
    WriteLine("Entered" x; 
    await Task.Delay(200);
    WriteLine("Waited
    return value
}) 
WriteLine("Result: $result")
""";

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();

        Assert.Contains(root.DescendantTokens(), token => token.IsMissing);

        Assert.DoesNotContain(
            tree.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.ParserMadeNoProgress);
    }
}
