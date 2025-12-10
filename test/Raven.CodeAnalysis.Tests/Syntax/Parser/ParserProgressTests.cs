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
}
