
namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class TestSyntaxVisitor2 : SyntaxVisitor<string>
{
    public override string DefaultVisit(SyntaxNode node)
    {
        return node.ToString();
    }
}
