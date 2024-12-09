

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

public abstract class BaseNamespaceDeclarationSyntax : MemberDeclarationSyntax
{
    public BaseNamespaceDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent) : base(greenNode, parent)
    {
    }

    public BaseNamespaceDeclarationSyntax(GreenNode greenNode, SyntaxTree syntaxTree) : base(greenNode, syntaxTree)
    {
    }
}
