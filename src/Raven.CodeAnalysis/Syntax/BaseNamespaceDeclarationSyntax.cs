

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

public abstract class BaseNamespaceDeclarationSyntax : MemberDeclarationSyntax
{
    internal BaseNamespaceDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public virtual NameSyntax Name { get; }

    public virtual SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public virtual SyntaxList<MemberDeclarationSyntax> Members { get; }
}