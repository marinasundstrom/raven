
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class MethodDeclarationSyntax : MemberDeclarationSyntax
{
    public MethodDeclarationSyntax(TypeSyntax returnType, IdentifierNameSyntax name, TypeParameterListSyntax parameters)
        : base(SyntaxKind.MethodDeclaration,
              [
                      returnType,
                      name,
                      parameters,
                      null
              ])
    {
    }

    public MethodDeclarationSyntax(TypeSyntax returnType, IdentifierNameSyntax name, TypeParameterListSyntax parameters, BlockSyntax body)
    : base(SyntaxKind.MethodDeclaration,
          [
                  returnType,
                name,
                parameters,
                body
          ])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.MethodDeclarationSyntax(this, parent, position);
    }
}