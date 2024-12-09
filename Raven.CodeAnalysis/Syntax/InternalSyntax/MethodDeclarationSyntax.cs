
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
              ],
              returnType.FullWidth + name.FullWidth + parameters.FullWidth)
    {
    }

    public MethodDeclarationSyntax(TypeSyntax returnType, IdentifierNameSyntax name, TypeParameterListSyntax parameters, BlockSyntax body)
    : base(SyntaxKind.MethodDeclaration,
          [
                  returnType,
                name,
                parameters,
                body
          ],
          returnType.FullWidth + name.FullWidth + parameters.FullWidth)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.MethodDeclarationSyntax(this, parent);
    }
}