
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class MethodDeclarationSyntax : MemberDeclarationSyntax
{
    public MethodDeclarationSyntax(TypeSyntax returnType, IdentifierNameSyntax name, ParameterListSyntax parameters)
        : base(SyntaxKind.MethodDeclaration,
              [
                      returnType,
                      name,
                      parameters,
                      null
              ])
    {
    }

    public MethodDeclarationSyntax(TypeSyntax returnType, IdentifierNameSyntax name, ParameterListSyntax parameters, BlockSyntax body)
    : base(SyntaxKind.MethodDeclaration,
          [
                  returnType,
                name,
                parameters,
                body
          ])
    {
    }
}