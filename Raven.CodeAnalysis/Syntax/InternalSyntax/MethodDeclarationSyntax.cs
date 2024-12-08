namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class MethodDeclarationSyntax : SyntaxNode
{
    public MethodDeclarationSyntax(TypeSyntax returnType, IdentifierNameSyntax name, TypeParameterListSyntax parameters)
        : base(SyntaxKind.MethodDeclaration,
              [
                      returnType,
                      name,
                      parameters
              ],
              returnType.FullWidth + name.FullWidth + parameters.FullWidth)
    {
    }
}
