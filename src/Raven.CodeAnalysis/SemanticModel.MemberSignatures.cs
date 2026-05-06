using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private bool _memberSignaturesDeclared;

    internal bool MemberSignaturesDeclared => _memberSignaturesDeclared;

    internal void EnsureMemberSignaturesDeclared()
    {
        if (_memberSignaturesDeclared)
            return;

        foreach (var methodDeclaration in SyntaxTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
        {
            MemberSignatureDeclarationPass.DeclareMethodSignature(this, methodDeclaration);
        }

        foreach (var propertyDeclaration in SyntaxTree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>())
        {
            MemberSignatureDeclarationPass.DeclarePropertySignature(this, propertyDeclaration);
        }

        foreach (var eventDeclaration in SyntaxTree.GetRoot().DescendantNodes().OfType<EventDeclarationSyntax>())
        {
            MemberSignatureDeclarationPass.DeclareEventSignature(this, eventDeclaration);
        }

        _memberSignaturesDeclared = true;
    }
}
