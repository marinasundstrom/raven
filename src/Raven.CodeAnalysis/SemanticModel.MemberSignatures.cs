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

        _memberSignaturesDeclared = true;

        var root = SyntaxTree.GetRoot();
        EnsureDeclarations();
        using var sourceNamespaceLookupSuppression = Compilation.SuppressSourceNamespaceLookupDeclarationCompletion();
        var compilationUnitBinder = GetBinderForIncrementalSemanticQuery(root);
        BindNamespaceMembers(
            root,
            compilationUnitBinder,
            Compilation.GlobalNamespace,
            bindMemberSignatures: true);

    }
}
