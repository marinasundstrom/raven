using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class Compilation : SourceDeclarationServices
{
    System.Collections.Immutable.ImmutableArray<Symbol> SourceDeclarationServices.GetFunctionContractMembers(TypeSymbol owner)
        => GetFunctionContractMembers(owner);

    bool SourceDeclarationServices.UsesDeferredSourceAttributes
        => Options.TargetPlatform == TargetPlatform.NeoCLR && Options.MetadataImportOptions?.UseSourceCore == true;

    bool SourceDeclarationServices.SourceDeclarationsComplete => SourceDeclarationsComplete;

    SourceAttributeDeclarationContext SourceDeclarationServices.GetAttributeDeclarationContext(Symbol owner)
        => new(owner is SynthesizedNamespaceMembersClassSymbol,
            owner is SourceMethodSymbol { MethodKind: MethodKind.Constructor },
            owner is UnionCaseTypeSymbol,
            owner is SourcePropertySymbol,
            owner is SourceEventSymbol,
            owner is FieldSymbol { AssociatedSymbol: SourcePropertySymbol { EmitAsFieldOnly: true } });

    bool SourceDeclarationServices.TryGetCurrentDeclaringSyntax(SyntaxReference reference, out SyntaxNode syntax)
        => TryGetCurrentSourceDeclaration(reference, out syntax, out _);

    SourceAttributeBinding? SourceDeclarationServices.BindAttribute(Symbol owner, AttributeSyntax syntax)
    {
        var model = GetSemanticModel(syntax.SyntaxTree!);
        var binder = model.GetBinder((SyntaxNode?)syntax.Parent ?? syntax);
        var attributeBinder = binder as AttributeBinder ?? new AttributeBinder(owner, binder);
        var data = AttributeDataFactory.Create(attributeBinder.BindAttribute(syntax), syntax);
        return data is null ? null : new SourceAttributeBinding(data, syntax, attributeBinder.Diagnostics);
    }

    void SourceDeclarationServices.PublishAttributeDiagnostics(SourceAttributeBinding attribute)
        => GetSemanticModel(attribute.Syntax.SyntaxTree!).AddDeclarationDiagnostics(attribute.Diagnostics.AsEnumerable());

    internal bool TryGetCurrentSourceDeclaration(
        SyntaxReference syntaxReference,
        out SyntaxNode syntax,
        out SemanticModel semanticModel)
    {
        if (TryGetSemanticModel(syntaxReference.SyntaxTree, out semanticModel))
        {
            syntax = syntaxReference.GetSyntax();
            return true;
        }

        var staleSyntax = syntaxReference.GetSyntax();
        var staleFilePath = syntaxReference.SyntaxTree.FilePath;
        if (string.IsNullOrWhiteSpace(staleFilePath))
        {
            syntax = null!;
            semanticModel = null!;
            return false;
        }

        var currentTree = SyntaxTrees.FirstOrDefault(tree =>
            string.Equals(tree.FilePath, staleFilePath, StringComparison.OrdinalIgnoreCase));
        if (currentTree is null ||
            !TryGetSemanticModel(currentTree, out semanticModel))
        {
            syntax = null!;
            semanticModel = null!;
            return false;
        }

        var root = currentTree.GetRoot();
        if (!root.FullSpan.Contains(syntaxReference.Span))
        {
            syntax = null!;
            semanticModel = null!;
            return false;
        }

        var candidate = root.FindNode(syntaxReference.Span, getInnermostNodeForTie: true);
        for (var current = candidate; current is not null; current = current.Parent)
        {
            if (current.Kind == staleSyntax.Kind && current.Span == syntaxReference.Span)
            {
                syntax = current;
                return true;
            }
        }

        syntax = null!;
        semanticModel = null!;
        return false;
    }

}
