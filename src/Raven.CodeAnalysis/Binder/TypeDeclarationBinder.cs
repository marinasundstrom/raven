

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class TypeDeclarationBinder : Binder
{
    private readonly INamedTypeSymbol _containingType;

    public TypeDeclarationBinder(Binder parent, INamedTypeSymbol containingType)
        : base(parent)
    {
        _containingType = containingType;
    }

    public ISymbol ContainingSymbol => _containingType;

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        return node switch
        {
            MethodDeclarationSyntax method => BindMethodSymbol(method),
            FieldDeclarationSyntax field => BindFieldSymbol(field),
            _ => base.BindDeclaredSymbol(node)
        };
    }

    private ISymbol? BindFieldSymbol(FieldDeclarationSyntax field)
    {
        foreach (var decl in field.Declaration.Declarators)
        {
            var match = _containingType.GetMembers()
                .OfType<IFieldSymbol>()
                .FirstOrDefault(f => f.Name == decl.Identifier.Text);

            if (match != null)
                return match;
        }

        return null;
    }

    private ISymbol? BindMethodSymbol(MethodDeclarationSyntax method)
    {
        return _containingType.GetMembers()
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Name == method.Identifier.Text &&
                                 m.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == method));
    }

    // Implement BindMethodSymbol / BindFieldSymbol as needed
}