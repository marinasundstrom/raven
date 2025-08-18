using System.Linq;
using System.Collections.Immutable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal class TypeDeclarationBinder : Binder
{
    private readonly INamedTypeSymbol _containingType;

    public TypeDeclarationBinder(Binder parent, INamedTypeSymbol containingType)
        : base(parent)
    {
        _containingType = containingType;
    }

    public new INamedTypeSymbol ContainingSymbol => _containingType;

    public override ISymbol? LookupSymbol(string name)
    {
        var symbol = _containingType.GetMembers(name).FirstOrDefault();
        if (symbol is not null)
            return symbol;

        var parentSymbol = ParentBinder?.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        return base.LookupSymbol(name);
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        return node switch
        {
            ClassDeclarationSyntax => _containingType,
            _ => base.BindDeclaredSymbol(node)
        };
    }

    public void EnsureDefaultConstructor(ClassDeclarationSyntax classDecl)
    {
        if (_containingType is INamedTypeSymbol named && !named.Constructors.Any(x => x.Parameters.Length == 0))
        {
            _ = new SourceMethodSymbol(
                ".ctor",
                Compilation.GetSpecialType(SpecialType.System_Void),
                ImmutableArray<SourceParameterSymbol>.Empty,
                _containingType,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [classDecl.GetLocation()],
                [classDecl.GetReference()],
                isStatic: false,
                methodKind: MethodKind.Constructor);
        }
    }
}
