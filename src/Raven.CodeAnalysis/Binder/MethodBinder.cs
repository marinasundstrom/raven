using System.Collections.Generic;
using System.Linq;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class MethodBinder : TypeMemberBinder
{
    private readonly IMethodSymbol _methodSymbol;

    public MethodBinder(IMethodSymbol methodSymbol, Binder parent)
        : base(parent, (INamedTypeSymbol)methodSymbol.ContainingType!)
    {
        _methodSymbol = methodSymbol;
    }

    public MethodBinder(IMethodSymbol methodSymbol, Binder parent, IEnumerable<IParameterSymbol> parameters)
        : this(methodSymbol, parent)
    {
        // Parameters are retrieved from _methodSymbol; no additional storage needed
    }

    public override ITypeSymbol? LookupType(string name)
    {
        var methodTypeParameter = _methodSymbol.TypeParameters.FirstOrDefault(tp => tp.Name == name);
        if (methodTypeParameter is not null)
            return methodTypeParameter;

        return base.LookupType(name);
    }

    public override ISymbol? LookupSymbol(string name)
    {
        var paramSymbol = _methodSymbol.Parameters.FirstOrDefault(p => p.Name == name);
        if (paramSymbol is not null)
            return paramSymbol;

        var parentSymbol = base.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        return Compilation.GlobalNamespace.GetMembers(name).FirstOrDefault();
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        if (node is MethodDeclarationSyntax
            or ConstructorDeclarationSyntax
            or NamedConstructorDeclarationSyntax
            or AccessorDeclarationSyntax
            or PropertyDeclarationSyntax)
            return _methodSymbol;

        if (node is ParameterSyntax parameter)
        {
            return _methodSymbol.Parameters.FirstOrDefault(p =>
                p.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == parameter));
        }

        if (node is ArrowTypeClauseSyntax)
            return _methodSymbol;

        return base.BindDeclaredSymbol(node);
    }

    public IMethodSymbol GetMethodSymbol() => _methodSymbol;

    protected override bool IsInUnsafeContext
    {
        get
        {
            foreach (var reference in _methodSymbol.DeclaringSyntaxReferences)
            {
                var syntax = reference.GetSyntax();
                var hasUnsafeModifier = syntax switch
                {
                    MethodDeclarationSyntax methodDeclaration => methodDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.UnsafeKeyword),
                    ConstructorDeclarationSyntax constructorDeclaration => constructorDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.UnsafeKeyword),
                    NamedConstructorDeclarationSyntax namedConstructorDeclaration => namedConstructorDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.UnsafeKeyword),
                    FunctionStatementSyntax functionStatement => functionStatement.Modifiers.Any(m => m.Kind == SyntaxKind.UnsafeKeyword),
                    _ => false,
                };

                if (hasUnsafeModifier)
                    return true;
            }

            return base.IsInUnsafeContext;
        }
    }
}
