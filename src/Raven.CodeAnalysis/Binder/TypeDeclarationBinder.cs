using System.Collections.Immutable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using System.Collections.Generic;

namespace Raven.CodeAnalysis;

internal class TypeDeclarationBinder : Binder
{
    private readonly INamedTypeSymbol _containingType;

    public TypeDeclarationBinder(Binder parent, INamedTypeSymbol containingType)
        : base(parent)
    {
        _containingType = containingType;
    }

    public new ITypeSymbol ContainingSymbol => _containingType;

    public override ISymbol? LookupSymbol(string name)
    {
        var symbol = ContainingSymbol.GetMembers(name).FirstOrDefault();
        if (symbol is not null)
            return symbol;

        var parentSymbol1 = ParentBinder?.LookupSymbol(name);
        if (parentSymbol1 != null)
            return parentSymbol1;

        return base.LookupSymbol(name);
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        return node switch
        {
            ClassDeclarationSyntax => _containingType,
            MethodDeclarationSyntax method => BindMethodSymbol(method),
            ConstructorDeclarationSyntax ctor => BindConstructorSymbol(ctor),
            NamedConstructorDeclarationSyntax namedCtor => BindConstructorSymbol(namedCtor),
            VariableDeclaratorSyntax variable => BindFieldSymbol(variable),
            _ => base.BindDeclaredSymbol(node)
        };
    }

    private ISymbol? BindFieldSymbol(VariableDeclaratorSyntax variable)
    {
        return _containingType.GetMembers()
            .OfType<IFieldSymbol>()
            .FirstOrDefault(f => f.Name == variable.Identifier.Text &&
                                 f.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == variable));
    }

    private ISymbol? BindMethodSymbol(MethodDeclarationSyntax method)
    {
        return _containingType.GetMembers()
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Name == method.Identifier.Text &&
                                 m.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == method));
    }

    private ISymbol? BindConstructorSymbol(BaseConstructorDeclarationSyntax ctor)
    {
        string name = ".ctor";

        if (ctor is NamedConstructorDeclarationSyntax namedConstructor)
        {
            name = namedConstructor.Identifier.Text;
        }

        return _containingType.GetMembers()
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Name == name &&
                                 m.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == ctor));
    }

    public void BindFieldDeclaration(FieldDeclarationSyntax fieldDecl)
    {
        foreach (var decl in fieldDecl.Declaration.Declarators)
        {
            var fieldType = decl.TypeAnnotation is null
                ? Compilation.GetSpecialType(SpecialType.System_Object)
                : ResolveType(decl.TypeAnnotation.Type);

            _ = new SourceFieldSymbol(
                decl.Identifier.Text,
                fieldType,
                isStatic: false,
                isLiteral: false,
                constantValue: null,
                _containingType,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [decl.GetLocation()],
                [decl.GetReference()]
            );
        }
    }

    public MethodBinder BindMethodDeclaration(MethodDeclarationSyntax methodDecl)
    {
        var returnType = methodDecl.ReturnType is null
            ? Compilation.GetSpecialType(SpecialType.System_Void)
            : ResolveType(methodDecl.ReturnType.Type);

        var methodSymbol = new SourceMethodSymbol(
            methodDecl.Identifier.Text,
            returnType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [methodDecl.GetLocation()],
            [methodDecl.GetReference()],
            isStatic: false);

        var parameters = new List<SourceParameterSymbol>();
        foreach (var p in methodDecl.ParameterList.Parameters)
        {
            var pType = ResolveType(p.TypeAnnotation!.Type);
            var pSymbol = new SourceParameterSymbol(
                p.Identifier.Text,
                pType,
                methodSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [p.GetLocation()],
                [p.GetReference()]
            );
            parameters.Add(pSymbol);
        }

        methodSymbol.SetParameters(parameters);
        return new MethodBinder(methodSymbol, this);
    }

    public MethodBinder BindConstructorDeclaration(ConstructorDeclarationSyntax ctorDecl)
    {
        var ctorSymbol = new SourceMethodSymbol(
            ".ctor",
            Compilation.GetSpecialType(SpecialType.System_Void),
            ImmutableArray<SourceParameterSymbol>.Empty,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [ctorDecl.GetLocation()],
            [ctorDecl.GetReference()],
            isStatic: false,
            methodKind: MethodKind.Constructor);

        var parameters = new List<SourceParameterSymbol>();
        foreach (var p in ctorDecl.ParameterList.Parameters)
        {
            var pType = ResolveType(p.TypeAnnotation!.Type);
            var pSymbol = new SourceParameterSymbol(
                p.Identifier.Text,
                pType,
                ctorSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [p.GetLocation()],
                [p.GetReference()]
            );
            parameters.Add(pSymbol);
        }

        ctorSymbol.SetParameters(parameters);
        return new MethodBinder(ctorSymbol, this);
    }

    public MethodBinder BindNamedConstructorDeclaration(NamedConstructorDeclarationSyntax ctorDecl)
    {
        var ctorSymbol = new SourceMethodSymbol(
            ctorDecl.Identifier.Text,
            _containingType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [ctorDecl.GetLocation()],
            [ctorDecl.GetReference()],
            isStatic: true,
            methodKind: MethodKind.NamedConstructor);

        var parameters = new List<SourceParameterSymbol>();
        foreach (var p in ctorDecl.ParameterList.Parameters)
        {
            var pType = ResolveType(p.TypeAnnotation!.Type);
            var pSymbol = new SourceParameterSymbol(
                p.Identifier.Text,
                pType,
                ctorSymbol,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [p.GetLocation()],
                [p.GetReference()]
            );
            parameters.Add(pSymbol);
        }

        ctorSymbol.SetParameters(parameters);
        return new MethodBinder(ctorSymbol, this);
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