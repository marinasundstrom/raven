using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal class TypeMemberBinder : Binder
{
    private readonly INamedTypeSymbol _containingType;

    public TypeMemberBinder(Binder parent, INamedTypeSymbol containingType)
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

        return base.LookupSymbol(name);
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        return node switch
        {
            MethodDeclarationSyntax method => BindMethodSymbol(method),
            ConstructorDeclarationSyntax ctor => BindConstructorSymbol(ctor),
            NamedConstructorDeclarationSyntax namedCtor => BindConstructorSymbol(namedCtor),
            PropertyDeclarationSyntax property => BindPropertySymbol(property),
            IndexerDeclarationSyntax indexer => BindIndexerSymbol(indexer),
            AccessorDeclarationSyntax accessor => BindAccessorSymbol(accessor),
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
        if (ctor is NamedConstructorDeclarationSyntax namedCtor)
            name = namedCtor.Identifier.Text;

        return _containingType.GetMembers()
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Name == name &&
                                 m.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == ctor));
    }

    private ISymbol? BindPropertySymbol(PropertyDeclarationSyntax property)
    {
        return _containingType.GetMembers()
            .OfType<IPropertySymbol>()
            .FirstOrDefault(p => !p.IsIndexer &&
                                 p.Name == property.Identifier.Text &&
                                 p.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == property));
    }

    private ISymbol? BindIndexerSymbol(IndexerDeclarationSyntax indexer)
    {
        return _containingType.GetMembers()
            .OfType<IPropertySymbol>()
            .FirstOrDefault(p => p.IsIndexer &&
                                 p.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == indexer));
    }

    private ISymbol? BindAccessorSymbol(AccessorDeclarationSyntax accessor)
    {
        return _containingType.GetMembers()
            .OfType<IPropertySymbol>()
            .SelectMany(p => new[] { p.GetMethod, p.SetMethod }.Where(m => m is not null))
            .FirstOrDefault(m => m!.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == accessor));
    }

    public void BindFieldDeclaration(FieldDeclarationSyntax fieldDecl)
    {
        var isStatic = fieldDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);

        foreach (var decl in fieldDecl.Declaration.Declarators)
        {
            var fieldType = decl.TypeAnnotation is null
                ? Compilation.GetSpecialType(SpecialType.System_Object)
                : ResolveType(decl.TypeAnnotation.Type);

            BoundExpression? initializer = null;
            if (decl.Initializer is not null)
            {
                var exprBinder = new BlockBinder(_containingType, this);
                initializer = exprBinder.BindExpression(decl.Initializer.Value);

                foreach (var diag in exprBinder.Diagnostics.AsEnumerable())
                    _diagnostics.Report(diag);
            }

            _ = new SourceFieldSymbol(
                decl.Identifier.Text,
                fieldType,
                isStatic: isStatic,
                isLiteral: false,
                constantValue: null,
                _containingType,
                _containingType,
                CurrentNamespace!.AsSourceNamespace(),
                [decl.GetLocation()],
                [decl.GetReference()],
                initializer
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
        var isStatic = ctorDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);

        var ctorSymbol = new SourceMethodSymbol(
            ".ctor",
            Compilation.GetSpecialType(SpecialType.System_Void),
            ImmutableArray<SourceParameterSymbol>.Empty,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [ctorDecl.GetLocation()],
            [ctorDecl.GetReference()],
            isStatic: isStatic,
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

    public Dictionary<AccessorDeclarationSyntax, MethodBinder> BindPropertyDeclaration(PropertyDeclarationSyntax propertyDecl)
    {
        var propertyType = ResolveType(propertyDecl.Type.Type);
        var isStatic = propertyDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);

        var propertySymbol = new SourcePropertySymbol(
            propertyDecl.Identifier.Text,
            propertyType,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [propertyDecl.GetLocation()],
            [propertyDecl.GetReference()],
            isStatic: isStatic);

        var binders = new Dictionary<AccessorDeclarationSyntax, MethodBinder>();

        SourceMethodSymbol? getMethod = null;
        SourceMethodSymbol? setMethod = null;

        if (propertyDecl.AccessorList is not null)
        {
            foreach (var accessor in propertyDecl.AccessorList.Accessors)
            {
                bool isGet = accessor.Kind == SyntaxKind.GetAccessorDeclaration;
                var returnType = isGet ? propertyType : Compilation.GetSpecialType(SpecialType.System_Void);
                var name = (isGet ? "get_" : "set_") + propertySymbol.Name;

                var methodSymbol = new SourceMethodSymbol(
                    name,
                    returnType,
                    ImmutableArray<SourceParameterSymbol>.Empty,
                    propertySymbol,
                    _containingType,
                    CurrentNamespace!.AsSourceNamespace(),
                    [accessor.GetLocation()],
                    [accessor.GetReference()],
                    isStatic: isStatic,
                    methodKind: isGet ? MethodKind.PropertyGet : MethodKind.PropertySet);

                var parameters = new List<SourceParameterSymbol>();
                if (!isGet)
                {
                    parameters.Add(new SourceParameterSymbol(
                        "value",
                        propertyType,
                        methodSymbol,
                        _containingType,
                        CurrentNamespace!.AsSourceNamespace(),
                        [accessor.GetLocation()],
                        [accessor.GetReference()]));
                }
                methodSymbol.SetParameters(parameters);

                var binder = new MethodBinder(methodSymbol, this);
                binders[accessor] = binder;

                if (isGet)
                    getMethod = methodSymbol;
                else
                    setMethod = methodSymbol;
            }
        }

        propertySymbol.SetAccessors(getMethod, setMethod);

        return binders;
    }

    public Dictionary<AccessorDeclarationSyntax, MethodBinder> BindIndexerDeclaration(IndexerDeclarationSyntax indexerDecl)
    {
        var propertyType = ResolveType(indexerDecl.Type.Type);

        var propertySymbol = new SourcePropertySymbol(
            indexerDecl.Identifier.Text,
            propertyType,
            _containingType,
            _containingType,
            CurrentNamespace!.AsSourceNamespace(),
            [indexerDecl.GetLocation()],
            [indexerDecl.GetReference()],
            isIndexer: true);

        var binders = new Dictionary<AccessorDeclarationSyntax, MethodBinder>();

        // Prepare indexer parameters
        var indexerParameters = indexerDecl.ParameterList.Parameters
            .Select(p => new { Syntax = p, Type = ResolveType(p.TypeAnnotation!.Type) })
            .ToArray();

        SourceMethodSymbol? getMethod = null;
        SourceMethodSymbol? setMethod = null;

        if (indexerDecl.AccessorList is not null)
        {
            foreach (var accessor in indexerDecl.AccessorList.Accessors)
            {
                bool isGet = accessor.Kind == SyntaxKind.GetAccessorDeclaration;
                var returnType = isGet ? propertyType : Compilation.GetSpecialType(SpecialType.System_Void);
                var name = (isGet ? "get_" : "set_") + propertySymbol.Name;

                var methodSymbol = new SourceMethodSymbol(
                    name,
                    returnType,
                    ImmutableArray<SourceParameterSymbol>.Empty,
                    propertySymbol,
                    _containingType,
                    CurrentNamespace!.AsSourceNamespace(),
                    [accessor.GetLocation()],
                    [accessor.GetReference()],
                    isStatic: false,
                    methodKind: isGet ? MethodKind.PropertyGet : MethodKind.PropertySet);

                var parameters = new List<SourceParameterSymbol>();
                foreach (var param in indexerParameters)
                {
                    parameters.Add(new SourceParameterSymbol(
                        param.Syntax.Identifier.Text,
                        param.Type,
                        methodSymbol,
                        _containingType,
                        CurrentNamespace!.AsSourceNamespace(),
                        [param.Syntax.GetLocation()],
                        [param.Syntax.GetReference()]));
                }
                if (!isGet)
                {
                    parameters.Add(new SourceParameterSymbol(
                        "value",
                        propertyType,
                        methodSymbol,
                        _containingType,
                        CurrentNamespace!.AsSourceNamespace(),
                        [accessor.GetLocation()],
                        [accessor.GetReference()]));
                }
                methodSymbol.SetParameters(parameters);

                var binder = new MethodBinder(methodSymbol, this);
                binders[accessor] = binder;

                if (isGet)
                    getMethod = methodSymbol;
                else
                    setMethod = methodSymbol;
            }
        }

        propertySymbol.SetAccessors(getMethod, setMethod);

        return binders;
    }
}

