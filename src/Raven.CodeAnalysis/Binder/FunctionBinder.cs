using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class FunctionBinder : Binder
{
    private readonly FunctionStatementSyntax _syntax;
    private MethodBinder? _methodBodyBinder;
    private SourceMethodSymbol? _methodSymbol;

    public FunctionBinder(Binder parent, FunctionStatementSyntax syntax)
        : base(parent)
    {
        _syntax = syntax;
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        if (node == _syntax)
        {
            return GetMethodSymbol();
        }

        return base.BindDeclaredSymbol(node);
    }

    public IMethodSymbol GetMethodSymbol()
    {
        if (_methodSymbol is not null)
            return _methodSymbol;

        //ISymbol container = null; //this.ContainingSymbol;
        var container = Compilation.SourceRootNamespace.LookupType("Program") as INamedTypeSymbol;
        if (container is null)
            throw new InvalidOperationException("Synthesized Program type not found.");

        var existingMethod = container
            .GetMembers(_syntax.Identifier.Text)
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == _syntax));

        if (existingMethod is SourceMethodSymbol existingSource)
        {
            _methodSymbol = existingSource;
            return _methodSymbol;
        }

        var returnType = _syntax.ReturnType is null
            ? Compilation.GetSpecialType(SpecialType.System_Unit)
            : ResolveType(_syntax.ReturnType.Type);

        _methodSymbol = new SourceMethodSymbol(
            _syntax.Identifier.Text,
            returnType,
            [],
            container,
            container,
            container.ContainingNamespace,
            [_syntax.GetLocation()],
            [_syntax.GetReference()],
            isStatic: true,
            declaredAccessibility: Accessibility.Internal);

        var parameters = _syntax.ParameterList.Parameters
            .Select(p =>
            {
                var typeSyntax = p.TypeAnnotation.Type;
                var refKind = RefKind.None;
                if (typeSyntax is ByRefTypeSyntax byRefSyntax)
                {
                    refKind = p.Modifiers.Any(m => m.Kind == SyntaxKind.OutKeyword) ? RefKind.Out : RefKind.Ref;
                    typeSyntax = byRefSyntax.ElementType;
                }

                var type = ResolveType(typeSyntax);
                var hasDefaultValue = TypeMemberBinder.TryEvaluateParameterDefaultValue(p, type, out var defaultValue);
                return new SourceParameterSymbol(
                    p.Identifier.Text,
                    type,
                    _methodSymbol,
                    container.ContainingType,
                    container.ContainingNamespace,
                    [p.GetLocation()],
                    [p.GetReference()],
                    refKind,
                    hasDefaultValue,
                    defaultValue);
            })
            .ToArray();

        _methodSymbol.SetParameters(parameters);
        return _methodSymbol;
    }

    public MethodBinder GetMethodBodyBinder()
    {
        var methodSymbol = GetMethodSymbol();
        return _methodBodyBinder ??= new MethodBinder(methodSymbol!, this, methodSymbol.Parameters);
    }
}
