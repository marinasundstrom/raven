using System.Collections.Immutable;
using System.Linq;

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
        var container = Compilation.SourceGlobalNamespace.LookupType("Program") as INamedTypeSymbol;
        if (container is null)
            throw new InvalidOperationException("Synthesized Program type not found.");

        var existingMethod = container
            .GetMembers(_syntax.Identifier.ValueText)
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.DeclaringSyntaxReferences.Any(r => r.GetSyntax() == _syntax));

        if (existingMethod is SourceMethodSymbol existingSource)
        {
            _methodSymbol = existingSource;
            return _methodSymbol;
        }

        var isAsync = _syntax.Modifiers.Any(m => m.Kind == SyntaxKind.AsyncKeyword);

        var inferredReturnType = isAsync
            ? Compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task)
            : Compilation.GetSpecialType(SpecialType.System_Unit);

        _methodSymbol = new SourceMethodSymbol(
            _syntax.Identifier.ValueText,
            inferredReturnType,
            [],
            container,
            container,
            container.ContainingNamespace,
            [_syntax.GetLocation()],
            [_syntax.GetReference()],
            isStatic: true,
            isAsync: isAsync,
            declaredAccessibility: Accessibility.Internal);

        if (_syntax.TypeParameterList is { } typeParameterList && typeParameterList.Parameters.Count > 0)
        {
            var typeParametersBuilder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(typeParameterList.Parameters.Count);
            int ordinal = 0;
            foreach (var typeParameterSyntax in typeParameterList.Parameters)
            {
                var (constraintKind, constraintTypeReferences) = AnalyzeTypeParameterConstraints(typeParameterSyntax);
                var variance = GetDeclaredVariance(typeParameterSyntax);

                var typeParameterSymbol = new SourceTypeParameterSymbol(
                    typeParameterSyntax.Identifier.ValueText,
                    _methodSymbol,
                    container,
                    container.ContainingNamespace,
                    [typeParameterSyntax.GetLocation()],
                    [typeParameterSyntax.GetReference()],
                    ordinal++,
                    constraintKind,
                    constraintTypeReferences,
                    variance);
                typeParametersBuilder.Add(typeParameterSymbol);
            }

            _methodSymbol.SetTypeParameters(typeParametersBuilder);
        }

        _methodBodyBinder ??= new MethodBinder(_methodSymbol, this);
        var methodBinder = _methodBodyBinder;

        if (_methodSymbol.TypeParameters.Length > 0)
            methodBinder.EnsureTypeParameterConstraintTypesResolved(_methodSymbol.TypeParameters);

        var hasInvalidAsyncReturnType = false;

        var returnType = _syntax.ReturnType is null
            ? inferredReturnType
            : methodBinder.ResolveType(_syntax.ReturnType.Type);

        if (isAsync && _syntax.ReturnType is { } annotatedReturn && !IsValidAsyncReturnType(returnType))
        {
            var display = returnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
            _diagnostics.ReportAsyncReturnTypeMustBeTaskLike(display, annotatedReturn.Type.GetLocation());
            returnType = Compilation.GetSpecialType(SpecialType.System_Threading_Tasks_Task);
            hasInvalidAsyncReturnType = true;
        }

        _methodSymbol.SetReturnType(returnType);

        if (hasInvalidAsyncReturnType)
            _methodSymbol.MarkAsyncReturnTypeError();

        if (isAsync && _syntax.ReturnType is null)
            _methodSymbol.RequireAsyncReturnTypeInference();

        var parameters = _syntax.ParameterList.Parameters
            .Select(p =>
            {
                var typeSyntax = p.TypeAnnotation.Type;
                var refKind = RefKind.None;
                var isByRefSyntax = typeSyntax is ByRefTypeSyntax;

                if (isByRefSyntax)
                    refKind = p.Modifiers.Any(m => m.Kind == SyntaxKind.OutKeyword) ? RefKind.Out : RefKind.Ref;

                var refKindForType = refKind == RefKind.None && isByRefSyntax ? RefKind.Ref : refKind;
                var type = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                    ? methodBinder.ResolveType(typeSyntax, refKindForType)
                    : methodBinder.ResolveType(typeSyntax);
                var hasDefaultValue = TypeMemberBinder.TryEvaluateParameterDefaultValue(p, type, out var defaultValue);
                return new SourceParameterSymbol(
                    p.Identifier.ValueText,
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
        return _methodBodyBinder ??= new MethodBinder(methodSymbol!, this);
    }

    private static (TypeParameterConstraintKind constraintKind, ImmutableArray<SyntaxReference> constraintTypeReferences) AnalyzeTypeParameterConstraints(TypeParameterSyntax parameter)
    {
        var constraints = parameter.Constraints;
        if (constraints.Count == 0)
            return (TypeParameterConstraintKind.None, ImmutableArray<SyntaxReference>.Empty);

        var constraintKind = TypeParameterConstraintKind.None;
        var typeConstraintReferences = ImmutableArray.CreateBuilder<SyntaxReference>();

        foreach (var constraint in constraints)
        {
            switch (constraint)
            {
                case ClassConstraintSyntax:
                    constraintKind |= TypeParameterConstraintKind.ReferenceType;
                    break;
                case StructConstraintSyntax:
                    constraintKind |= TypeParameterConstraintKind.ValueType;
                    break;
                case TypeConstraintSyntax typeConstraint:
                    constraintKind |= TypeParameterConstraintKind.TypeConstraint;
                    typeConstraintReferences.Add(typeConstraint.GetReference());
                    break;
            }
        }

        return (constraintKind, typeConstraintReferences.ToImmutable());
    }

    private static VarianceKind GetDeclaredVariance(TypeParameterSyntax parameter)
    {
        return parameter.VarianceKeyword?.Kind switch
        {
            SyntaxKind.OutKeyword => VarianceKind.Out,
            SyntaxKind.InKeyword => VarianceKind.In,
            _ => VarianceKind.None,
        };
    }
}
