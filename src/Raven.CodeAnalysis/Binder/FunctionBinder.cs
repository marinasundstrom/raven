using System;
using System.Collections.Generic;
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

        TypeParameterInitializer.InitializeMethodTypeParameters(
            _methodSymbol,
            (INamedTypeSymbol)container, // or container.ContainingType if that’s what you mean by “declaring type context”
            _syntax.TypeParameterList,
            _syntax.ConstraintClauses,   // <-- whatever you named it on FunctionStatementSyntax
            _syntax.SyntaxTree,
            _diagnostics);

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

        var parameters = new List<SourceParameterSymbol>();
        var seenOptionalParameter = false;
        foreach (var p in _syntax.ParameterList.Parameters)
        {
            var typeSyntax = p.TypeAnnotation.Type;
            var refKindTokenKind = p.RefKindKeyword?.Kind;
            var isByRefSyntax = typeSyntax is ByRefTypeSyntax;

            var refKind = isByRefSyntax
                ? refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.Ref,
                }
                : refKindTokenKind switch
                {
                    SyntaxKind.OutKeyword => RefKind.Out,
                    SyntaxKind.InKeyword => RefKind.In,
                    SyntaxKind.RefKeyword => RefKind.Ref,
                    _ => RefKind.None,
                };

            var refKindForType = refKind == RefKind.None && isByRefSyntax ? RefKind.Ref : refKind;
            var type = refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                ? methodBinder.ResolveType(typeSyntax, refKindForType)
                : methodBinder.ResolveType(typeSyntax);

            var isMutable = p.BindingKeyword?.Kind == SyntaxKind.VarKeyword;

            var defaultResult = TypeMemberBinder.ProcessParameterDefault(
                p,
                type,
                p.Identifier.ValueText,
                _diagnostics,
                ref seenOptionalParameter);

            parameters.Add(new SourceParameterSymbol(
                p.Identifier.ValueText,
                type,
                _methodSymbol,
                container.ContainingType,
                container.ContainingNamespace,
                [p.GetLocation()],
                [p.GetReference()],
                refKind,
                defaultResult.HasExplicitDefaultValue,
                defaultResult.ExplicitDefaultValue,
                isMutable));
        }

        _methodSymbol.SetParameters(parameters);
        return _methodSymbol;
    }

    public MethodBinder GetMethodBodyBinder()
    {
        var methodSymbol = GetMethodSymbol();
        return _methodBodyBinder ??= new MethodBinder(methodSymbol!, this);
    }
}
