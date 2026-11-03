using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class EventDelegateMustBeNullableAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9003";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Event delegate type must be nullable",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Event '{0}' uses '{1}', which can't be null. Use '{2}' if the event can be empty.",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
        => context.RegisterSyntaxNodeAction(AnalyzeEventDeclaration, SyntaxKind.EventDeclaration);

    private static void AnalyzeEventDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not EventDeclarationSyntax ev)
            return;

        // No type annotation => nothing actionable to suggest.
        if (ev.Type is not TypeAnnotationClauseSyntax annotation)
            return;

        if (context.SemanticModel.GetDeclaredSymbol(ev) is not EventSymbol symbol)
            return;

        var type = symbol.Type;

        // Only applies to delegate(-ish) event types.
        if (!type.ContainsDelegateType())
            return;

        // Already nullable (or union where all alternatives are nullable) => OK.
        if (type.IsEffectivelyNullable())
            return;

        Report(context.ReportDiagnostic, symbol, annotation.Type, type);
    }

    private static void Report(
        Action<Diagnostic> reportDiagnostic,
        EventSymbol symbol,
        SyntaxNode typeNode,
        TypeSymbol type)
    {
        var typeDisplay = type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var nullableSuggestion = typeDisplay + "?";
        var location = typeNode.GetLocation();

        reportDiagnostic(Diagnostic.Create(
            Descriptor,
            location,
            symbol.Name,
            typeDisplay,
            nullableSuggestion));
    }
}

public static class Ex
{
    public static bool ContainsDelegateType(this TypeSymbol type)
        => IsDelegateType(type);

    public static bool IsEffectivelyNullable(this TypeSymbol type)
        => IsNullable(type);

    public static bool IsDelegateType(TypeSymbol type)
        => type is NamedTypeSymbol named && named.TypeKind == TypeKind.Delegate;

    public static bool IsNullable(TypeSymbol type)
        => type.IsNullable;
}
