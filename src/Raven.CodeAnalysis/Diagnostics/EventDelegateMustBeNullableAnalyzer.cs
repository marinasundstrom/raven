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
        messageFormat: "Event '{0}' has non-nullable delegate type '{1}'; consider '{2}'",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
        => context.RegisterSyntaxTreeAction(AnalyzeTree);

    private static void AnalyzeTree(SyntaxTreeAnalysisContext context)
    {
        var semanticModel = context.Compilation.GetSemanticModel(context.SyntaxTree);
        var root = context.SyntaxTree.GetRoot();

        foreach (var ev in root.DescendantNodes().OfType<EventDeclarationSyntax>())
            AnalyzeEventDeclaration(context, semanticModel, ev);
    }

    private static void AnalyzeEventDeclaration(
        SyntaxTreeAnalysisContext context,
        SemanticModel semanticModel,
        EventDeclarationSyntax ev)
    {
        // No type annotation => nothing actionable to suggest.
        if (ev.Type is not TypeAnnotationClauseSyntax annotation)
            return;

        if (semanticModel.GetDeclaredSymbol(ev) is not IEventSymbol symbol)
            return;

        var type = symbol.Type;

        // Only applies to delegate(-ish) event types.
        if (!type.ContainsDelegateType())
            return;

        // Already nullable (or union where all alternatives are nullable) => OK.
        if (type.IsEffectivelyNullable())
            return;

        Report(context, symbol, annotation.Type, type);
    }

    private static void Report(
        SyntaxTreeAnalysisContext context,
        IEventSymbol symbol,
        SyntaxNode typeNode,
        ITypeSymbol type)
    {
        var typeDisplay = type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var nullableSuggestion = typeDisplay + "?";
        var location = typeNode.GetLocation();

        context.ReportDiagnostic(Diagnostic.Create(
            Descriptor,
            location,
            symbol.Name,
            typeDisplay,
            nullableSuggestion));
    }
}

public static class Ex
{
    public static bool ContainsDelegateType(this ITypeSymbol type)
        => type is ITypeUnionSymbol union
            ? union.Types.Any(IsDelegateType)
            : IsDelegateType(type);

    public static bool IsEffectivelyNullable(this ITypeSymbol type)
        => type is ITypeUnionSymbol union
            ? union.Types.All(IsNullable)
            : IsNullable(type);

    public static bool IsDelegateType(ITypeSymbol type)
        => type is INamedTypeSymbol named && named.TypeKind == TypeKind.Delegate;

    public static bool IsNullable(ITypeSymbol type)
        => type.IsNullable;
}
