using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class NonNullDeclarationsAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9012";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Option/Result over nullable types",
        description: "Nullable types are allowed, but Option/Result usually makes absence and failure clearer.",
        helpLinkUri: string.Empty,
        messageFormat: "Use '{0}' instead of '{1}'.",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(
            AnalyzeTypeAnnotationClause,
            SyntaxKind.TypeAnnotationClause);

        context.RegisterSyntaxNodeAction(
            AnalyzeArrowTypeClause,
            SyntaxKind.ArrowTypeClause);

        context.RegisterSyntaxNodeAction(
            AnalyzeCatchDeclaration,
            SyntaxKind.CatchDeclaration);
    }

    private static void AnalyzeTypeAnnotationClause(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not TypeAnnotationClauseSyntax annotation)
            return;

        if (!IsDeclarationTypeAnnotation(annotation.Parent))
            return;

        AnalyzeTypeSyntax(context, annotation.Type);
    }

    private static void AnalyzeArrowTypeClause(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not ArrowTypeClauseSyntax returnType)
            return;

        if (!IsDeclarationReturnType(returnType.Parent))
            return;

        AnalyzeTypeSyntax(context, returnType.Type);
    }

    private static void AnalyzeCatchDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not CatchDeclarationSyntax catchDeclaration)
            return;

        AnalyzeTypeSyntax(context, catchDeclaration.Type);
    }

    private static void AnalyzeTypeSyntax(SyntaxNodeAnalysisContext context, TypeSyntax typeSyntax)
    {
        var type = context.SemanticModel.GetTypeInfo(typeSyntax).Type;
        if (type is null || type is ErrorTypeSymbol)
            return;

        if (!IsNullableDeclarationType(type))
            return;

        var fromDisplay = FormatType(type);

        // Prefer reporting only when we can produce a concrete replacement.
        if (!TryBuildOptionSuggestion(type, out var optionSuggestion))
            return;

        var diagnostic = Diagnostic.Create(
            Descriptor,
            typeSyntax.GetLocation(),
            optionSuggestion,
            fromDisplay);

        context.ReportDiagnostic(diagnostic);
    }

    private static bool IsDeclarationTypeAnnotation(SyntaxNode? parent)
        => parent is ParameterSyntax
            or VariableDeclaratorSyntax
            or PropertyDeclarationSyntax
            or EventDeclarationSyntax
            or IndexerDeclarationSyntax
            or TypedVariableDesignationSyntax;

    private static bool IsDeclarationReturnType(SyntaxNode? parent)
        => parent is FunctionStatementSyntax
            or MethodDeclarationSyntax
            or DelegateDeclarationSyntax
            or OperatorDeclarationSyntax
            or ConversionOperatorDeclarationSyntax;

    private static bool IsNullableDeclarationType(ITypeSymbol type)
    {
        if (type.IsNullable || type.TypeKind == TypeKind.Null)
            return true;

        if (type is not ITypeUnionSymbol union)
            return false;

        foreach (var member in union.Types)
        {
            if (IsNullableDeclarationType(member))
                return true;
        }

        return false;
    }

    private static bool TryBuildOptionSuggestion(ITypeSymbol nullableType, out string optionSuggestion)
    {
        optionSuggestion = string.Empty;

        var nonNullableAlternatives = CollectNonNullableAlternatives(nullableType);
        if (nonNullableAlternatives.Count == 0)
            return false;

        var payload = nonNullableAlternatives.Count == 1
            ? nonNullableAlternatives[0]
            : string.Join(" | ", nonNullableAlternatives);

        optionSuggestion = $"Option<{payload}>";
        return true;
    }

    private static List<string> CollectNonNullableAlternatives(ITypeSymbol type)
    {
        var items = new List<string>();
        Collect(type, items);
        return items;
    }

    private static void Collect(ITypeSymbol type, List<string> items)
    {
        if (type.TypeKind == TypeKind.Null)
            return;

        if (type is ITypeUnionSymbol union)
        {
            foreach (var member in union.Types)
                Collect(member, items);

            return;
        }

        if (type.IsNullable)
        {
            var underlying = type.StripNullable();
            if (underlying is not null)
                Collect(underlying, items);

            return;
        }

        var display = FormatType(type);
        if (!items.Contains(display, StringComparer.Ordinal))
            items.Add(display);
    }

    private static string FormatType(ITypeSymbol type)
    {
        if (type is ITypeUnionSymbol union)
        {
            var parts = new List<string>();
            foreach (var member in union.Types)
            {
                var memberDisplay = FormatType(member);
                if (!parts.Contains(memberDisplay, StringComparer.Ordinal))
                    parts.Add(memberDisplay);
            }

            return string.Join(" | ", parts);
        }

        return type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
    }
}
