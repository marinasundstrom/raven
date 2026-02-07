using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class NonNullDeclarationsAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9008";
    public const string OptionSuggestionDiagnosticId = "RAV9012";

    private static readonly DiagnosticDescriptor NullableDeclarationDescriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Nullable declaration type is not allowed",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Declaration type '{0}' is nullable and not recommended",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Warning);

    private static readonly DiagnosticDescriptor OptionSuggestionDescriptor = DiagnosticDescriptor.Create(
        id: OptionSuggestionDiagnosticId,
        title: "Prefer Option<T> over nullable declaration types",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Use '{0}' instead of a nullable declaration type",
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

        var typeDisplay = FormatType(type);
        var nullableDiagnostic = Diagnostic.Create(NullableDeclarationDescriptor, typeSyntax.GetLocation(), typeDisplay);
        context.ReportDiagnostic(nullableDiagnostic);

        if (!TryBuildOptionSuggestion(type, out var optionSuggestion))
            return;

        var suggestionDiagnostic = Diagnostic.Create(OptionSuggestionDescriptor, typeSyntax.GetLocation(), optionSuggestion);
        context.ReportDiagnostic(suggestionDiagnostic);
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
