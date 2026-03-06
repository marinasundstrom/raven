using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class ConstructorParameterNamingAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9023";
    internal const string SuggestedNameProperty = "SuggestedName";
    internal const string SymbolKindProperty = "SymbolKindDisplay";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Constructor parameter naming",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Constructor parameter '{0}' should use {1}.",
        category: "Naming",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(AnalyzeParameter, SyntaxKind.Parameter);
    }

    private static void AnalyzeParameter(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not ParameterSyntax parameterSyntax)
            return;

        if (parameterSyntax.Parent is not ParameterListSyntax parameterList)
            return;

        var parameterName = parameterSyntax.Identifier.ValueText;
        if (string.IsNullOrEmpty(parameterName))
            return;

        var style = GetExpectedStyle(parameterList, parameterSyntax);
        if (style is null)
            return;

        var matches = style == NamingStyle.PascalCase
            ? IsPascalCase(parameterName)
            : IsCamelCase(parameterName);
        if (matches)
            return;

        var suggestedName = style == NamingStyle.PascalCase
            ? ToPascalCase(parameterName)
            : ToCamelCase(parameterName);
        if (string.IsNullOrEmpty(suggestedName) || string.Equals(suggestedName, parameterName, StringComparison.Ordinal))
            return;

        var symbolKindDisplay = GetSymbolKindDisplay(parameterList, parameterSyntax);
        var properties = ImmutableDictionary<string, string?>.Empty
            .Add(SuggestedNameProperty, suggestedName)
            .Add(SymbolKindProperty, symbolKindDisplay);

        context.ReportDiagnostic(new Diagnostic(
            Descriptor,
            parameterSyntax.Identifier.GetLocation(),
            [parameterName, style == NamingStyle.PascalCase ? "PascalCase naming" : "camelCase naming"],
            properties: properties));
    }

    private static NamingStyle? GetExpectedStyle(ParameterListSyntax parameterList, ParameterSyntax parameterSyntax)
    {
        if (parameterList.Parent is ConstructorDeclarationSyntax)
            return NamingStyle.CamelCase;

        if (parameterList.Parent is not TypeDeclarationSyntax typeDeclaration)
            return null;

        var refKeywordKind = parameterSyntax.RefKindKeyword.Kind;
        var refKindIsNone = refKeywordKind is SyntaxKind.None;
        var typeIsByRef = parameterSyntax.TypeAnnotation?.Type is ByRefTypeSyntax;
        var isByRef = typeIsByRef || refKeywordKind is SyntaxKind.RefKeyword or SyntaxKind.OutKeyword or SyntaxKind.InKeyword;
        if (!refKindIsNone || isByRef)
            return NamingStyle.CamelCase;

        var isRecord = typeDeclaration is RecordDeclarationSyntax;
        var bindingKeyword = parameterSyntax.BindingKeyword.Kind;
        var isPromoted = isRecord || bindingKeyword is SyntaxKind.ValKeyword or SyntaxKind.VarKeyword;
        if (!isPromoted)
            return NamingStyle.CamelCase;

        var accessibilityKeyword = parameterSyntax.AccessibilityKeyword.Kind;
        var isPublicPromotion = accessibilityKeyword is SyntaxKind.None or SyntaxKind.PublicKeyword;
        return isPublicPromotion ? NamingStyle.PascalCase : null;
    }

    private static string GetSymbolKindDisplay(ParameterListSyntax parameterList, ParameterSyntax parameterSyntax)
    {
        if (parameterList.Parent is ConstructorDeclarationSyntax)
            return "parameter";

        if (parameterList.Parent is not TypeDeclarationSyntax typeDeclaration)
            return "parameter";

        var isRecord = typeDeclaration is RecordDeclarationSyntax;
        var bindingKeyword = parameterSyntax.BindingKeyword.Kind;
        var isPromoted = isRecord || bindingKeyword is SyntaxKind.ValKeyword or SyntaxKind.VarKeyword;
        return isPromoted ? "property" : "parameter";
    }

    private static bool IsCamelCase(string value) => value.Length > 0 && char.IsLower(value[0]);

    private static bool IsPascalCase(string value) => value.Length > 0 && char.IsUpper(value[0]);

    private static string ToCamelCase(string value)
    {
        if (value.Length == 0)
            return value;

        var first = value[0];
        return !char.IsLetter(first)
            ? value
            : char.ToLowerInvariant(first) + value[1..];
    }

    private static string ToPascalCase(string value)
    {
        if (value.Length == 0)
            return value;

        var first = value[0];
        return !char.IsLetter(first)
            ? value
            : char.ToUpperInvariant(first) + value[1..];
    }

    private enum NamingStyle
    {
        CamelCase,
        PascalCase,
    }
}
