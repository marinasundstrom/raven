using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports explicitly-qualified union case construction where a target union type is already known
/// (for example, invocation arguments with a discriminated-union parameter).
/// </summary>
public sealed class PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9011";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Prefer target-typed union case syntax",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Target type is already '{0}' here — you can use a case directly.",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(AnalyzeInvocation, SyntaxKind.InvocationExpression);
        context.RegisterSyntaxNodeAction(AnalyzeLocalDeclaration, SyntaxKind.LocalDeclarationStatement);
    }

    private static void AnalyzeInvocation(SyntaxNodeAnalysisContext context)
    {
        if (!SuggestionsDiagnosticProperties.IsSuggestionModeEnabled(context.Compilation))
            return;

        if (context.Node is not InvocationExpressionSyntax invocation)
            return;

        // Receiver-binding invocations can be present inside conditional-access forms (e.g. `f?(2)`).
        // Binding these directly through GetSymbolInfo can throw for unsupported paths; they are not
        // relevant to this analyzer's target-typed union-case preference.
        if (invocation.Expression is ReceiverBindingExpressionSyntax)
            return;

        IMethodSymbol? method;
        try
        {
            method = context.SemanticModel.GetSymbolInfo(invocation).Symbol as IMethodSymbol;
        }
        catch (NotSupportedException)
        {
            return;
        }

        if (method is null)
            return;

        var arguments = invocation.ArgumentList.Arguments;
        for (var i = 0; i < arguments.Count; i++)
        {
            var argument = arguments[i];
            if (!TryGetTargetParameter(method, arguments, i, out var parameter))
                continue;

            if (!TryCreateSuggestion(argument.Expression, parameter.Type, context.SemanticModel, out var suggestion))
                continue;

            var properties = SuggestionsDiagnosticProperties.CreateRewriteSuggestion(
                suggestion.OriginalExpressionText,
                suggestion.RewrittenExpressionText);

            var diagnostic = new Diagnostic(
                Descriptor,
                suggestion.Location,
                [suggestion.TargetUnionTypeText],
                properties: properties);

            context.ReportDiagnostic(diagnostic);
        }
    }

    private static bool TryCreateSuggestion(
        ExpressionSyntax expression,
        ITypeSymbol targetType,
        SemanticModel semanticModel,
        out Suggestion suggestion)
    {
        suggestion = default;

        if (!TryGetUnionCaseAccess(expression, out var access))
            return false;

        var targetUnion = targetType.TryGetDiscriminatedUnion();
        if (targetUnion is null)
            return false;

        var qualifiedType = semanticModel.GetTypeInfo(access.MemberAccess.Expression).Type;
        if (qualifiedType is null || qualifiedType.TypeKind == TypeKind.Error)
            return false;

        var qualifiedUnion = qualifiedType.TryGetDiscriminatedUnion();
        if (qualifiedUnion is null)
            return false;

        if (!SymbolEqualityComparer.Default.Equals(qualifiedUnion, targetUnion))
            return false;

        var rewritten = access.TargetTypedExpressionText;
        var original = expression.ToString().TrimEnd();

        if (string.Equals(original, rewritten, StringComparison.Ordinal))
            return false;

        var targetTypeText = targetUnion.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
        suggestion = new Suggestion(expression.GetLocation(), original, rewritten, targetTypeText);
        return true;
    }

    private static void AnalyzeLocalDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (!SuggestionsDiagnosticProperties.IsSuggestionModeEnabled(context.Compilation))
            return;

        if (context.Node is not LocalDeclarationStatementSyntax localDeclaration)
            return;

        foreach (var declarator in localDeclaration.Declaration.Declarators)
        {
            if (declarator.TypeAnnotation is null || declarator.Initializer is null)
                continue;

            var targetType = context.SemanticModel.GetTypeInfo(declarator.TypeAnnotation.Type).Type;
            if (targetType is null || targetType.TypeKind == TypeKind.Error)
                continue;

            if (!TryCreateSuggestion(declarator.Initializer.Value, targetType, context.SemanticModel, out var suggestion))
                continue;

            var properties = SuggestionsDiagnosticProperties.CreateRewriteSuggestion(
                suggestion.OriginalExpressionText,
                suggestion.RewrittenExpressionText);

            var diagnostic = new Diagnostic(
                Descriptor,
                suggestion.Location,
                [suggestion.TargetUnionTypeText],
                properties: properties);

            context.ReportDiagnostic(diagnostic);
        }
    }

    private static bool TryGetTargetParameter(
        IMethodSymbol method,
        SeparatedSyntaxList<ArgumentSyntax> arguments,
        int argumentIndex,
        out IParameterSymbol parameter)
    {
        parameter = default!;

        var argument = arguments[argumentIndex];
        if (argument.NameColon is not null)
        {
            var name = argument.NameColon.Name.Identifier.ValueText;
            var named = method.Parameters.FirstOrDefault(p => string.Equals(p.Name, name, StringComparison.Ordinal));
            if (named is null)
                return false;

            parameter = named;
            return true;
        }

        if (argumentIndex >= method.Parameters.Length)
            return false;

        parameter = method.Parameters[argumentIndex];
        return true;
    }

    private static bool TryGetUnionCaseAccess(ExpressionSyntax expression, out UnionCaseAccess access)
    {
        access = default;

        if (expression is InvocationExpressionSyntax invocation &&
            invocation.Expression is MemberAccessExpressionSyntax invocationMember)
        {
            var targetTyped = "." + invocationMember.Name + invocation.ArgumentList +
                              (invocation.Initializer?.ToString() ?? string.Empty);
            access = new UnionCaseAccess(invocationMember, targetTyped);
            return true;
        }

        if (expression is MemberAccessExpressionSyntax memberAccess)
        {
            access = new UnionCaseAccess(memberAccess, "." + memberAccess.Name);
            return true;
        }

        return false;
    }

    internal readonly record struct Suggestion(
        Location Location,
        string OriginalExpressionText,
        string RewrittenExpressionText,
        string TargetUnionTypeText);

    private readonly record struct UnionCaseAccess(
        MemberAccessExpressionSyntax MemberAccess,
        string TargetTypedExpressionText);
}
