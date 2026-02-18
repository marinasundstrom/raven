using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Suggests rewriting explicit union-case qualification into target-typed form when the local can carry
/// the explicit type annotation.
/// </summary>
public sealed class PreferTargetTypedUnionCaseAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9010";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Variable infers union case type",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "This binds to '{0}' (a case). If you meant the union, use '{1}'.",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(AnalyzeLocalDeclaration, SyntaxKind.LocalDeclarationStatement);
    }

    private static void AnalyzeLocalDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (!SuggestionsDiagnosticProperties.IsSuggestionModeEnabled(context.Compilation))
            return;

        if (context.Node is not LocalDeclarationStatementSyntax localDeclaration)
            return;

        if (!TryCreateSuggestion(localDeclaration, context.SemanticModel, out var suggestion))
            return;

        var properties = SuggestionsDiagnosticProperties.CreateRewriteSuggestion(
            suggestion.OriginalStatementText,
            suggestion.RewrittenStatementText);

        var diagnostic = new Diagnostic(
            Descriptor,
            suggestion.Location,
            [suggestion.InferredCaseTypeText, suggestion.UnionTypeText],
            properties: properties);

        context.ReportDiagnostic(diagnostic);
    }

    internal static bool TryCreateSuggestion(
        LocalDeclarationStatementSyntax localDeclaration,
        SemanticModel semanticModel,
        out Suggestion suggestion)
    {
        suggestion = default;

        if (localDeclaration.Declaration.Declarators.Count != 1)
            return false;

        var declarator = localDeclaration.Declaration.Declarators[0];
        if (declarator.TypeAnnotation is not null || declarator.Initializer is null)
            return false;

        var initializer = declarator.Initializer.Value;
        if (!TryGetUnionCaseAccess(initializer, out var access))
            return false;

        if (semanticModel.GetDeclaredSymbol(declarator) is not ILocalSymbol local)
            return false;

        var localType = local.Type;
        if (localType is null || localType.TypeKind == TypeKind.Error)
            return false;

        var qualifiedType = semanticModel.GetTypeInfo(access.MemberAccess.Expression).Type;
        if (qualifiedType is null || qualifiedType.TypeKind == TypeKind.Error)
            return false;

        var union = qualifiedType.TryGetDiscriminatedUnion();
        if (union is null)
            return false;

        var inferredCase = localType.TryGetDiscriminatedUnionCase();
        if (inferredCase is null)
            return false;

        if (!SymbolEqualityComparer.Default.Equals(inferredCase.Union, union))
            return false;

        var caseName = access.MemberAccess.Name switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.ValueText,
            GenericNameSyntax generic => generic.Identifier.ValueText,
            _ => access.MemberAccess.Name.ToString()
        };

        if (!union.Cases.Any(c => string.Equals(c.Name, caseName, StringComparison.Ordinal)))
            return false;

        var typeText = qualifiedType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var inferredCaseTypeText = localType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var bindingKeyword = localDeclaration.Declaration.BindingKeyword.Text;
        var identifierText = declarator.Identifier.Text;
        var rewritten = $"{bindingKeyword} {identifierText}: {typeText} = {access.TargetTypedExpressionText}";

        if (localDeclaration.TerminatorToken.Kind == SyntaxKind.SemicolonToken)
            rewritten += localDeclaration.TerminatorToken.Text;

        var original = localDeclaration.ToString().TrimEnd();
        if (string.Equals(original, rewritten, StringComparison.Ordinal))
            return false;

        suggestion = new Suggestion(access.MemberAccess.Expression.GetLocation(), original, rewritten, inferredCaseTypeText, typeText);
        return true;
    }

    private static bool TryGetUnionCaseAccess(ExpressionSyntax initializer, out UnionCaseAccess access)
    {
        access = default;

        if (initializer is InvocationExpressionSyntax invocation &&
            invocation.Expression is MemberAccessExpressionSyntax invocationMember)
        {
            var targetTyped = "." + invocationMember.Name + invocation.ArgumentList +
                              (invocation.Initializer?.ToString() ?? string.Empty);
            access = new UnionCaseAccess(invocationMember, targetTyped);
            return true;
        }

        if (initializer is MemberAccessExpressionSyntax memberAccess)
        {
            access = new UnionCaseAccess(memberAccess, "." + memberAccess.Name);
            return true;
        }

        return false;
    }

    internal readonly record struct Suggestion(
        Location Location,
        string OriginalStatementText,
        string RewrittenStatementText,
        string InferredCaseTypeText,
        string UnionTypeText);

    private readonly record struct UnionCaseAccess(
        MemberAccessExpressionSyntax MemberAccess,
        string TargetTypedExpressionText);
}
