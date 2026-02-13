using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class PreferNewLineBetweenDeclarationsAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV1051";

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(AnalyzeCompilationUnit, SyntaxKind.CompilationUnit);
    }

    private static void AnalyzeCompilationUnit(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not CompilationUnitSyntax compilationUnit)
            return;

        AnalyzeMemberList(context, compilationUnit.Members);

        foreach (var member in compilationUnit.Members)
            AnalyzeNestedMembers(context, member);
    }

    private static void AnalyzeNestedMembers(SyntaxNodeAnalysisContext context, MemberDeclarationSyntax member)
    {
        switch (member)
        {
            case BaseNamespaceDeclarationSyntax namespaceDeclaration:
                AnalyzeMemberList(context, namespaceDeclaration.Members);
                foreach (var nested in namespaceDeclaration.Members)
                    AnalyzeNestedMembers(context, nested);
                break;

            case TypeDeclarationSyntax typeDeclaration:
                AnalyzeMemberList(context, typeDeclaration.Members);
                foreach (var nested in typeDeclaration.Members)
                    AnalyzeNestedMembers(context, nested);
                break;
        }
    }

    private static void AnalyzeMemberList(SyntaxNodeAnalysisContext context, SyntaxList<MemberDeclarationSyntax> members)
    {
        for (var i = 0; i < members.Count - 1; i++)
        {
            var current = members[i];
            var next = members[i + 1];

            if (!TryGetTerminatorToken(current, out var terminatorToken))
                continue;

            if (!terminatorToken.IsKind(SyntaxKind.SemicolonToken))
                continue;

            if (IsSameLine(terminatorToken, next.GetFirstToken()))
            {
                context.ReportDiagnostic(
                    Diagnostic.Create(
                        CompilerDiagnostics.PreferNewLineBetweenDeclarations,
                        terminatorToken.GetLocation()));
            }
        }
    }

    private static bool TryGetTerminatorToken(MemberDeclarationSyntax member, out SyntaxToken terminatorToken)
    {
        switch (member)
        {
            case BaseTypeDeclarationSyntax typeDeclaration:
                terminatorToken = typeDeclaration.TerminatorToken;
                return true;
            case DelegateDeclarationSyntax delegateDeclaration:
                terminatorToken = delegateDeclaration.TerminatorToken;
                return true;
            case NamespaceDeclarationSyntax namespaceDeclaration:
                terminatorToken = namespaceDeclaration.TerminatorToken;
                return true;
            case FileScopedNamespaceDeclarationSyntax fileScopedNamespaceDeclaration:
                terminatorToken = fileScopedNamespaceDeclaration.TerminatorToken;
                return true;
            default:
                terminatorToken = default;
                return false;
        }
    }

    private static bool IsSameLine(SyntaxToken left, SyntaxToken right)
    {
        var leftLine = left.GetLocation().GetLineSpan().EndLinePosition.Line;
        var rightLine = right.GetLocation().GetLineSpan().StartLinePosition.Line;
        return leftLine == rightLine;
    }
}
