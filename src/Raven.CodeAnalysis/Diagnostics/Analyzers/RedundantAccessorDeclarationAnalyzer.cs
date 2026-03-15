using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class RedundantAccessorDeclarationAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9020";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Remove redundant accessor declarations",
        description: null,
        helpLinkUri: string.Empty,
        category: "Style",
        messageFormat: "Accessor list is redundant for '{0}'.",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(AnalyzePropertyDeclaration, SyntaxKind.PropertyDeclaration);
        context.RegisterSyntaxNodeAction(AnalyzeIndexerDeclaration, SyntaxKind.IndexerDeclaration);
    }

    private static void AnalyzePropertyDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not PropertyDeclarationSyntax property)
            return;

        if (!TryGetRedundantAccessorList(
                property.BindingKeyword.Kind,
                property.AccessorList,
                out var accessorList))
        {
            return;
        }

        var diagnostic = Diagnostic.Create(
            Descriptor,
            accessorList.GetLocation(),
            property.Identifier.ValueText);

        context.ReportDiagnostic(diagnostic);
    }

    private static void AnalyzeIndexerDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not IndexerDeclarationSyntax indexer)
            return;

        if (!TryGetRedundantAccessorList(
                indexer.BindingKeyword.Kind,
                indexer.AccessorList,
                out var accessorList))
        {
            return;
        }

        var diagnostic = Diagnostic.Create(
            Descriptor,
            accessorList.GetLocation(),
            "indexer");

        context.ReportDiagnostic(diagnostic);
    }

    internal static bool TryGetRedundantAccessorList(
        SyntaxKind bindingKeywordKind,
        AccessorListSyntax? accessorList,
        out AccessorListSyntax redundantAccessorList)
    {
        redundantAccessorList = null!;
        if (accessorList is null ||
            accessorList.IsMissing ||
            accessorList.OpenBraceToken.IsMissing ||
            accessorList.CloseBraceToken.IsMissing)
        {
            return false;
        }

        var accessors = new List<AccessorDeclarationSyntax>();
        foreach (var accessor in accessorList.Accessors)
        {
            if (accessor.IsMissing || accessor.Keyword.IsMissing)
                return false;

            if (accessor.AttributeLists.Count > 0 || accessor.Modifiers.Count > 0)
                return false;

            if (accessor.Body is not null || accessor.ExpressionBody is not null)
                return false;

            accessors.Add(accessor);
        }

        if (accessors.Count == 0)
            return false;

        var isValDefault =
            bindingKeywordKind == SyntaxKind.ValKeyword &&
            accessors.Count == 1 &&
            accessors[0].Kind == SyntaxKind.GetAccessorDeclaration;

        var isVarDefault =
            bindingKeywordKind == SyntaxKind.VarKeyword &&
            accessors.Count == 2 &&
            accessors.Any(a => a.Kind == SyntaxKind.GetAccessorDeclaration) &&
            accessors.Any(a => a.Kind == SyntaxKind.SetAccessorDeclaration);

        if (!isValDefault && !isVarDefault)
            return false;

        redundantAccessorList = accessorList;
        return true;
    }
}
