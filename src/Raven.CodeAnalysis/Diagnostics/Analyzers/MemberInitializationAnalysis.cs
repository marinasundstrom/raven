using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

internal static class MemberInitializationAnalysis
{
    public static bool IsAssignedInInstanceInitialization(
        SemanticModel semanticModel,
        TypeDeclarationSyntax typeDeclaration,
        ISymbol member)
    {
        var assigned = new HashSet<ISymbol>(SymbolEqualityComparer.Default);

        foreach (var ctor in typeDeclaration.Members.OfType<BaseConstructorDeclarationSyntax>())
        {
            if (ctor.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword))
                continue;

            CollectAssignedMembers(semanticModel, ctor, assigned);
        }

        foreach (var initBlock in typeDeclaration.Members.OfType<InitializerBlockDeclarationSyntax>())
            CollectAssignedMembers(semanticModel, initBlock.Body, assigned);

        return assigned.Contains(member.UnderlyingSymbol);
    }

    private static void CollectAssignedMembers(
        SemanticModel semanticModel,
        BaseConstructorDeclarationSyntax ctor,
        HashSet<ISymbol> assigned)
    {
        if (ctor.Body is not null)
            CollectAssignedMembers(semanticModel, ctor.Body, assigned);

        if (ctor.ExpressionBody is not null)
            CollectAssignedMembers(semanticModel, ctor.ExpressionBody.Expression, assigned);
    }

    private static void CollectAssignedMembers(
        SemanticModel semanticModel,
        SyntaxNode root,
        HashSet<ISymbol> assigned)
    {
        foreach (var assignment in root.DescendantNodesAndSelf().OfType<AssignmentStatementSyntax>())
            TryMarkAssigned(semanticModel, assignment.Left, assigned);

        foreach (var assignment in root.DescendantNodesAndSelf().OfType<AssignmentExpressionSyntax>())
            TryMarkAssigned(semanticModel, assignment.Left, assigned);
    }

    private static void TryMarkAssigned(
        SemanticModel semanticModel,
        ExpressionOrPatternSyntax left,
        HashSet<ISymbol> assigned)
    {
        var symbol = semanticModel.GetSymbolInfo(left).Symbol?.UnderlyingSymbol;
        if (symbol is IFieldSymbol { AssociatedSymbol: IPropertySymbol property })
        {
            assigned.Add(property.UnderlyingSymbol);
            return;
        }

        if (symbol is IPropertySymbol or IFieldSymbol)
            assigned.Add(symbol);
    }
}
