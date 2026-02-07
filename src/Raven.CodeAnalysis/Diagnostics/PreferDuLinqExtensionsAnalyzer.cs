using System;
using System.Collections.Frozen;
using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class PreferDuLinqExtensionsAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9014";

    private static readonly FrozenDictionary<string, string> PreferredAlternatives =
        new Dictionary<string, string>(StringComparer.Ordinal)
        {
            ["First"] = "FirstOrError",
            ["FirstOrDefault"] = "FirstOrNone",
            ["Single"] = "SingleOrError",
            ["SingleOrDefault"] = "SingleOrNone",
            ["Last"] = "LastOrError",
            ["LastOrDefault"] = "LastOrNone",
            ["ElementAt"] = "ElementAtOrError",
            ["ElementAtOrDefault"] = "ElementAtOrNone",
        }.ToFrozenDictionary(StringComparer.Ordinal);

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Prefer DU LINQ extensions",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Prefer '{1}' over '{0}'",
        category: "ErrorHandling",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
        => context.RegisterSyntaxNodeAction(AnalyzeInvocation, SyntaxKind.InvocationExpression);

    private static void AnalyzeInvocation(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not InvocationExpressionSyntax invocation)
            return;

        // Conditional receiver binding paths can throw in symbol resolution and are
        // outside the scope of this preference rule.
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

        if (!IsLinqMethod(method))
            return;

        if (!PreferredAlternatives.TryGetValue(method.Name, out var preferredName))
            return;

        var location = invocation.Expression switch
        {
            MemberAccessExpressionSyntax memberAccess => memberAccess.Name.GetLocation(),
            _ => invocation.Expression.GetLocation()
        };

        var diagnostic = Diagnostic.Create(Descriptor, location, method.Name, preferredName);
        context.ReportDiagnostic(diagnostic);
    }

    private static bool IsLinqMethod(IMethodSymbol method)
    {
        var ns = method.ContainingNamespace;
        if (ns is null || !string.Equals(ns.ToMetadataName(), "System.Linq", StringComparison.Ordinal))
            return false;

        var containingType = method.ContainingType;
        if (containingType is null)
            return false;

        return containingType.Name is "Enumerable" or "Queryable" or "ParallelEnumerable";
    }
}
