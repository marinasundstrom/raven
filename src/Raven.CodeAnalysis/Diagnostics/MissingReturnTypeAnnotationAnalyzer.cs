using System.Collections.Generic;
using System.Linq;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports methods without explicit return type annotations and suggests the inferred type.
/// </summary>
public sealed class MissingReturnTypeAnnotationAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9001";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Return type annotation missing",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Method '{0}' has no return type annotation; consider '{1}'",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxTreeAction(AnalyzeTree);
    }

    private static void AnalyzeTree(SyntaxTreeAnalysisContext context)
    {
        var semanticModel = context.Compilation.GetSemanticModel(context.SyntaxTree);
        var root = context.SyntaxTree.GetRoot();

        void AnalyzeNode(SyntaxNode node, SyntaxToken identifier, SyntaxNode? returnType, SyntaxNode? body)
        {
            if (returnType is not null || body is null)
                return;

            var symbol = semanticModel.GetDeclaredSymbol(node) as IMethodSymbol;
            if (symbol is null)
                return;

            var boundBody = semanticModel.GetBoundNode(body);
            var inferred = ReturnTypeCollector.Infer(boundBody);

            if (inferred is null || inferred.SpecialType == SpecialType.System_Unit)
                return;

            if (inferred is IUnionTypeSymbol union && union.Types.All(t => t.SpecialType != SpecialType.System_Unit))
            {
                var commonBase = FindCommonBase(union.Types);
                if (commonBase is not null &&
                    commonBase.SpecialType is not SpecialType.System_Object and not SpecialType.System_ValueType)
                {
                    inferred = commonBase;
                }
            }

            var typeDisplay = FormatType(inferred);
            var location = identifier.GetLocation();
            var diagnostic = Diagnostic.Create(Descriptor, location, symbol.Name, typeDisplay);
            context.ReportDiagnostic(diagnostic);
        }

        foreach (var method in root.DescendantNodes().OfType<MethodDeclarationSyntax>())
            AnalyzeNode(method, method.Identifier, method.ReturnType, method.Body);

        foreach (var function in root.DescendantNodes().OfType<FunctionStatementSyntax>())
            AnalyzeNode(function, function.Identifier, function.ReturnType, function.Body);
    }

    private static string FormatType(ITypeSymbol type)
    {
        if (type is IUnionTypeSymbol union)
        {
            var parts = union.Types
                .Select(t => t.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat))
                .OrderBy(x => x);
            return string.Join(" | ", parts);
        }

        return type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
    }

    private static ITypeSymbol? FindCommonBase(IEnumerable<ITypeSymbol> types)
    {
        HashSet<INamedTypeSymbol>? intersection = null;

        foreach (var type in types)
        {
            if (type is not INamedTypeSymbol named)
                return null;

            var bases = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
            for (INamedTypeSymbol? current = named; current is not null; current = current.BaseType)
                bases.Add(current);

            if (intersection is null)
                intersection = bases;
            else
                intersection.IntersectWith(bases);

            if (intersection.Count == 0)
                return null;
        }

        return intersection?.FirstOrDefault();
    }
}
