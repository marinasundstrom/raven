using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports methods without explicit return type annotations and suggests the inferred type.
/// </summary>
/// <remarks>
/// Making return types explicit helps teams balance the expressiveness of functional-style
/// inference with the clarity and intent often emphasized in object-oriented code.
/// </remarks>
public sealed class MissingReturnTypeAnnotationAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9001";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Return type annotation missing",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Add a return type to '{0}': '{1}'.",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(
            AnalyzeNode,
            SyntaxKind.MethodDeclaration,
            SyntaxKind.FunctionStatement);
    }

    private static void AnalyzeNode(SyntaxNodeAnalysisContext context)
    {
        switch (context.Node)
        {
            case MethodDeclarationSyntax method:
                AnalyzeDeclaration(
                    context,
                    method,
                    method.Identifier,
                    method.ReturnType,
                    method.Body ?? (SyntaxNode?)method.ExpressionBody);
                break;

            case FunctionStatementSyntax function:
                AnalyzeDeclaration(
                    context,
                    function,
                    function.Identifier,
                    function.ReturnType,
                    function.Body ?? (SyntaxNode?)function.ExpressionBody);
                break;
        }
    }

    private static void AnalyzeDeclaration(
        SyntaxNodeAnalysisContext context,
        SyntaxNode node,
        SyntaxToken identifier,
        SyntaxNode? returnType,
        SyntaxNode? body)
    {
        if (returnType is not null || body is null)
            return;

        var symbol = context.SemanticModel.GetDeclaredSymbol(node) as IMethodSymbol;
        if (symbol is null)
            return;

        var boundBody = context.SemanticModel.GetBoundNode(body);
        var inferred = ReturnTypeCollector.Infer(boundBody);

        if (inferred is null ||
            inferred.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
            return;

        if (inferred is ErrorTypeSymbol)
            return;

        if (inferred is ITypeUnionSymbol union)
            inferred = InferBestEffortType(context.SemanticModel.Compilation, union);

        var typeDisplay = FormatType(inferred);
        var location = identifier.GetLocation();
        var diagnostic = Diagnostic.Create(Descriptor, location, symbol.Name, typeDisplay);
        context.ReportDiagnostic(diagnostic);
    }

    private static string FormatType(ITypeSymbol type)
    {
        if (type is ITypeUnionSymbol union)
        {
            var parts = union.Types
                .Select(t => t.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat))
                .OrderBy(x => x);
            return string.Join(" | ", parts);
        }

        return type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
    }

    private static ITypeSymbol InferBestEffortType(Compilation compilation, ITypeUnionSymbol union)
    {
        var members = union.Types
            .Where(t => t.SpecialType is not (SpecialType.System_Unit or SpecialType.System_Void))
            .Distinct<ITypeSymbol>(SymbolEqualityComparer.Default)
            .ToArray();

        if (members.Length <= 1)
            return union;

        var memberCandidate = members
            .Select(candidate => new CandidateScore(candidate, ScoreImplicitTarget(compilation, members, candidate)))
            .Where(candidate => candidate.Score >= 0 && !IsBroadTopType(candidate.Type))
            .OrderBy(candidate => candidate.Score)
            .ThenBy(candidate => candidate.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat), StringComparer.Ordinal)
            .FirstOrDefault();

        if (memberCandidate is not null)
            return memberCandidate.Type;

        var commonDenominator = TypeSymbolExtensionsForCodeGen.FindCommonDenominator(members);
        if (commonDenominator is not null &&
            !IsBroadTopType(commonDenominator) &&
            ScoreImplicitTarget(compilation, members, commonDenominator) >= 0)
        {
            return commonDenominator;
        }

        return union;
    }

    private static int ScoreImplicitTarget(Compilation compilation, ITypeSymbol[] sources, ITypeSymbol target)
    {
        var score = 0;
        foreach (var source in sources)
        {
            var conversion = compilation.ClassifyConversion(source, target);
            if (!conversion.Exists || !conversion.IsImplicit)
                return -1;

            if (!conversion.IsIdentity)
                score++;
        }

        return score;
    }

    private static bool IsBroadTopType(ITypeSymbol type)
    {
        return type.SpecialType is SpecialType.System_Object or SpecialType.System_ValueType;
    }

    private sealed class CandidateScore
    {
        public CandidateScore(ITypeSymbol type, int score)
        {
            Type = type;
            Score = score;
        }

        public ITypeSymbol Type { get; }

        public int Score { get; }
    }
}
