using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnnecessaryTrailingSeparatorAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9028";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Unnecessary trailing separator",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Trailing separator '{0}' is unnecessary here.",
        category: "Style",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(
            AnalyzeNode,
            SyntaxKind.AttributeList,
            SyntaxKind.ParameterList,
            SyntaxKind.BracketedParameterList,
            SyntaxKind.TypeParameterList,
            SyntaxKind.TypeArgumentList,
            SyntaxKind.ArgumentList,
            SyntaxKind.BracketedArgumentList,
            SyntaxKind.TupleExpression,
            SyntaxKind.ArrayExpression,
            SyntaxKind.PropertyPatternClause,
            SyntaxKind.ParenthesizedVariableDesignation,
            SyntaxKind.FunctionTypeParameterList,
            SyntaxKind.PositionalPattern,
            SyntaxKind.NominalDeconstructionPatternArgumentList,
            SyntaxKind.SequencePattern,
            SyntaxKind.DictionaryPattern);
    }

    private static void AnalyzeNode(SyntaxNodeAnalysisContext context)
    {
        switch (context.Node)
        {
            case AttributeListSyntax attributeList:
                AnalyzeTrailingComma(context, attributeList.Attributes, attributeList.CloseBracketToken);
                break;
            case ParameterListSyntax parameterList:
                AnalyzeTrailingComma(context, parameterList.Parameters, parameterList.CloseParenToken);
                break;
            case BracketedParameterListSyntax parameterList:
                AnalyzeTrailingComma(context, parameterList.Parameters, parameterList.CloseBracketToken);
                break;
            case TypeParameterListSyntax typeParameterList:
                AnalyzeTrailingComma(context, typeParameterList.Parameters, typeParameterList.GreaterThanToken);
                break;
            case TypeArgumentListSyntax typeArgumentList:
                AnalyzeTrailingComma(context, typeArgumentList.Arguments, typeArgumentList.GreaterThanToken);
                break;
            case ArgumentListSyntax argumentList:
                AnalyzeTrailingComma(context, argumentList.Arguments, argumentList.CloseParenToken);
                break;
            case BracketedArgumentListSyntax argumentList:
                AnalyzeTrailingComma(context, argumentList.Arguments, argumentList.CloseBracketToken);
                break;
            case TupleExpressionSyntax tupleExpression:
                AnalyzeTrailingComma(context, tupleExpression.Arguments, tupleExpression.CloseParenToken);
                break;
            case ArrayExpressionSyntax arrayExpression:
                AnalyzeTrailingComma(context, arrayExpression.Elements, arrayExpression.CloseArrayToken);
                break;
            case PropertyPatternClauseSyntax propertyPatternClause:
                AnalyzeTrailingComma(context, propertyPatternClause.Properties, propertyPatternClause.CloseBraceToken);
                break;
            case ParenthesizedVariableDesignationSyntax designation:
                AnalyzeTrailingComma(context, designation.Variables, designation.CloseParenToken);
                break;
            case FunctionTypeParameterListSyntax functionTypeParameterList:
                AnalyzeTrailingComma(context, functionTypeParameterList.Parameters, functionTypeParameterList.CloseParenToken);
                break;
            case PositionalPatternSyntax positionalPattern:
                AnalyzeTrailingComma(context, positionalPattern.Elements, positionalPattern.CloseParenToken);
                break;
            case NominalDeconstructionPatternArgumentListSyntax argumentList:
                AnalyzeTrailingComma(context, argumentList.Arguments, argumentList.CloseParenToken);
                break;
            case SequencePatternSyntax sequencePattern:
                AnalyzeTrailingComma(context, sequencePattern.Elements, sequencePattern.CloseBracketToken);
                break;
            case DictionaryPatternSyntax dictionaryPattern:
                AnalyzeTrailingComma(context, dictionaryPattern.Entries, dictionaryPattern.CloseBracketToken);
                break;
        }
    }

    private static void AnalyzeTrailingComma<TNode>(
        SyntaxNodeAnalysisContext context,
        SeparatedSyntaxList<TNode> list,
        SyntaxToken closingToken)
        where TNode : SyntaxNode
    {
        if (closingToken.IsMissing || list.SeparatorCount == 0)
            return;

        var trailingSeparator = GetTrailingSeparator(list);
        if (trailingSeparator.Kind != SyntaxKind.CommaToken || trailingSeparator.IsMissing)
            return;

        context.ReportDiagnostic(new Diagnostic(
            Descriptor,
            trailingSeparator.GetLocation(),
            [trailingSeparator.Text]));
    }

    private static SyntaxToken GetTrailingSeparator<TNode>(SeparatedSyntaxList<TNode> list)
        where TNode : SyntaxNode
    {
        var items = list.GetWithSeparators().ToArray();
        if (items.Length == 0)
            return default;

        var last = items[^1];
        return last.IsToken ? last.AsToken() : default;
    }
}
