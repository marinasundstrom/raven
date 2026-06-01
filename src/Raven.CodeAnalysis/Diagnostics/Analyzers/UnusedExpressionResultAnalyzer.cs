using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnusedExpressionResultAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9034";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Expression result is not used",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Expression result is not used.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => [Descriptor];

    public override void Initialize(AnalysisContext context)
        => context.RegisterOperationAction(AnalyzeExpressionStatement, OperationKind.ExpressionStatement);

    private static void AnalyzeExpressionStatement(OperationAnalysisContext context)
    {
        if (context.Operation is not IExpressionStatementOperation
            {
                Syntax: ExpressionStatementSyntax expressionStatement,
                Operation: { } operation
            })
        {
            return;
        }

        if (IsImplicitValueReturnTarget(expressionStatement, context.SemanticModel))
            return;

        if (!ReturnsValue(operation.Type))
            return;

        if (!IsPureValueComposition(operation))
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, operation.Syntax.GetLocation()));
    }

    private static bool IsPureValueComposition(IOperation operation)
    {
        operation = Unwrap(operation);

        return operation switch
        {
            ILiteralOperation => true,
            ILocalReferenceOperation => true,
            IVariableReferenceOperation => true,
            IParameterReferenceOperation => true,
            IUnaryOperation unary => unary.Operand is { } operand && IsPureValueComposition(operand),
            IBinaryOperation binary => binary.Left is { } left &&
                                       binary.Right is { } right &&
                                       IsPureValueComposition(left) &&
                                       IsPureValueComposition(right),
            ITupleOperation tuple => tuple.Elements.All(IsPureValueComposition),
            _ => false,
        };
    }

    private static IOperation Unwrap(IOperation operation)
    {
        while (true)
        {
            switch (operation)
            {
                case IParenthesizedOperation { Operand: { } operand }:
                    operation = operand;
                    continue;

                case IConversionOperation { Operand: { } operand }:
                    operation = operand;
                    continue;

                default:
                    return operation;
            }
        }
    }

    private static bool IsImplicitValueReturnTarget(ExpressionStatementSyntax expressionStatement, SemanticModel semanticModel)
    {
        SyntaxNode blockNode = expressionStatement.Parent;
        SyntaxList<StatementSyntax> statements;

        switch (blockNode)
        {
            case BlockStatementSyntax blockStatement:
                statements = blockStatement.Statements;
                break;
            case BlockSyntax blockExpression:
                statements = blockExpression.Statements;
                break;
            default:
                return false;
        }

        if (statements.Count == 0)
            return false;

        var trailingStatement = statements[^1];
        if (trailingStatement.SyntaxTree != expressionStatement.SyntaxTree ||
            trailingStatement.Span != expressionStatement.Span)
        {
            return false;
        }

        return blockNode.Parent switch
        {
            BaseMethodDeclarationSyntax method => ReturnsValue(semanticModel.GetDeclaredSymbol(method) as IMethodSymbol),
            FunctionStatementSyntax function => ReturnsValue(semanticModel.GetDeclaredSymbol(function) as IMethodSymbol),
            AccessorDeclarationSyntax accessor => AccessorReturnsValue(accessor, semanticModel),
            FunctionExpressionSyntax functionExpression => LambdaReturnsValue(functionExpression, semanticModel),
            _ => false,
        };
    }

    private static bool AccessorReturnsValue(AccessorDeclarationSyntax accessor, SemanticModel semanticModel)
    {
        if (accessor.Keyword.Text != "get")
            return false;

        return accessor.Parent?.Parent is PropertyDeclarationSyntax property &&
               semanticModel.GetDeclaredSymbol(property) is IPropertySymbol propertySymbol &&
               ReturnsValue(propertySymbol.Type);
    }

    private static bool LambdaReturnsValue(FunctionExpressionSyntax functionExpression, SemanticModel semanticModel)
    {
        var typeInfo = semanticModel.GetTypeInfo(functionExpression);
        var delegateType = typeInfo.ConvertedType as INamedTypeSymbol ?? typeInfo.Type as INamedTypeSymbol;
        return ReturnsValue(delegateType?.GetDelegateInvokeMethod()?.ReturnType);
    }

    private static bool ReturnsValue(IMethodSymbol? method)
        => ReturnsValue(method?.ReturnType);

    private static bool ReturnsValue(ITypeSymbol? type)
        => type is not null &&
           type.SpecialType is not SpecialType.System_Unit and not SpecialType.System_Void &&
           type.TypeKind is not TypeKind.Error;
}
