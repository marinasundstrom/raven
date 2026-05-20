using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnhandledMemberReturnValueAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9029";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Member return value is ignored",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Return value of member '{0}' is ignored. Assign it to a target or explicitly discard it.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
        => context.RegisterSyntaxNodeAction(AnalyzeExpressionStatement, SyntaxKind.ExpressionStatement);

    private static void AnalyzeExpressionStatement(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not ExpressionStatementSyntax expressionStatement)
            return;

        if (IsImplicitValueReturnTarget(expressionStatement, context.SemanticModel))
            return;

        if (context.SemanticModel.GetOperation(expressionStatement) is not IExpressionStatementOperation statement ||
            statement.Operation is null)
        {
            return;
        }

        if (!TryGetUnhandledMemberReturnValue(statement.Operation, out var memberName, out var resultType))
            return;

        if (!ReturnsValue(resultType))
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, statement.Operation.Syntax.GetLocation(), memberName));
    }

    private static bool TryGetUnhandledMemberReturnValue(IOperation operation, out string memberName, out ITypeSymbol? resultType)
    {
        memberName = string.Empty;
        resultType = null;

        operation = Unwrap(operation);

        switch (operation)
        {
            case IInvocationOperation invocation:
                memberName = invocation.TargetMethod.Name;
                resultType = invocation.TargetMethod.ReturnType;
                return true;

            case IPropertyReferenceOperation propertyReference:
                memberName = propertyReference.Property.Name;
                resultType = propertyReference.Property.Type;
                return true;

            case IFieldReferenceOperation fieldReference:
                memberName = fieldReference.Field.Name;
                resultType = fieldReference.Field.Type;
                return true;

            case IMemberReferenceOperation memberReference:
                memberName = memberReference.Symbol.Name;
                resultType = operation.Type;
                return true;

            case IConditionalAccessOperation conditionalAccess:
                return conditionalAccess.WhenNotNull is not null &&
                       TryGetUnhandledMemberReturnValue(conditionalAccess.WhenNotNull, out memberName, out resultType);

            case IAwaitOperation awaitOperation:
                if (awaitOperation.Operation is not null &&
                    TryGetUnhandledMemberReturnValue(awaitOperation.Operation, out memberName, out _))
                {
                    resultType = awaitOperation.Type;
                    return true;
                }

                break;
        }

        return false;
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
