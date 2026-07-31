using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

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
        messageFormat: "Expression result is not used; assign it to '_' to discard it explicitly.",
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

        var isTrailingValueInUnitCallable =
            IsTrailingValueInUnitCallable(expressionStatement, context.SemanticModel);
        var isFullReturnedValueHandling =
            context.Compilation.Options.ReturnedValueHandlingMode == ReturnedValueHandlingMode.Full;

        if (!isTrailingValueInUnitCallable &&
            !IsValueFormingExpression(operation) &&
            !(isFullReturnedValueHandling && IsUnhandledMemberResult(operation)))
        {
            return;
        }

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, operation.Syntax.GetLocation()));
    }

    private static bool IsValueFormingExpression(IOperation operation)
    {
        operation = Unwrap(operation);

        return operation switch
        {
            ILiteralOperation => true,
            ILocalReferenceOperation => true,
            IVariableReferenceOperation => true,
            IParameterReferenceOperation => true,
            IUnaryOperation => true,
            IBinaryOperation => true,
            ITupleOperation => true,
            _ => false,
        };
    }

    private static bool IsUnhandledMemberResult(IOperation operation)
    {
        operation = Unwrap(operation);

        return operation switch
        {
            IInvocationOperation => true,
            IPropertyReferenceOperation => true,
            IFieldReferenceOperation => true,
            IMemberReferenceOperation => true,
            IConditionalAccessOperation { WhenNotNull: { } whenNotNull } =>
                IsUnhandledMemberResult(whenNotNull),
            IAwaitOperation { Operation: { } awaitedOperation } =>
                IsUnhandledMemberResult(awaitedOperation),
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
        SyntaxNode? blockNode = expressionStatement.Parent;
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

        if (blockNode is BlockSyntax branchBlock &&
            IsBranchValueConsumed(branchBlock))
        {
            return true;
        }

        return GetCallableOwner(blockNode) switch
        {
            BaseMethodDeclarationSyntax method => ReturnsValue(semanticModel.GetDeclaredSymbol(method) as IMethodSymbol),
            FunctionStatementSyntax function => ReturnsValue(semanticModel.GetDeclaredSymbol(function) as IMethodSymbol),
            AccessorDeclarationSyntax accessor => AccessorReturnsValue(accessor, semanticModel),
            FunctionExpressionSyntax functionExpression => LambdaReturnsValue(functionExpression, semanticModel),
            _ => false,
        };
    }

    private static bool IsTrailingValueInUnitCallable(
        ExpressionStatementSyntax expressionStatement,
        SemanticModel semanticModel)
    {
        if (!TryGetTrailingBlock(expressionStatement, out var blockNode))
            return false;

        ITypeSymbol? returnType = GetCallableOwner(blockNode) switch
        {
            BaseMethodDeclarationSyntax method =>
                (semanticModel.GetDeclaredSymbol(method) as IMethodSymbol)?.ReturnType,
            FunctionStatementSyntax function =>
                (semanticModel.GetDeclaredSymbol(function) as IMethodSymbol)?.ReturnType,
            AccessorDeclarationSyntax accessor =>
                GetAccessorReturnType(accessor, semanticModel),
            FunctionExpressionSyntax functionExpression =>
                GetLambdaReturnType(functionExpression, semanticModel),
            _ => null,
        };

        return returnType?.SpecialType is SpecialType.System_Unit or SpecialType.System_Void;
    }

    private static SyntaxNode? GetCallableOwner(SyntaxNode blockNode)
        => blockNode.Parent is ArrowExpressionClauseSyntax arrowExpression
            ? arrowExpression.Parent
            : blockNode.Parent;

    private static bool TryGetTrailingBlock(
        ExpressionStatementSyntax expressionStatement,
        [NotNullWhen(true)] out SyntaxNode? blockNode)
    {
        blockNode = expressionStatement.Parent;
        var statements = blockNode switch
        {
            BlockStatementSyntax blockStatement => blockStatement.Statements,
            BlockSyntax blockExpression => blockExpression.Statements,
            _ => default,
        };

        if (statements.Count == 0)
            return false;

        var trailingStatement = statements[^1];
        return trailingStatement.SyntaxTree == expressionStatement.SyntaxTree &&
               trailingStatement.Span == expressionStatement.Span;
    }

    private static bool IsBranchValueConsumed(BlockSyntax blockExpression)
    {
        SyntaxNode current = blockExpression;

        while (true)
        {
            switch (current.Parent)
            {
                case IfExpressionSyntax ifExpression
                    when ifExpression.Expression.SyntaxTree == current.SyntaxTree &&
                         ifExpression.Expression.Span == current.Span:
                    current = ifExpression;
                    continue;

                case IfPatternExpressionSyntax ifPatternExpression
                    when ifPatternExpression.Expression.SyntaxTree == current.SyntaxTree &&
                         ifPatternExpression.Expression.Span == current.Span:
                    current = ifPatternExpression;
                    continue;

                case ElseExpressionClauseSyntax { Parent: IfExpressionSyntax ifExpression } elseClause
                    when elseClause.Expression.SyntaxTree == current.SyntaxTree &&
                         elseClause.Expression.Span == current.Span:
                    current = ifExpression;
                    continue;

                case ElseExpressionClauseSyntax { Parent: IfPatternExpressionSyntax ifPatternExpression } elseClause
                    when elseClause.Expression.SyntaxTree == current.SyntaxTree &&
                         elseClause.Expression.Span == current.Span:
                    current = ifPatternExpression;
                    continue;

                case MatchArmSyntax { Parent: MatchExpressionSyntax matchExpression } matchArm
                    when matchArm.Expression.SyntaxTree == current.SyntaxTree &&
                         matchArm.Expression.Span == current.Span:
                    current = matchExpression;
                    continue;

                case MatchArmSyntax { Parent: PostfixMatchExpressionSyntax matchExpression } matchArm
                    when matchArm.Expression.SyntaxTree == current.SyntaxTree &&
                         matchArm.Expression.Span == current.Span:
                    current = matchExpression;
                    continue;

                case ParenthesizedExpressionSyntax parenthesized
                    when parenthesized.Expression.SyntaxTree == current.SyntaxTree &&
                         parenthesized.Expression.Span == current.Span:
                    current = parenthesized;
                    continue;

                case ExpressionStatementSyntax:
                    return false;

                default:
                    return current.Parent is not null
                        and not FunctionExpressionSyntax
                        and not BaseMethodDeclarationSyntax
                        and not FunctionStatementSyntax
                        and not AccessorDeclarationSyntax
                        and not ArrowExpressionClauseSyntax;
            }
        }
    }

    private static bool AccessorReturnsValue(AccessorDeclarationSyntax accessor, SemanticModel semanticModel)
    {
        if (accessor.Keyword.Text != "get")
            return false;

        return accessor.Parent?.Parent is PropertyDeclarationSyntax property &&
               semanticModel.GetDeclaredSymbol(property) is IPropertySymbol propertySymbol &&
               ReturnsValue(propertySymbol.Type);
    }

    private static ITypeSymbol? GetAccessorReturnType(
        AccessorDeclarationSyntax accessor,
        SemanticModel semanticModel)
    {
        if (accessor.Keyword.Text != "get")
            return semanticModel.Compilation.GetSpecialType(SpecialType.System_Unit);

        return accessor.Parent?.Parent is PropertyDeclarationSyntax property &&
               semanticModel.GetDeclaredSymbol(property) is IPropertySymbol propertySymbol
            ? propertySymbol.Type
            : null;
    }

    private static bool LambdaReturnsValue(FunctionExpressionSyntax functionExpression, SemanticModel semanticModel)
        => ReturnsValue(GetLambdaReturnType(functionExpression, semanticModel));

    private static ITypeSymbol? GetLambdaReturnType(
        FunctionExpressionSyntax functionExpression,
        SemanticModel semanticModel)
    {
        var typeInfo = semanticModel.GetTypeInfo(functionExpression);
        var delegateType = typeInfo.ConvertedType as INamedTypeSymbol ?? typeInfo.Type as INamedTypeSymbol;
        return delegateType?.GetDelegateInvokeMethod()?.ReturnType;
    }

    private static bool ReturnsValue(IMethodSymbol? method)
        => ReturnsValue(method?.ReturnType);

    private static bool ReturnsValue(ITypeSymbol? type)
        => type is not null &&
           type.SpecialType is not SpecialType.System_Unit and not SpecialType.System_Void &&
           type.TypeKind is not TypeKind.Error;
}
