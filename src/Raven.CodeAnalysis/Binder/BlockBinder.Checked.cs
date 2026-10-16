using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    // Lexical syntax supplies context without mutable binder state or cache keys.
    // An enclosing context never changes the body of a nested function.
    private static bool IsCheckedContext(SyntaxNode? syntax, bool defaultValue = false)
    {
        for (var current = syntax; current is not null; current = current.Parent)
        {
            if (current is CheckedExpressionSyntax context)
                return context.Keyword.Kind == SyntaxKind.CheckedKeyword;
            if (current is CheckedStatementSyntax statement)
                return statement.Keyword.Kind == SyntaxKind.CheckedKeyword;
            if (current is FunctionExpressionSyntax or FunctionStatementSyntax or MemberDeclarationSyntax)
                return defaultValue;
        }
        return defaultValue;
    }

    private BoundExpression BindCheckedExpression(CheckedExpressionSyntax syntax)
    {
        var targetType = GetScopedTargetType(syntax);
        var expression = targetType is null
            ? BindExpression(syntax.Expression)
            : BindExpressionWithTargetType(syntax.Expression, targetType);
        return expression is BoundErrorExpression ? expression : new BoundParenthesizedExpression(expression);
    }

    private static BoundBinaryOperator WithCheckedContext(BoundBinaryOperator op, SyntaxNode? syntax)
        => IsCheckedContext(syntax) &&
           op.OperatorKind is BinaryOperatorKind.Addition or BinaryOperatorKind.Subtraction or BinaryOperatorKind.Multiplication &&
           op.LeftType.SpecialType is SpecialType.System_Int32 or SpecialType.System_UInt32 or
               SpecialType.System_Int64 or SpecialType.System_UInt64 or SpecialType.System_IntPtr or SpecialType.System_UIntPtr
            ? op.WithChecked()
            : op;
    internal bool TryEvaluateConstant(BoundExpression expression, SyntaxNode syntax, out object? value)
    {
        var status = BoundNumericConstantEvaluator.Evaluate(expression,
            node => IsCheckedContext(SemanticModel.GetSyntax(node) ?? syntax, defaultValue: true), out value);
        return status == BoundNumericConstantEvaluator.Status.Constant ||
            status == BoundNumericConstantEvaluator.Status.NotConstant && syntax is ExpressionSyntax expressionSyntax &&
            ConstantValueEvaluator.TryEvaluate(expressionSyntax, out value);
    }

    private BoundExpression CheckConstantOverflow(BoundExpression expression, SyntaxNode syntax)
    {
        if (expression is not (BoundBinaryExpression or BoundUnaryExpression or BoundConversionExpression))
            return expression;
        var status = BoundNumericConstantEvaluator.Evaluate(expression,
            node => IsCheckedContext(SemanticModel.GetSyntax(node) ?? syntax, defaultValue: true), out _);
        if (status == BoundNumericConstantEvaluator.Status.DivisionByZero)
        {
            _diagnostics.ReportConstantDivisionByZero(syntax.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
        }
        if (status != BoundNumericConstantEvaluator.Status.Overflow)
            return expression;
        _diagnostics.ReportConstantArithmeticOverflow(syntax.GetLocation());
        return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
    }
}
