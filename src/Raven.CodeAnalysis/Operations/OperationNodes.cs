using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Operations;

internal static class OperationUtilities
{
    public static ImmutableArray<IOperation>.Builder AddIfNotNull(this ImmutableArray<IOperation>.Builder builder, IOperation? operation)
    {
        if (operation is not null)
            builder.Add(operation);

        return builder;
    }

    public static ImmutableArray<IOperation> CreateChildOperations(SemanticModel semanticModel, SyntaxNode syntax)
    {
        var builder = ImmutableArray.CreateBuilder<IOperation>();

        foreach (var childSyntax in syntax.ChildNodes())
        {
            var operation = semanticModel.GetOperation(childSyntax);
            if (operation is null)
                continue;

            builder.Add(operation);
        }

        return builder.ToImmutable();
    }

    public static ImmutableArray<IOperation> CreateChildOperations(SemanticModel semanticModel, IEnumerable<SyntaxNode?> syntaxNodes)
    {
        var builder = ImmutableArray.CreateBuilder<IOperation>();

        foreach (var childSyntax in syntaxNodes)
        {
            if (childSyntax is null)
                continue;

            var operation = semanticModel.GetOperation(childSyntax);
            if (operation is null)
                continue;

            builder.Add(operation);
        }

        return builder.ToImmutable();
    }
}

internal sealed class BlockOperation : Operation, IBlockOperation
{
    private readonly ImmutableArray<ILocalSymbol> _locals;
    private ImmutableArray<IOperation>? _operations;

    internal BlockOperation(
        SemanticModel semanticModel,
        BoundNode bound,
        OperationKind kind,
        SyntaxNode syntax,
        ImmutableArray<ILocalSymbol> locals,
        ITypeSymbol? type,
        bool isImplicit)
        : base(semanticModel, kind, syntax, type, isImplicit)
    {
        _locals = locals.IsDefault ? ImmutableArray<ILocalSymbol>.Empty : locals;
    }

    public ImmutableArray<IOperation> Operations => _operations ??= OperationUtilities.CreateChildOperations(SemanticModel, Syntax);

    public ImmutableArray<ILocalSymbol> Locals => _locals;

    protected override ImmutableArray<IOperation> GetChildrenCore() => Operations;
}

internal sealed class ExpressionStatementOperation : Operation, IExpressionStatementOperation
{
    private IOperation? _operation;

    internal ExpressionStatementOperation(
        SemanticModel semanticModel,
        BoundExpressionStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ExpressionStatement, syntax, bound.Expression.Type, isImplicit)
    {
    }

    public IOperation? Operation => _operation ??= SemanticModel.GetOperation(((ExpressionStatementSyntax)Syntax).Expression);

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Operation is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Operation);
    }
}

internal sealed class FunctionOperation : Operation, IFunctionOperation
{
    internal FunctionOperation(
        SemanticModel semanticModel,
        BoundFunctionStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Function, syntax, null, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
    }
}

internal sealed class VariableDeclarationOperation : Operation, IVariableDeclarationOperation
{
    private ImmutableArray<IVariableDeclaratorOperation>? _declarators;

    internal VariableDeclarationOperation(
        SemanticModel semanticModel,
        BoundLocalDeclarationStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.LocalDeclaration, syntax, null, isImplicit)
    {
    }

    public ImmutableArray<IVariableDeclaratorOperation> Declarators => _declarators ??= OperationUtilities.CreateChildOperations(SemanticModel, ((LocalDeclarationStatementSyntax)Syntax).Declaration.Declarators).OfType<IVariableDeclaratorOperation>().ToImmutableArray();

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return ImmutableArray.CreateRange<IOperation>(Declarators);
    }
}

internal sealed class VariableDeclaratorOperation : Operation, IVariableDeclaratorOperation
{
    private readonly BoundVariableDeclarator _bound;
    private IVariableInitializerOperation? _initializerOperation;

    internal VariableDeclaratorOperation(
        SemanticModel semanticModel,
        BoundVariableDeclarator bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.VariableDeclarator, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public ILocalSymbol Symbol => _bound.Local;

    public ImmutableArray<IOperation> IgnoredArguments => ImmutableArray<IOperation>.Empty;

    public IVariableInitializerOperation? Initializer
    {
        get
        {
            if (_initializerOperation is not null)
                return _initializerOperation;

            if (_bound.Initializer is null)
                return null;

            var syntax = (VariableDeclaratorSyntax)Syntax;
            if (syntax.Initializer is null)
                return null;

            _initializerOperation = new VariableInitializerOperation(SemanticModel, _bound.Initializer, syntax.Initializer, isImplicit: _bound.Initializer.Reason != BoundExpressionReason.None);
            return _initializerOperation;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Initializer is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create<IOperation>(Initializer);
    }
}

internal sealed class VariableInitializerOperation : Operation, IVariableInitializerOperation
{
    private readonly BoundExpression _expression;
    private IOperation? _valueOperation;

    internal VariableInitializerOperation(
        SemanticModel semanticModel,
        BoundExpression expression,
        EqualsValueClauseSyntax syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.VariableDeclarator, syntax, expression.Type, isImplicit)
    {
        _expression = expression;
    }

    public IOperation? Value => _valueOperation ??= SemanticModel.GetOperation(((EqualsValueClauseSyntax)Syntax).Value);

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Value is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Value);
    }
}

internal sealed class ReturnOperation : Operation, IReturnOperation
{
    private readonly BoundReturnStatement _bound;
    private IOperation? _returnedValue;

    internal ReturnOperation(
        SemanticModel semanticModel,
        BoundReturnStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Return, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public IOperation? ReturnedValue => _returnedValue ??= ((ReturnStatementSyntax)Syntax).Expression is { } expression ? SemanticModel.GetOperation(expression) : null;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return ReturnedValue is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(ReturnedValue);
    }
}

internal sealed class YieldReturnOperation : Operation, IYieldReturnOperation
{
    private readonly BoundYieldReturnStatement _bound;
    private IOperation? _returnedValue;

    internal YieldReturnOperation(
        SemanticModel semanticModel,
        BoundYieldReturnStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.YieldReturn, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public IOperation? ReturnedValue => _returnedValue ??= ((YieldReturnStatementSyntax)Syntax).Expression is { } expression ? SemanticModel.GetOperation(expression) : null;

    public ITypeSymbol ElementType => _bound.ElementType;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return ReturnedValue is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(ReturnedValue);
    }
}

internal sealed class YieldBreakOperation : Operation, IYieldBreakOperation
{
    private readonly BoundYieldBreakStatement _bound;

    internal YieldBreakOperation(
        SemanticModel semanticModel,
        BoundYieldBreakStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.YieldBreak, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public ITypeSymbol ElementType => _bound.ElementType;

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class ThrowOperation : Operation, IThrowOperation
{
    private readonly BoundThrowStatement _bound;
    private IOperation? _exception;

    internal ThrowOperation(
        SemanticModel semanticModel,
        BoundThrowStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Throw, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public IOperation? Exception => _exception ??= SemanticModel.GetOperation(((ThrowStatementSyntax)Syntax).Expression);

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Exception is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Exception);
    }
}

internal sealed class LiteralOperation : Operation, ILiteralOperation
{
    private readonly BoundLiteralExpression _bound;

    internal LiteralOperation(
        SemanticModel semanticModel,
        BoundLiteralExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Literal, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public object Value => _bound.Value;

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class InterpolatedStringOperation : Operation, IInterpolatedStringOperation
{
    private ImmutableArray<IInterpolatedStringContentOperation>? _contents;

    internal InterpolatedStringOperation(
        SemanticModel semanticModel,
        BoundExpression bound,
        InterpolatedStringExpressionSyntax syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.InterpolatedString, syntax, bound.Type, isImplicit)
    {
    }

    public ImmutableArray<IInterpolatedStringContentOperation> Contents
        => _contents ??= OperationUtilities.CreateChildOperations(SemanticModel, ((InterpolatedStringExpressionSyntax)Syntax).Contents)
            .OfType<IInterpolatedStringContentOperation>()
            .ToImmutableArray();

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Contents.IsDefaultOrEmpty
            ? ImmutableArray<IOperation>.Empty
            : Contents.Cast<IOperation>().ToImmutableArray();
    }
}

internal sealed class InterpolatedStringTextOperation : Operation, IInterpolatedStringTextOperation
{
    internal InterpolatedStringTextOperation(
        SemanticModel semanticModel,
        InterpolatedStringTextSyntax syntax,
        bool isImplicit)
        : base(
            semanticModel,
            OperationKind.InterpolatedStringText,
            syntax,
            semanticModel.Compilation.GetSpecialType(SpecialType.System_String),
            isImplicit)
    {
    }

    public string Text => ((InterpolatedStringTextSyntax)Syntax).Token.ValueText ?? string.Empty;

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class InterpolationOperation : Operation, IInterpolationOperation
{
    private IOperation? _expression;

    internal InterpolationOperation(
        SemanticModel semanticModel,
        InterpolationSyntax syntax,
        bool isImplicit)
        : base(
            semanticModel,
            OperationKind.Interpolation,
            syntax,
            semanticModel.GetBoundNode(syntax.Expression).Type,
            isImplicit)
    {
    }

    public IOperation? Expression => _expression ??= SemanticModel.GetOperation(((InterpolationSyntax)Syntax).Expression);

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Expression is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Expression);
    }
}

internal sealed class DefaultValueOperation : Operation, IDefaultValueOperation
{
    internal DefaultValueOperation(
        SemanticModel semanticModel,
        BoundDefaultValueExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.DefaultValue, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class BreakOperation : Operation, IBreakOperation
{
    internal BreakOperation(SemanticModel semanticModel, BoundBreakStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Break, syntax, null, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class ContinueOperation : Operation, IContinueOperation
{
    internal ContinueOperation(SemanticModel semanticModel, BoundContinueStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Continue, syntax, null, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class GotoOperation : Operation, IGotoOperation
{
    private readonly BoundGotoStatement _bound;

    internal GotoOperation(SemanticModel semanticModel, BoundGotoStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Goto, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public ILabelSymbol Target => _bound.Target;

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class ConditionalGotoOperation : Operation, IConditionalGotoOperation
{
    private readonly BoundConditionalGotoStatement _bound;
    private IOperation? _condition;

    internal ConditionalGotoOperation(SemanticModel semanticModel, BoundConditionalGotoStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.ConditionalGoto, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public ILabelSymbol Target => _bound.Target;

    public bool JumpIfTrue => _bound.JumpIfTrue;

    public IOperation? Condition
    {
        get
        {
            if (_condition is not null)
                return _condition;

            var conditionSyntax = SemanticModel.GetSyntax(_bound.Condition);
            _condition = conditionSyntax is null ? null : SemanticModel.GetOperation(conditionSyntax);

            return _condition;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Condition is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Condition);
    }
}

internal sealed class LabeledOperation : Operation, ILabeledOperation
{
    private readonly BoundLabeledStatement _bound;

    internal LabeledOperation(SemanticModel semanticModel, BoundLabeledStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Labeled, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public ILabelSymbol Label => _bound.Label;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
    }
}

public abstract class SymbolReferenceOperation<TSymbol> : Operation, ISymbolReferenceOperation<TSymbol> where TSymbol : ISymbol
{
    internal SymbolReferenceOperation(
        SemanticModel semanticModel,
        OperationKind kind,
        SyntaxNode syntax,
        TSymbol symbol,
        ITypeSymbol? type,
        bool isImplicit)
        : base(semanticModel, kind, syntax, type, isImplicit)
    {
        Symbol = symbol;
    }

    public TSymbol Symbol { get; }

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class LocalReferenceOperation : SymbolReferenceOperation<ILocalSymbol>, ILocalReferenceOperation
{
    internal LocalReferenceOperation(SemanticModel semanticModel, BoundLocalAccess bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.LocalReference, syntax, bound.Local, bound.Type, isImplicit)
    {
    }

    public ILocalSymbol Local => Symbol;
}

internal sealed class VariableReferenceOperation : SymbolReferenceOperation<ILocalSymbol>, IVariableReferenceOperation
{
    internal VariableReferenceOperation(SemanticModel semanticModel, BoundVariableExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.VariableReference, syntax, bound.Variable, bound.Type, isImplicit)
    {
    }

    public ILocalSymbol Variable => Symbol;
}

internal sealed class ParameterReferenceOperation : SymbolReferenceOperation<IParameterSymbol>, IParameterReferenceOperation
{
    internal ParameterReferenceOperation(SemanticModel semanticModel, BoundParameterAccess bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.ParameterReference, syntax, bound.Parameter, bound.Type, isImplicit)
    {
    }

    public IParameterSymbol Parameter => Symbol;
}

internal sealed class FieldReferenceOperation : SymbolReferenceOperation<IFieldSymbol>, IFieldReferenceOperation
{
    internal FieldReferenceOperation(SemanticModel semanticModel, BoundFieldAccess bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.FieldReference, syntax, bound.Field, bound.Type, isImplicit)
    {
    }

    public IFieldSymbol Field => Symbol;
}

internal sealed class PropertyReferenceOperation : SymbolReferenceOperation<IPropertySymbol>, IPropertyReferenceOperation
{
    internal PropertyReferenceOperation(SemanticModel semanticModel, BoundPropertyAccess bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.PropertyReference, syntax, bound.Property, bound.Type, isImplicit)
    {
    }

    public IPropertySymbol Property => Symbol;
}

internal sealed class MethodReferenceOperation : SymbolReferenceOperation<IMethodSymbol>, IMethodReferenceOperation
{
    internal MethodReferenceOperation(SemanticModel semanticModel, BoundMethodGroupExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.MethodReference, syntax, bound.Methods.Single(), bound.Type, isImplicit)
    {
    }

    public IMethodSymbol Method => Symbol;
}

internal sealed class MemberReferenceOperation : SymbolReferenceOperation<ISymbol>, IMemberReferenceOperation
{
    internal MemberReferenceOperation(
        SemanticModel semanticModel,
        BoundMemberAccessExpression bound,
        OperationKind kind,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, kind, syntax, bound.Member, bound.Type, isImplicit)
    {
    }
}

internal sealed class UnaryOperation : Operation, IUnaryOperation
{
    private readonly BoundUnaryExpression _bound;
    private IOperation? _operand;

    internal UnaryOperation(
        SemanticModel semanticModel,
        BoundUnaryExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Unary, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    internal BoundUnaryOperator Operator => _bound.Operator;

    public IOperation? Operand => _operand ??= SemanticModel.GetOperation(Syntax.ChildNodes().OfType<ExpressionSyntax>().FirstOrDefault());

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Operand is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Operand);
    }
}

internal sealed class BinaryOperation : Operation, IBinaryOperation
{
    private readonly BoundBinaryExpression _bound;
    private IOperation? _left;
    private IOperation? _right;

    internal BinaryOperation(
        SemanticModel semanticModel,
        BoundBinaryExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Binary, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    internal BoundBinaryOperator Operator => _bound.Operator;

    public IOperation? Left => _left ??= SemanticModel.GetOperation(((BinaryExpressionSyntax)Syntax).Left);

    public IOperation? Right => _right ??= SemanticModel.GetOperation(((BinaryExpressionSyntax)Syntax).Right);

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<IOperation>()
            .AddIfNotNull(Left)
            .AddIfNotNull(Right)
            .ToImmutable();
    }
}

internal sealed class ParenthesizedOperation : Operation, IParenthesizedOperation
{
    private IOperation? _operation;

    internal ParenthesizedOperation(
        SemanticModel semanticModel,
        BoundParenthesizedExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Parenthesized, syntax, bound.Type, isImplicit)
    {
    }

    public IOperation? Operand => _operation ??= SemanticModel.GetOperation(((ParenthesizedExpressionSyntax)Syntax).Expression);

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Operand is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Operand);
    }
}

internal sealed class ConversionOperation : Operation, IConversionOperation
{
    private readonly Conversion _conversion;
    private IOperation? _operand;

    internal ConversionOperation(
        SemanticModel semanticModel,
        BoundExpression bound,
        Conversion conversion,
        SyntaxNode syntax,
        ITypeSymbol type,
        bool isImplicit)
        : base(semanticModel, OperationKind.Conversion, syntax, type, isImplicit)
    {
        _conversion = conversion;
    }

    public Conversion Conversion => _conversion;

    public IOperation? Operand
    {
        get
        {
            if (_operand is not null)
                return _operand;

            if (Syntax is CastExpressionSyntax cast)
                _operand = SemanticModel.GetOperation(cast.Expression);
            else if (Syntax is AsExpressionSyntax asExpression)
                _operand = SemanticModel.GetOperation(asExpression.Expression);

            return _operand;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Operand is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Operand);
    }
}

internal sealed class ConditionalOperation : Operation, IConditionalOperation
{
    private IOperation? _condition;
    private IOperation? _whenTrue;
    private IOperation? _whenFalse;

    internal ConditionalOperation(SemanticModel semanticModel, BoundNode bound, SyntaxNode syntax, ITypeSymbol? type, bool isImplicit)
        : base(semanticModel, OperationKind.Conditional, syntax, type, isImplicit)
    {
    }

    public IOperation? Condition
    {
        get
        {
            if (_condition is not null)
                return _condition;

            _condition = Syntax switch
            {
                IfStatementSyntax ifStatement => SemanticModel.GetOperation(ifStatement.Condition),
                IfExpressionSyntax ifExpression => SemanticModel.GetOperation(ifExpression.Condition),
                _ => null
            };

            return _condition;
        }
    }

    public IOperation? WhenTrue
    {
        get
        {
            if (_whenTrue is not null)
                return _whenTrue;

            _whenTrue = Syntax switch
            {
                IfStatementSyntax ifStatement => SemanticModel.GetOperation(ifStatement.ThenStatement),
                IfExpressionSyntax ifExpression => SemanticModel.GetOperation(ifExpression.Expression),
                _ => null
            };

            return _whenTrue;
        }
    }

    public IOperation? WhenFalse
    {
        get
        {
            if (_whenFalse is not null)
                return _whenFalse;

            _whenFalse = Syntax switch
            {
                IfStatementSyntax ifStatement => ifStatement.ElseClause is { } elseClause
                    ? SemanticModel.GetOperation(elseClause.Statement)
                    : null,
                IfExpressionSyntax ifExpression => ifExpression.ElseClause is { } elseClause
                    ? SemanticModel.GetOperation(elseClause.Expression)
                    : null,
                _ => null
            };

            return _whenFalse;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<IOperation>()
            .AddIfNotNull(Condition)
            .AddIfNotNull(WhenTrue)
            .AddIfNotNull(WhenFalse)
            .ToImmutable();
    }
}

internal sealed class ConditionalAccessOperation : Operation, IConditionalAccessOperation
{
    private readonly BoundExpression _bound;
    private IOperation? _receiver;
    private IOperation? _whenNotNull;

    internal ConditionalAccessOperation(SemanticModel semanticModel, BoundExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.ConditionalAccess, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public IOperation? Receiver
    {
        get
        {
            if (_receiver is not null)
                return _receiver;

            if (_bound is BoundConditionalAccessExpression conditionalAccess)
            {
                _receiver = CreateOperationForBoundExpression(conditionalAccess.Receiver);
            }
            else if (_bound is BoundCarrierConditionalAccessExpression carrierConditionalAccess)
            {
                _receiver = CreateOperationForBoundExpression(carrierConditionalAccess.Receiver);
            }
            else if (Syntax is ConditionalAccessExpressionSyntax conditionalSyntax)
            {
                _receiver = SemanticModel.GetOperation(conditionalSyntax.Expression);
            }
            else if (Syntax is TryExpressionSyntax trySyntax)
            {
                _receiver = SemanticModel.GetOperation(trySyntax.Expression);
            }

            return _receiver;
        }
    }

    public IOperation? WhenNotNull
    {
        get
        {
            if (_whenNotNull is not null)
                return _whenNotNull;

            if (_bound is BoundConditionalAccessExpression conditionalAccess)
            {
                _whenNotNull = CreateOperationForBoundExpression(conditionalAccess.WhenNotNull);
            }
            else if (_bound is BoundCarrierConditionalAccessExpression carrierConditionalAccess)
            {
                _whenNotNull = CreateOperationForBoundExpression(carrierConditionalAccess.WhenPresent);
            }
            else if (Syntax is ConditionalAccessExpressionSyntax conditionalSyntax)
            {
                _whenNotNull = SemanticModel.GetOperation(conditionalSyntax.WhenNotNull);
            }

            return _whenNotNull;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<IOperation>()
            .AddIfNotNull(Receiver)
            .AddIfNotNull(WhenNotNull)
            .ToImmutable();
    }

    private IOperation? CreateOperationForBoundExpression(BoundExpression expression)
    {
        var syntax = SemanticModel.GetSyntax(expression) ?? Syntax;
        if (_bound is BoundCarrierConditionalAccessExpression && expression is BoundTryExpression tryExpression && Syntax is TryExpressionSyntax trySyntax)
        {
            // Preserve the semantic nesting for `try?`: conditional access over a try-expression receiver.
            return new TryExpressionOperation(SemanticModel, tryExpression, trySyntax, isImplicit: true);
        }

        return OperationFactory.Create(SemanticModel, syntax, expression);
    }
}

internal sealed class AwaitOperation : Operation, IAwaitOperation
{
    private readonly BoundAwaitExpression _bound;
    private IOperation? _operation;

    internal AwaitOperation(
        SemanticModel semanticModel,
        BoundAwaitExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Await, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public IOperation? Operation => _operation ??= SemanticModel.GetOperation(((UnaryExpressionSyntax)Syntax).Expression);

    public IMethodSymbol GetAwaiterMethod => _bound.GetAwaiterMethod;

    public IMethodSymbol GetResultMethod => _bound.GetResultMethod;

    public IPropertySymbol IsCompletedProperty => _bound.IsCompletedProperty;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Operation is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Operation);
    }
}

internal sealed class ArgumentOperation : Operation, IArgumentOperation
{
    private IOperation? _value;
    private string? _name;

    internal ArgumentOperation(
        SemanticModel semanticModel,
        BoundExpression bound,
        ArgumentSyntax syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Argument, syntax, bound.Type, isImplicit)
    {
    }

    public IOperation? Value => _value ??= SemanticModel.GetOperation(((ArgumentSyntax)Syntax).Expression);

    public string? Name => _name ??= ((ArgumentSyntax)Syntax).NameColon is { Name: IdentifierNameSyntax identifier }
        ? identifier.Identifier.ValueText
        : null;

    public bool IsNamed => Name is not null;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Value is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Value);
    }
}

internal sealed class InvocationOperation : Operation, IInvocationOperation
{
    private readonly BoundInvocationExpression _bound;
    private ImmutableArray<IOperation>? _arguments;
    private IOperation? _instance;

    internal InvocationOperation(
        SemanticModel semanticModel,
        BoundInvocationExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Invocation, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public IMethodSymbol TargetMethod => _bound.Method;

    public IOperation? Instance => _instance ??= (Syntax as InvocationExpressionSyntax)?.Expression is { } expr ? SemanticModel.GetOperation(expr) : null;

    public ImmutableArray<IOperation> Arguments
    {
        get
        {
            if (_arguments.HasValue)
                return _arguments.Value;

            if (Syntax is InvocationExpressionSyntax invocation)
            {
                _arguments = OperationUtilities.CreateChildOperations(SemanticModel, invocation.ArgumentList.Arguments);
            }
            else
            {
                _arguments = ImmutableArray<IOperation>.Empty;
            }

            return _arguments.Value;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<IOperation>();

        if (Instance is not null)
            builder.Add(Instance);

        builder.AddRange(Arguments);
        return builder.ToImmutable();
    }
}

internal sealed class DelegateCreationOperation : Operation, IDelegateCreationOperation
{
    internal DelegateCreationOperation(SemanticModel semanticModel, BoundDelegateCreationExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.DelegateCreation, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class AddressOfOperation : Operation, IAddressOfOperation
{
    internal AddressOfOperation(SemanticModel semanticModel, BoundAddressOfExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.AddressOf, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class ElementAccessOperation : Operation, IElementAccessOperation
{
    private readonly BoundExpression _bound;
    private IOperation? _instance;
    private ImmutableArray<IOperation>? _arguments;

    internal ElementAccessOperation(SemanticModel semanticModel, BoundExpression bound, OperationKind kind, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, kind, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public IOperation? Instance
    {
        get
        {
            if (_instance is not null)
                return _instance;

            if (Syntax is ElementAccessExpressionSyntax elementAccess)
            {
                _instance = SemanticModel.GetOperation(elementAccess.Expression);
            }
            else if (_bound is BoundArrayAccessExpression arrayAccess)
            {
                var receiverSyntax = SemanticModel.GetSyntax(arrayAccess.Receiver);
                if (receiverSyntax is not null)
                    _instance = SemanticModel.GetOperation(receiverSyntax);
            }
            else if (_bound is BoundIndexerAccessExpression indexerAccess)
            {
                var receiverSyntax = SemanticModel.GetSyntax(indexerAccess.Receiver);
                if (receiverSyntax is not null)
                    _instance = SemanticModel.GetOperation(receiverSyntax);
            }

            return _instance;
        }
    }

    public ImmutableArray<IOperation> Arguments
    {
        get
        {
            if (_arguments.HasValue)
                return _arguments.Value;

            _arguments = Syntax is ElementAccessExpressionSyntax elementAccess
                ? OperationUtilities.CreateChildOperations(SemanticModel, elementAccess.ArgumentList.Arguments)
                : ImmutableArray<IOperation>.Empty;

            return _arguments.Value;
        }
    }

    public IPropertySymbol? Indexer => _bound is BoundIndexerAccessExpression indexerAccess
        ? indexerAccess.Indexer
        : null;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<IOperation>();
        builder.AddIfNotNull(Instance);
        builder.AddRange(Arguments);
        return builder.ToImmutable();
    }
}

internal sealed class IndexOperation : Operation, IIndexOperation
{
    private readonly BoundIndexExpression _bound;
    private IOperation? _value;

    internal IndexOperation(SemanticModel semanticModel, BoundIndexExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Index, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public bool IsFromEnd => _bound.IsFromEnd;

    public IOperation? Value => _value ??= SemanticModel.GetOperation(((IndexExpressionSyntax)Syntax).Expression);

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Value is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Value);
    }
}

internal sealed class RangeOperation : Operation, IRangeOperation
{
    private IOperation? _left;
    private IOperation? _right;

    internal RangeOperation(SemanticModel semanticModel, BoundRangeExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Range, syntax, bound.Type, isImplicit)
    {
    }

    public IOperation? Left => _left ??= ((RangeExpressionSyntax)Syntax).LeftExpression is { } left ? SemanticModel.GetOperation(left) : null;

    public IOperation? Right => _right ??= ((RangeExpressionSyntax)Syntax).RightExpression is { } right ? SemanticModel.GetOperation(right) : null;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<IOperation>();

        builder.AddIfNotNull(Left);
        builder.AddIfNotNull(Right);

        return builder.ToImmutable();
    }
}

internal sealed class TypeOfOperation : Operation, ITypeOfOperation
{
    internal TypeOfOperation(SemanticModel semanticModel, BoundTypeOfExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.TypeOf, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class ObjectCreationOperation : Operation, IObjectCreationOperation
{
    private readonly BoundObjectCreationExpression _bound;
    private ImmutableArray<IOperation>? _arguments;

    internal ObjectCreationOperation(
        SemanticModel semanticModel,
        BoundObjectCreationExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ObjectCreation, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public IMethodSymbol? Constructor => _bound.Constructor;

    public ImmutableArray<IOperation> Arguments => _arguments ??= Syntax is ObjectCreationExpressionSyntax creation
        ? OperationUtilities.CreateChildOperations(SemanticModel, creation.ArgumentList.Arguments)
        : ImmutableArray<IOperation>.Empty;

    protected override ImmutableArray<IOperation> GetChildrenCore() => Arguments;
}

internal sealed class AssignmentOperation : Operation, IAssignmentOperation
{
    private readonly BoundExpression _bound;
    private IOperation? _target;
    private IOperation? _value;

    internal AssignmentOperation(
        SemanticModel semanticModel,
        BoundExpression bound,
        SyntaxNode syntax,
        ITypeSymbol? type,
        bool isImplicit)
        : base(semanticModel, OperationKind.Assignment, syntax, type, isImplicit)
    {
        _bound = bound;
    }

    public IOperation? Target
    {
        get
        {
            if (_target is not null)
                return _target;

            if (Syntax is AssignmentExpressionSyntax assignment)
                _target = SemanticModel.GetOperation(assignment.Left);
            else if (Syntax is AssignmentStatementSyntax statement)
                _target = SemanticModel.GetOperation(statement.Left);

            return _target;
        }
    }

    public IOperation? Value
    {
        get
        {
            if (_value is not null)
                return _value;

            if (Syntax is AssignmentExpressionSyntax assignment)
                _value = SemanticModel.GetOperation(assignment.Right);
            else if (Syntax is AssignmentStatementSyntax statement)
                _value = SemanticModel.GetOperation(statement.Right);

            return _value;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<IOperation>()
            .AddIfNotNull(Target)
            .AddIfNotNull(Value)
            .ToImmutable();
    }
}

internal abstract class LoopOperation : Operation, ILoopOperation
{
    private IOperation? _body;

    protected LoopOperation(
        SemanticModel semanticModel,
        OperationKind kind,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, kind, syntax, null, isImplicit)
    {
    }

    protected abstract StatementSyntax BodySyntax { get; }

    public IOperation? Body => _body ??= SemanticModel.GetOperation(BodySyntax);
}

internal sealed class WhileLoopOperation : LoopOperation, IWhileLoopOperation
{
    private IOperation? _condition;

    internal WhileLoopOperation(SemanticModel semanticModel, BoundWhileStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.WhileLoop, syntax, isImplicit)
    {
    }

    public IOperation? Condition => _condition ??= SemanticModel.GetOperation(((WhileStatementSyntax)Syntax).Condition);

    protected override StatementSyntax BodySyntax => ((WhileStatementSyntax)Syntax).Statement;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<IOperation>()
            .AddIfNotNull(Condition)
            .AddIfNotNull(Body)
            .ToImmutable();
    }
}

internal sealed class ForLoopOperation : LoopOperation, IForLoopOperation
{
    private readonly BoundForStatement _bound;
    private IOperation? _collection;

    internal ForLoopOperation(SemanticModel semanticModel, BoundForStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.ForLoop, syntax, isImplicit)
    {
        _bound = bound;
    }

    public ILocalSymbol? Local => _bound.Local;

    public ITypeSymbol ElementType => _bound.Iteration.ElementType;

    public IOperation? Collection => _collection ??= SemanticModel.GetOperation(((ForStatementSyntax)Syntax).Expression);

    protected override StatementSyntax BodySyntax => ((ForStatementSyntax)Syntax).Body;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<IOperation>()
            .AddIfNotNull(Collection)
            .AddIfNotNull(Body)
            .ToImmutable();
    }
}

internal sealed class TupleOperation : Operation, ITupleOperation
{
    private ImmutableArray<IOperation>? _elements;

    internal TupleOperation(
        SemanticModel semanticModel,
        BoundTupleExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Tuple, syntax, bound.Type, isImplicit)
    {
    }

    public ImmutableArray<IOperation> Elements
    {
        get
        {
            if (_elements.HasValue)
                return _elements.Value;

            _elements = Syntax is TupleExpressionSyntax tuple
                ? OperationUtilities.CreateChildOperations(SemanticModel, tuple.Arguments)
                : ImmutableArray<IOperation>.Empty;

            return _elements.Value;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => Elements;
}

internal sealed class TryOperation : Operation, ITryOperation
{
    private readonly BoundTryStatement _bound;
    private ImmutableArray<ICatchClauseOperation>? _catches;
    private IOperation? _body;
    private IOperation? _finally;

    internal TryOperation(SemanticModel semanticModel, BoundTryStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Try, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public IOperation? Body => _body ??= SemanticModel.GetOperation(((TryStatementSyntax)Syntax).Block);

    public ImmutableArray<ICatchClauseOperation> Catches
    {
        get
        {
            if (_catches.HasValue)
                return _catches.Value;

            var catches = ((TryStatementSyntax)Syntax).CatchClauses;
            var builder = ImmutableArray.CreateBuilder<ICatchClauseOperation>();
            for (var i = 0; i < _bound.CatchClauses.Length && i < catches.Count; i++)
            {
                builder.Add(new CatchClauseOperation(SemanticModel, _bound.CatchClauses[i], catches[i], isImplicit: false));
            }

            _catches = builder.ToImmutable();

            return _catches.Value;
        }
    }

    public IOperation? Finally => _finally ??= ((TryStatementSyntax)Syntax).FinallyClause is { } finallyClause
        ? SemanticModel.GetOperation(finallyClause.Block)
        : null;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<IOperation>();
        builder.AddIfNotNull(Body);
        builder.AddRange(Catches.Cast<IOperation>());
        builder.AddIfNotNull(Finally);
        return builder.ToImmutable();
    }
}

internal sealed class CatchClauseOperation : Operation, ICatchClauseOperation
{
    private readonly BoundCatchClause _bound;
    private IOperation? _body;

    internal CatchClauseOperation(SemanticModel semanticModel, BoundCatchClause bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.CatchClause, syntax, bound.ExceptionType, isImplicit)
    {
        _bound = bound;
    }

    public ITypeSymbol ExceptionType => _bound.ExceptionType;

    public ILocalSymbol? Local => _bound.Local;

    public IOperation? Body => _body ??= SemanticModel.GetOperation(((CatchClauseSyntax)Syntax).Block);

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Body is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Body);
    }
}

internal sealed class TryExpressionOperation : Operation, ITryExpressionOperation
{
    private readonly BoundTryExpression _bound;
    private IOperation? _operation;

    internal TryExpressionOperation(SemanticModel semanticModel, BoundTryExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.TryExpression, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public IOperation? Operation => _operation ??= SemanticModel.GetOperation(((TryExpressionSyntax)Syntax).Expression);

    public ITypeSymbol ExceptionType => _bound.ExceptionType;

    public IMethodSymbol OkConstructor => _bound.OkConstructor;

    public IMethodSymbol ErrorConstructor => _bound.ErrorConstructor;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Operation is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Operation);
    }
}

internal sealed class LambdaOperation : Operation, ILambdaOperation
{
    private readonly BoundLambdaExpression _bound;
    private IOperation? _body;
    private ImmutableArray<IParameterSymbol>? _parameters;
    private ImmutableArray<ISymbol>? _capturedVariables;

    internal LambdaOperation(
        SemanticModel semanticModel,
        BoundLambdaExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Lambda, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public ImmutableArray<IParameterSymbol> Parameters => _parameters ??= _bound.Parameters.ToImmutableArray();

    public ITypeSymbol ReturnType => _bound.ReturnType;

    public IOperation? Body
    {
        get
        {
            if (_body is not null)
                return _body;

            if (Syntax is LambdaExpressionSyntax lambda)
                _body = SemanticModel.GetOperation(lambda.ExpressionBody);

            return _body;
        }
    }

    public ImmutableArray<INamedTypeSymbol> CandidateDelegates => _bound.CandidateDelegates;

    public ImmutableArray<ISymbol> CapturedVariables => _capturedVariables ??= _bound.CapturedVariables.ToImmutableArray();

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Body is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Body);
    }
}

internal sealed class SwitchOperation : Operation, ISwitchOperation
{
    private IOperation? _value;
    private ImmutableArray<IOperation>? _patterns;
    private ImmutableArray<IOperation>? _guards;
    private ImmutableArray<IOperation>? _armValues;

    internal SwitchOperation(SemanticModel semanticModel, BoundMatchExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Switch, syntax, bound.Type, isImplicit)
    {
    }

    public IOperation? Value => _value ??= SemanticModel.GetOperation(((MatchExpressionSyntax)Syntax).Expression);

    public ImmutableArray<IOperation> Patterns
    {
        get
        {
            if (_patterns.HasValue)
                return _patterns.Value;

            var builder = ImmutableArray.CreateBuilder<IOperation>();
            var matchExpression = (MatchExpressionSyntax)Syntax;
            foreach (var arm in matchExpression.Arms)
            {
                builder.AddIfNotNull(SemanticModel.GetOperation(arm.Pattern));
            }

            _patterns = builder.ToImmutable();
            return _patterns.Value;
        }
    }

    public ImmutableArray<IOperation> Guards
    {
        get
        {
            if (_guards.HasValue)
                return _guards.Value;

            var builder = ImmutableArray.CreateBuilder<IOperation>();
            var matchExpression = (MatchExpressionSyntax)Syntax;
            foreach (var arm in matchExpression.Arms)
            {
                if (arm.WhenClause is null)
                    continue;

                builder.AddIfNotNull(SemanticModel.GetOperation(arm.WhenClause.Condition));
            }

            _guards = builder.ToImmutable();
            return _guards.Value;
        }
    }

    public ImmutableArray<IOperation> ArmValues
    {
        get
        {
            if (_armValues.HasValue)
                return _armValues.Value;

            var builder = ImmutableArray.CreateBuilder<IOperation>();
            var matchExpression = (MatchExpressionSyntax)Syntax;
            foreach (var arm in matchExpression.Arms)
            {
                builder.AddIfNotNull(SemanticModel.GetOperation(arm.Expression));
            }

            _armValues = builder.ToImmutable();
            return _armValues.Value;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<IOperation>();
        builder.AddIfNotNull(Value);
        builder.AddRange(Patterns);
        builder.AddRange(Guards);
        builder.AddRange(ArmValues);
        return builder.ToImmutable();
    }
}

internal sealed class IsPatternOperation : Operation, IIsPatternOperation
{
    private IOperation? _value;
    private IOperation? _pattern;

    internal IsPatternOperation(SemanticModel semanticModel, BoundIsPatternExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.IsPattern, syntax, bound.Type, isImplicit)
    {
    }

    public IOperation? Value => _value ??= SemanticModel.GetOperation(((IsPatternExpressionSyntax)Syntax).Expression);

    public IOperation? Pattern => _pattern ??= SemanticModel.GetOperation(((IsPatternExpressionSyntax)Syntax).Pattern);

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<IOperation>();

        builder.AddIfNotNull(Value);
        builder.AddIfNotNull(Pattern);

        return builder.ToImmutable();
    }
}

internal abstract class PatternOperation : Operation, IPatternOperation
{
    protected PatternOperation(
        SemanticModel semanticModel,
        OperationKind kind,
        SyntaxNode syntax,
        ITypeSymbol? type,
        bool isImplicit)
        : base(semanticModel, kind, syntax, type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class CasePatternOperation : PatternOperation, ICasePatternOperation
{
    private readonly BoundCasePattern _bound;
    private ImmutableArray<IOperation>? _arguments;

    internal CasePatternOperation(
        SemanticModel semanticModel,
        BoundCasePattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.CasePattern, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public IDiscriminatedUnionCaseSymbol CaseSymbol => _bound.CaseSymbol;

    public IMethodSymbol TryGetMethod => _bound.TryGetMethod;

    public ImmutableArray<IOperation> Arguments
    {
        get
        {
            if (_arguments.HasValue)
                return _arguments.Value;

            var builder = ImmutableArray.CreateBuilder<IOperation>();
            foreach (var argument in _bound.Arguments)
            {
                var syntax = SemanticModel.GetSyntax(argument);
                if (syntax is null)
                    continue;

                builder.AddIfNotNull(SemanticModel.GetOperation(syntax));
            }

            _arguments = builder.ToImmutable();
            return _arguments.Value;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => Arguments;
}

internal sealed class DeclarationPatternOperation : PatternOperation, IDeclarationPatternOperation
{
    private readonly BoundDeclarationPattern _bound;
    private IOperation? _designator;

    internal DeclarationPatternOperation(
        SemanticModel semanticModel,
        BoundDeclarationPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.DeclarationPattern, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public ITypeSymbol DeclaredType => _bound.DeclaredType;

    public IOperation? Designator
    {
        get
        {
            if (_designator is not null)
                return _designator;

            var designatorSyntax = SemanticModel.GetSyntax(_bound.Designator);
            _designator = designatorSyntax is null ? null : SemanticModel.GetOperation(designatorSyntax);

            return _designator;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<IOperation>();

        builder.AddRange(OperationUtilities.CreateChildOperations(SemanticModel, Syntax));
        builder.AddIfNotNull(Designator);

        return builder.ToImmutable();
    }
}

internal sealed class ConstantPatternOperation : PatternOperation, IConstantPatternOperation
{
    private readonly BoundConstantPattern _bound;
    private IOperation? _value;

    internal ConstantPatternOperation(
        SemanticModel semanticModel,
        BoundConstantPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ConstantPattern, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public object ConstantValue => _bound.ConstantValue;

    public IOperation? Value
    {
        get
        {
            if (_value is not null)
                return _value;

            if (_bound.Expression is not null)
            {
                var syntax = SemanticModel.GetSyntax(_bound.Expression);
                if (syntax is not null)
                    _value = SemanticModel.GetOperation(syntax);
            }
            else if (Syntax is ConstantPatternSyntax constantPattern)
            {
                _value = SemanticModel.GetOperation(constantPattern.Expression);
            }

            return _value;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Value is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Value);
    }
}

internal sealed class PositionalPatternOperation : PatternOperation, IPositionalPatternOperation
{
    private ImmutableArray<IOperation>? _subpatterns;

    internal PositionalPatternOperation(
        SemanticModel semanticModel,
        BoundPositionalPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.PositionalPattern, syntax, bound.Type, isImplicit)
    {
    }

    public ImmutableArray<IOperation> Subpatterns
    {
        get
        {
            if (_subpatterns.HasValue)
                return _subpatterns.Value;

            _subpatterns = Syntax is PositionalPatternSyntax positionalPattern
                ? OperationUtilities.CreateChildOperations(SemanticModel, positionalPattern.Elements.Select(element => element.Pattern))
                : ImmutableArray<IOperation>.Empty;

            return _subpatterns.Value;
        }
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => Subpatterns;
}

internal sealed class DiscardPatternOperation : PatternOperation, IDiscardPatternOperation
{
    internal DiscardPatternOperation(
        SemanticModel semanticModel,
        BoundDiscardPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.DiscardPattern, syntax, bound.Type, isImplicit)
    {
    }
}

internal sealed class NotPatternOperation : PatternOperation, INotPatternOperation
{
    private IOperation? _pattern;

    internal NotPatternOperation(
        SemanticModel semanticModel,
        BoundNotPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.NotPattern, syntax, bound.Type, isImplicit)
    {
    }

    public IOperation? Pattern => _pattern ??= Syntax is UnaryPatternSyntax unaryPattern
        ? SemanticModel.GetOperation(unaryPattern.Pattern)
        : null;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Pattern is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Pattern);
    }
}

internal sealed class AndPatternOperation : PatternOperation, IAndPatternOperation
{
    private IOperation? _left;
    private IOperation? _right;

    internal AndPatternOperation(
        SemanticModel semanticModel,
        BoundAndPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.AndPattern, syntax, bound.Type, isImplicit)
    {
    }

    public IOperation? Left => _left ??= Syntax is BinaryPatternSyntax binaryPattern
        ? SemanticModel.GetOperation(binaryPattern.Left)
        : null;

    public IOperation? Right => _right ??= Syntax is BinaryPatternSyntax binaryPattern
        ? SemanticModel.GetOperation(binaryPattern.Right)
        : null;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<IOperation>()
            .AddIfNotNull(Left)
            .AddIfNotNull(Right)
            .ToImmutable();
    }
}

internal sealed class OrPatternOperation : PatternOperation, IOrPatternOperation
{
    private IOperation? _left;
    private IOperation? _right;

    internal OrPatternOperation(
        SemanticModel semanticModel,
        BoundOrPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.OrPattern, syntax, bound.Type, isImplicit)
    {
    }

    public IOperation? Left => _left ??= Syntax is BinaryPatternSyntax binaryPattern
        ? SemanticModel.GetOperation(binaryPattern.Left)
        : null;

    public IOperation? Right => _right ??= Syntax is BinaryPatternSyntax binaryPattern
        ? SemanticModel.GetOperation(binaryPattern.Right)
        : null;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<IOperation>()
            .AddIfNotNull(Left)
            .AddIfNotNull(Right)
            .ToImmutable();
    }
}

internal abstract class DesignatorOperation : Operation, IDesignatorOperation
{
    protected DesignatorOperation(
        SemanticModel semanticModel,
        OperationKind kind,
        SyntaxNode syntax,
        ITypeSymbol? type,
        bool isImplicit)
        : base(semanticModel, kind, syntax, type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class SingleVariableDesignatorOperation : DesignatorOperation, ISingleVariableDesignatorOperation
{
    private readonly BoundSingleVariableDesignator _bound;

    internal SingleVariableDesignatorOperation(
        SemanticModel semanticModel,
        BoundSingleVariableDesignator bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.SingleVariableDesignator, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public ILocalSymbol Local => _bound.Local;
}

internal sealed class DiscardDesignatorOperation : DesignatorOperation, IDiscardDesignatorOperation
{
    internal DiscardDesignatorOperation(
        SemanticModel semanticModel,
        BoundDiscardDesignator bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.DiscardDesignator, syntax, bound.Type, isImplicit)
    {
    }
}

internal sealed class CollectionOperation : Operation, ICollectionOperation
{
    internal CollectionOperation(SemanticModel semanticModel, BoundCollectionExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Collection, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class EmptyCollectionOperation : Operation, IEmptyCollectionOperation
{
    internal EmptyCollectionOperation(SemanticModel semanticModel, BoundEmptyCollectionExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.EmptyCollection, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class SpreadElementOperation : Operation, ISpreadElementOperation
{
    private IOperation? _expression;

    internal SpreadElementOperation(SemanticModel semanticModel, BoundSpreadElement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.SpreadElement, syntax, bound.Type, isImplicit)
    {
    }

    public IOperation? Expression => _expression ??= SemanticModel.GetOperation(((SpreadElementSyntax)Syntax).Expression);

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return Expression is null
            ? ImmutableArray<IOperation>.Empty
            : ImmutableArray.Create(Expression);
    }
}

internal sealed class TypeOperation : Operation, ITypeOperation
{
    internal TypeOperation(SemanticModel semanticModel, BoundTypeExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.TypeExpression, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class NamespaceOperation : Operation, INamespaceOperation
{
    internal NamespaceOperation(SemanticModel semanticModel, BoundNamespaceExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.NamespaceExpression, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class SelfOperation : Operation, ISelfOperation
{
    internal SelfOperation(SemanticModel semanticModel, BoundSelfExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.SelfReference, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class UnitOperation : Operation, IUnitOperation
{
    internal UnitOperation(
        SemanticModel semanticModel,
        BoundUnitExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Unit, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class InvalidOperation : Operation, IInvalidOperation
{
    internal InvalidOperation(
        SemanticModel semanticModel,
        BoundErrorExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Invalid, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
    }
}
