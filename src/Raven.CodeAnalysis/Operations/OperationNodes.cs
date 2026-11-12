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
    public static ImmutableArray<Operation>.Builder AddIfNotNull(this ImmutableArray<Operation>.Builder builder, Operation? operation)
    {
        if (operation is not null)
            builder.Add(operation);

        return builder;
    }

    public static ImmutableArray<Operation> CreateChildOperations(SemanticModel semanticModel, SyntaxNode syntax)
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();

        foreach (var childSyntax in syntax.ChildNodes())
        {
            var operation = semanticModel.GetOperation(childSyntax);
            if (operation is null)
                continue;

            builder.Add(operation);
        }

        return builder.ToImmutable();
    }

    public static ImmutableArray<Operation> CreateChildOperations(SemanticModel semanticModel, IEnumerable<SyntaxNode?> syntaxNodes)
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();

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

    public static Operation? CreateOperationFromBound(SemanticModel semanticModel, BoundNode? boundNode, SyntaxNode fallbackSyntax)
    {
        if (boundNode is null)
            return null;

        var syntax = semanticModel.GetSyntax(boundNode) ?? fallbackSyntax;
        return OperationFactory.Create(semanticModel, syntax, boundNode);
    }
}

internal sealed class BlockOperationImplementation : OperationBase, BlockOperation
{
    private readonly ImmutableArray<LocalSymbol> _locals;
    private ImmutableArray<Operation>? _operations;

    internal BlockOperationImplementation(
        SemanticModel semanticModel,
        BoundNode bound,
        OperationKind kind,
        SyntaxNode syntax,
        ImmutableArray<LocalSymbol> locals,
        TypeSymbol? type,
        bool isImplicit)
        : base(semanticModel, kind, syntax, type, isImplicit)
    {
        _locals = locals.IsDefault ? ImmutableArray<LocalSymbol>.Empty : locals;
    }

    public ImmutableArray<Operation> Operations => _operations ??= OperationUtilities.CreateChildOperations(SemanticModel, Syntax);

    public ImmutableArray<LocalSymbol> Locals => _locals;

    protected override ImmutableArray<Operation> GetChildrenCore() => Operations;
}

internal sealed class ExpressionStatementOperationImplementation : OperationBase, ExpressionStatementOperation
{
    private readonly BoundExpressionStatement _bound;
    private Operation? _operation;

    internal ExpressionStatementOperationImplementation(
        SemanticModel semanticModel,
        BoundExpressionStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ExpressionStatement, syntax, bound.Expression.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Operation => _operation ??= OperationUtilities.CreateOperationFromBound(
        SemanticModel,
        _bound.Expression,
        Syntax is ExpressionStatementSyntax expressionStatement
            ? expressionStatement.Expression
            : Syntax);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Operation is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Operation);
    }
}

internal sealed class FunctionOperationImplementation : OperationBase, FunctionOperation
{
    internal FunctionOperationImplementation(
        SemanticModel semanticModel,
        BoundFunctionStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Function, syntax, null, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
    }
}

internal sealed class VariableDeclarationOperationImplementation : OperationBase, VariableDeclarationOperation
{
    private readonly BoundLocalDeclarationStatement _bound;
    private ImmutableArray<VariableDeclaratorOperation>? _declarators;

    internal VariableDeclarationOperationImplementation(
        SemanticModel semanticModel,
        BoundLocalDeclarationStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.LocalDeclaration, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public ImmutableArray<VariableDeclaratorOperation> Declarators => _declarators ??= CreateDeclarators();

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateRange<Operation>(Declarators);
    }

    private ImmutableArray<VariableDeclaratorOperation> CreateDeclarators()
    {
        var builder = ImmutableArray.CreateBuilder<VariableDeclaratorOperation>();
        var fallbackDeclarators = GetFallbackDeclaratorSyntaxes();

        var index = 0;
        foreach (var declarator in _bound.Declarators)
        {
            var syntax = SemanticModel.GetSyntax(declarator);
            var fallbackSyntax = index < fallbackDeclarators.Length
                ? fallbackDeclarators[index]
                : Syntax;

            var operation = syntax is not null
                ? SemanticModel.GetOperation(syntax)
                : OperationUtilities.CreateOperationFromBound(SemanticModel, declarator, fallbackSyntax);
            if (operation is VariableDeclaratorOperation variableDeclarator)
                builder.Add(variableDeclarator);

            index++;
        }

        return builder.ToImmutable();
    }

    private ImmutableArray<SyntaxNode> GetFallbackDeclaratorSyntaxes()
    {
        var declaration = Syntax switch
        {
            LocalDeclarationStatementSyntax localDeclaration => localDeclaration.Declaration,
            UseDeclarationStatementSyntax useDeclaration => useDeclaration.Declaration,
            _ => null
        };

        return declaration?.Declarators.Cast<SyntaxNode>().ToImmutableArray()
            ?? ImmutableArray<SyntaxNode>.Empty;
    }
}

internal sealed class VariableDeclaratorOperationImplementation : OperationBase, VariableDeclaratorOperation
{
    private readonly BoundVariableDeclarator _bound;
    private VariableInitializerOperation? _initializerOperation;

    internal VariableDeclaratorOperationImplementation(
        SemanticModel semanticModel,
        BoundVariableDeclarator bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.VariableDeclarator, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public LocalSymbol Symbol => _bound.Local;

    public ImmutableArray<Operation> IgnoredArguments => ImmutableArray<Operation>.Empty;

    public VariableInitializerOperation? Initializer
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

            _initializerOperation = new VariableInitializerOperationImplementation(SemanticModel, _bound.Initializer, syntax.Initializer, isImplicit: _bound.Initializer.Reason != BoundExpressionReason.None);
            return _initializerOperation;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Initializer is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create<Operation>(Initializer);
    }
}

internal sealed class VariableInitializerOperationImplementation : OperationBase, VariableInitializerOperation
{
    private readonly BoundExpression _expression;
    private Operation? _valueOperation;

    internal VariableInitializerOperationImplementation(
        SemanticModel semanticModel,
        BoundExpression expression,
        EqualsValueClauseSyntax syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.VariableDeclarator, syntax, expression.Type, isImplicit)
    {
        _expression = expression;
    }

    public Operation? Value => _valueOperation ??= SemanticModel.GetOperation(((EqualsValueClauseSyntax)Syntax).Value);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Value is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Value);
    }
}

internal sealed class ReturnOperationImplementation : OperationBase, ReturnOperation
{
    private readonly BoundReturnStatement _bound;
    private Operation? _returnedValue;

    internal ReturnOperationImplementation(
        SemanticModel semanticModel,
        BoundReturnStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Return, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public Operation? ReturnedValue => _returnedValue ??= ((ReturnStatementSyntax)Syntax).Expression is { } expression ? SemanticModel.GetOperation(expression) : null;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ReturnedValue is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(ReturnedValue);
    }
}

internal sealed class ReturnExpressionOperationImplementation : OperationBase, ReturnOperation
{
    private Operation? _returnedValue;

    internal ReturnExpressionOperationImplementation(
        SemanticModel semanticModel,
        BoundReturnExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ReturnExpression, syntax, bound.Type, isImplicit)
    {
    }

    public Operation? ReturnedValue => _returnedValue ??= Syntax switch
    {
        ReturnExpressionSyntax { Expression: { } expression } => SemanticModel.GetOperation(expression),
        MacroExpansionExpressionSyntax macroExpansion => SemanticModel.GetOperation(macroExpansion.Expression),
        _ => null
    };

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ReturnedValue is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(ReturnedValue);
    }
}

internal sealed class BreakExpressionOperationImplementation : OperationBase, BreakOperation
{
    private readonly BoundBreakExpression _bound;

    internal BreakExpressionOperationImplementation(
        SemanticModel semanticModel,
        BoundBreakExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.BreakExpression, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public LabelSymbol? TargetLabel => _bound.TargetLabel;

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class ContinueExpressionOperationImplementation : OperationBase, ContinueOperation
{
    private readonly BoundContinueExpression _bound;

    internal ContinueExpressionOperationImplementation(
        SemanticModel semanticModel,
        BoundContinueExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ContinueExpression, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public LabelSymbol? TargetLabel => _bound.TargetLabel;

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class YieldExpressionOperationImplementation : OperationBase, YieldOperation
{
    private readonly BoundYieldExpression _bound;
    private Operation? _returnedValue;

    internal YieldExpressionOperationImplementation(
        SemanticModel semanticModel,
        BoundYieldExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.YieldExpression, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? ReturnedValue => _returnedValue ??=
        SemanticModel.GetOperation(((YieldExpressionSyntax)Syntax).Expression);

    public bool IsDelegating => _bound.Iteration is not null;

    public TypeSymbol ElementType => _bound.ElementType;

    protected override ImmutableArray<Operation> GetChildrenCore()
        => ReturnedValue is null ? ImmutableArray<Operation>.Empty : ImmutableArray.Create(ReturnedValue);
}

internal sealed class YieldOperationImplementation : OperationBase, YieldOperation
{
    private readonly BoundYieldStatement _bound;
    private Operation? _returnedValue;

    internal YieldOperationImplementation(
        SemanticModel semanticModel,
        BoundYieldStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Yield, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public Operation? ReturnedValue => _returnedValue ??= Syntax switch
    {
        YieldStatementSyntax yieldStatement when yieldStatement.Expression is { } expression => SemanticModel.GetOperation(expression),
        _ => null,
    };

    public bool IsDelegating => _bound.Iteration is not null;

    public TypeSymbol ElementType => _bound.ElementType;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ReturnedValue is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(ReturnedValue);
    }
}

internal sealed class ThrowOperationImplementation : OperationBase, ThrowOperation
{
    private readonly BoundThrowStatement _bound;
    private Operation? _exception;

    internal ThrowOperationImplementation(
        SemanticModel semanticModel,
        BoundThrowStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Throw, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Exception => _exception ??= SemanticModel.GetOperation(((ThrowStatementSyntax)Syntax).Expression);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Exception is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Exception);
    }
}

internal sealed class ThrowExpressionOperationImplementation : OperationBase, ThrowOperation
{
    private Operation? _exception;

    internal ThrowExpressionOperationImplementation(
        SemanticModel semanticModel,
        BoundThrowExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ThrowExpression, syntax, bound.Type, isImplicit)
    {
    }

    public Operation? Exception => _exception ??= SemanticModel.GetOperation(((ThrowExpressionSyntax)Syntax).Expression);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Exception is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Exception);
    }
}

internal sealed class LiteralOperationImplementation : OperationBase, LiteralOperation
{
    private readonly BoundLiteralExpression _bound;

    internal LiteralOperationImplementation(
        SemanticModel semanticModel,
        BoundLiteralExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Literal, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public object Value => _bound.Value;

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class InterpolatedStringOperationImplementation : OperationBase, InterpolatedStringOperation
{
    private ImmutableArray<InterpolatedStringContentOperation>? _contents;

    internal InterpolatedStringOperationImplementation(
        SemanticModel semanticModel,
        BoundExpression bound,
        InterpolatedStringExpressionSyntax syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.InterpolatedString, syntax, bound.Type, isImplicit)
    {
    }

    public ImmutableArray<InterpolatedStringContentOperation> Contents
        => _contents ??= OperationUtilities.CreateChildOperations(SemanticModel, ((InterpolatedStringExpressionSyntax)Syntax).Contents)
            .OfType<InterpolatedStringContentOperation>()
            .ToImmutableArray();

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Contents.IsDefaultOrEmpty
            ? ImmutableArray<Operation>.Empty
            : Contents.Cast<Operation>().ToImmutableArray();
    }
}

internal sealed class InterpolatedStringTextOperationImplementation : OperationBase, InterpolatedStringTextOperation
{
    internal InterpolatedStringTextOperationImplementation(
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

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class InterpolationOperationImplementation : OperationBase, InterpolationOperation
{
    private Operation? _expression;

    internal InterpolationOperationImplementation(
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

    public Operation? Expression => _expression ??= SemanticModel.GetOperation(((InterpolationSyntax)Syntax).Expression);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Expression is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Expression);
    }
}

internal sealed class DefaultValueOperationImplementation : OperationBase, DefaultValueOperation
{
    internal DefaultValueOperationImplementation(
        SemanticModel semanticModel,
        BoundDefaultValueExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.DefaultValue, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class BreakOperationImplementation : OperationBase, BreakOperation
{
    private readonly BoundBreakStatement _bound;

    internal BreakOperationImplementation(SemanticModel semanticModel, BoundBreakStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Break, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public LabelSymbol? TargetLabel => _bound.TargetLabel;

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class ContinueOperationImplementation : OperationBase, ContinueOperation
{
    private readonly BoundContinueStatement _bound;

    internal ContinueOperationImplementation(SemanticModel semanticModel, BoundContinueStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Continue, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public LabelSymbol? TargetLabel => _bound.TargetLabel;

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class GotoOperationImplementation : OperationBase, GotoOperation
{
    private readonly BoundGotoStatement _bound;

    internal GotoOperationImplementation(SemanticModel semanticModel, BoundGotoStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Goto, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public LabelSymbol Target => _bound.Target;

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class ConditionalGotoOperationImplementation : OperationBase, ConditionalGotoOperation
{
    private readonly BoundConditionalGotoStatement _bound;
    private Operation? _condition;

    internal ConditionalGotoOperationImplementation(SemanticModel semanticModel, BoundConditionalGotoStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.ConditionalGoto, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public LabelSymbol Target => _bound.Target;

    public bool JumpIfTrue => _bound.JumpIfTrue;

    public Operation? Condition
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

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Condition is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Condition);
    }
}

internal sealed class LabeledOperationImplementation : OperationBase, LabeledOperation
{
    private readonly BoundLabeledStatement _bound;

    internal LabeledOperationImplementation(SemanticModel semanticModel, BoundLabeledStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Labeled, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public LabelSymbol Label => _bound.Label;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
    }
}

public abstract class SymbolReferenceOperationBase<TSymbol> : OperationBase, SymbolReferenceOperation<TSymbol> where TSymbol : Symbol
{
    internal SymbolReferenceOperationBase(
        SemanticModel semanticModel,
        OperationKind kind,
        SyntaxNode syntax,
        TSymbol symbol,
        TypeSymbol? type,
        bool isImplicit)
        : base(semanticModel, kind, syntax, type, isImplicit)
    {
        Symbol = symbol;
    }

    public TSymbol Symbol { get; }

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class LocalReferenceOperationImplementation : SymbolReferenceOperationBase<LocalSymbol>, LocalReferenceOperation
{
    internal LocalReferenceOperationImplementation(SemanticModel semanticModel, BoundLocalAccess bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.LocalReference, syntax, bound.Local, bound.Type, isImplicit)
    {
    }

    public LocalSymbol Local => Symbol;
}

internal sealed class VariableReferenceOperationImplementation : SymbolReferenceOperationBase<LocalSymbol>, VariableReferenceOperation
{
    internal VariableReferenceOperationImplementation(SemanticModel semanticModel, BoundVariableExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.VariableReference, syntax, bound.Variable, bound.Type, isImplicit)
    {
    }

    public LocalSymbol Variable => Symbol;
}

internal sealed class ParameterReferenceOperationImplementation : SymbolReferenceOperationBase<ParameterSymbol>, ParameterReferenceOperation
{
    internal ParameterReferenceOperationImplementation(SemanticModel semanticModel, BoundParameterAccess bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.ParameterReference, syntax, bound.Parameter, bound.Type, isImplicit)
    {
    }

    public ParameterSymbol Parameter => Symbol;
}

internal sealed class FieldReferenceOperationImplementation : SymbolReferenceOperationBase<FieldSymbol>, FieldReferenceOperation
{
    internal FieldReferenceOperationImplementation(SemanticModel semanticModel, BoundFieldAccess bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.FieldReference, syntax, bound.Field, bound.Type, isImplicit)
    {
    }

    public FieldSymbol Field => Symbol;
}

internal sealed class PropertyReferenceOperationImplementation : SymbolReferenceOperationBase<PropertySymbol>, PropertyReferenceOperation
{
    internal PropertyReferenceOperationImplementation(SemanticModel semanticModel, BoundPropertyAccess bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.PropertyReference, syntax, bound.Property, bound.Type, isImplicit)
    {
    }

    public PropertySymbol Property => Symbol;
}

internal sealed class MethodReferenceOperationImplementation : SymbolReferenceOperationBase<MethodSymbol>, MethodReferenceOperation
{
    internal MethodReferenceOperationImplementation(SemanticModel semanticModel, BoundMethodGroupExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(
            semanticModel,
            OperationKind.MethodReference,
            syntax,
            bound.SelectedMethod ?? bound.Methods[0],
            bound.Type,
            isImplicit)
    {
    }

    public MethodSymbol Method => Symbol;
}

internal sealed class MemberReferenceOperationImplementation : SymbolReferenceOperationBase<Symbol>, MemberReferenceOperation
{
    private readonly BoundExpression? _receiver;
    private Operation? _instance;

    internal MemberReferenceOperationImplementation(
        SemanticModel semanticModel,
        BoundMemberAccessExpression bound,
        OperationKind kind,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, kind, syntax, bound.Member, bound.Type, isImplicit)
    {
        _receiver = bound.Receiver;
    }

    internal MemberReferenceOperationImplementation(
        SemanticModel semanticModel,
        BoundPointerMemberAccessExpression bound,
        OperationKind kind,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, kind, syntax, bound.Member, bound.Type, isImplicit)
    {
        _receiver = bound.PointerReceiver;
    }

    public Operation? Instance => Symbol.IsStatic || _receiver is null ? null :
        _instance ??= OperationFactory.Create(SemanticModel, SemanticModel.GetSyntax(_receiver) ?? Syntax, _receiver);

    protected override ImmutableArray<Operation> GetChildrenCore() => Instance is { } instance
        ? ImmutableArray.Create(instance)
        : ImmutableArray<Operation>.Empty;
}

internal sealed class UnaryOperationImplementation : OperationBase, UnaryOperation
{
    private readonly BoundUnaryExpression _bound;
    private Operation? _operand;

    internal UnaryOperationImplementation(
        SemanticModel semanticModel,
        BoundUnaryExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Unary, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    internal BoundUnaryOperator Operator => _bound.Operator;

    public bool IsChecked => _bound.Operator.OperatorKind == BoundUnaryOperatorKind.CheckedUnaryMinus;

    public Operation? Operand => _operand ??= SemanticModel.GetOperation(Syntax.ChildNodes().OfType<ExpressionSyntax>().FirstOrDefault());

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Operand is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Operand);
    }
}

internal sealed class BinaryOperationImplementation : OperationBase, BinaryOperation
{
    private readonly BoundBinaryExpression _bound;
    private Operation? _left;
    private Operation? _right;

    internal BinaryOperationImplementation(
        SemanticModel semanticModel,
        BoundBinaryExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Binary, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    internal BoundBinaryOperator Operator => _bound.Operator;

    public BinaryOperatorKind OperatorKind => (_bound.Operator.OperatorKind &
        ~(global::Raven.CodeAnalysis.BinaryOperatorKind.Lifted | global::Raven.CodeAnalysis.BinaryOperatorKind.Checked)) switch
    {
        global::Raven.CodeAnalysis.BinaryOperatorKind.None => BinaryOperatorKind.None,
        global::Raven.CodeAnalysis.BinaryOperatorKind.Addition => BinaryOperatorKind.Add,
        global::Raven.CodeAnalysis.BinaryOperatorKind.Subtraction => BinaryOperatorKind.Subtract,
        global::Raven.CodeAnalysis.BinaryOperatorKind.Multiplication => BinaryOperatorKind.Multiply,
        global::Raven.CodeAnalysis.BinaryOperatorKind.Division => BinaryOperatorKind.Divide,
        global::Raven.CodeAnalysis.BinaryOperatorKind.Equality => BinaryOperatorKind.Equals,
        global::Raven.CodeAnalysis.BinaryOperatorKind.Inequality => BinaryOperatorKind.NotEquals,
        global::Raven.CodeAnalysis.BinaryOperatorKind.GreaterThan => BinaryOperatorKind.GreaterThan,
        global::Raven.CodeAnalysis.BinaryOperatorKind.LessThan => BinaryOperatorKind.LessThan,
        global::Raven.CodeAnalysis.BinaryOperatorKind.GreaterThanOrEqual => BinaryOperatorKind.GreaterThanOrEqual,
        global::Raven.CodeAnalysis.BinaryOperatorKind.LessThanOrEqual => BinaryOperatorKind.LessThanOrEqual,
        global::Raven.CodeAnalysis.BinaryOperatorKind.Modulo => BinaryOperatorKind.Remainder,
        global::Raven.CodeAnalysis.BinaryOperatorKind.BitwiseAnd => BinaryOperatorKind.And,
        global::Raven.CodeAnalysis.BinaryOperatorKind.BitwiseOr => BinaryOperatorKind.Or,
        global::Raven.CodeAnalysis.BinaryOperatorKind.BitwiseXor => BinaryOperatorKind.ExclusiveOr,
        global::Raven.CodeAnalysis.BinaryOperatorKind.LogicalAnd => BinaryOperatorKind.ConditionalAnd,
        global::Raven.CodeAnalysis.BinaryOperatorKind.LogicalOr => BinaryOperatorKind.ConditionalOr,
        global::Raven.CodeAnalysis.BinaryOperatorKind.StringConcatenation => BinaryOperatorKind.Concatenate,
        global::Raven.CodeAnalysis.BinaryOperatorKind.ShiftLeft => BinaryOperatorKind.LeftShift,
        global::Raven.CodeAnalysis.BinaryOperatorKind.ShiftRight => BinaryOperatorKind.RightShift,
        global::Raven.CodeAnalysis.BinaryOperatorKind.UnsignedShiftRight => BinaryOperatorKind.UnsignedRightShift,
        _ => BinaryOperatorKind.None
    };

    public bool IsLifted => (_bound.Operator.OperatorKind & global::Raven.CodeAnalysis.BinaryOperatorKind.Lifted) != 0;

    public bool IsChecked => (_bound.Operator.OperatorKind & global::Raven.CodeAnalysis.BinaryOperatorKind.Checked) != 0;

    public MethodSymbol? OperatorMethod => _bound.Operator.MethodSymbol;


    public Operation? Left => _left ??= SemanticModel.GetOperation(((InfixOperatorExpressionSyntax)Syntax).Left);

    public Operation? Right => _right ??= SemanticModel.GetOperation(((InfixOperatorExpressionSyntax)Syntax).Right);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Left)
            .AddIfNotNull(Right)
            .ToImmutable();
    }
}

internal sealed class CoalesceOperationImplementation : OperationBase, CoalesceOperation
{
    private Operation? _left;
    private Operation? _right;

    internal CoalesceOperationImplementation(
        SemanticModel semanticModel,
        BoundNullCoalesceExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Coalesce, syntax, bound.Type, isImplicit)
    {
    }

    public BinaryOperatorKind OperatorKind => BinaryOperatorKind.None;

    public bool IsLifted => false;

    public bool IsChecked => false;

    public MethodSymbol? OperatorMethod => null;

    public Operation? Left => _left ??= SemanticModel.GetOperation(((NullCoalesceExpressionSyntax)Syntax).Left);

    public Operation? Right => _right ??= SemanticModel.GetOperation(((NullCoalesceExpressionSyntax)Syntax).Right);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Left)
            .AddIfNotNull(Right)
            .ToImmutable();
    }
}

internal sealed class NameOfOperationImplementation : OperationBase, NameOfOperation
{
    private readonly BoundNameOfExpression _bound;
    private Operation? _operand;

    internal NameOfOperationImplementation(
        SemanticModel semanticModel,
        BoundNameOfExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.NameOf, syntax, bound.StringType, isImplicit)
    {
        _bound = bound;
    }

    public string Name => _bound.Name;

    public Operation? Operand => _operand ??= SemanticModel.GetOperation(((NameOfExpressionSyntax)Syntax).Operand);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Operand is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Operand);
    }
}

internal sealed class ParenthesizedOperationImplementation : OperationBase, ParenthesizedOperation
{
    private Operation? _operation;

    internal ParenthesizedOperationImplementation(
        SemanticModel semanticModel,
        BoundParenthesizedExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Parenthesized, syntax, bound.Type, isImplicit)
    {
    }

    public Operation? Operand => _operation ??= SemanticModel.GetOperation(Syntax is CheckedExpressionSyntax checkedExpression
        ? checkedExpression.Expression : ((ParenthesizedExpressionSyntax)Syntax).Expression);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Operand is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Operand);
    }
}

internal sealed class ConversionOperationImplementation : OperationBase, ConversionOperation
{
    private readonly BoundExpression _bound;
    private readonly Conversion _conversion;
    private Operation? _operand;

    internal ConversionOperationImplementation(
        SemanticModel semanticModel,
        BoundExpression bound,
        Conversion conversion,
        SyntaxNode syntax,
        TypeSymbol type,
        bool isImplicit)
        : base(semanticModel, OperationKind.Conversion, syntax, type, isImplicit)
    {
        _bound = bound;
        _conversion = conversion;
    }

    public Conversion Conversion => _conversion;

    public bool IsChecked => _bound is BoundConversionExpression { IsChecked: true };

    public Operation? Operand
    {
        get
        {
            if (_operand is not null)
                return _operand;

            if (Syntax is CastExpressionSyntax cast)
                _operand = SemanticModel.GetOperation(cast.Expression);
            else if (Syntax is AsExpressionSyntax asExpression)
                _operand = SemanticModel.GetOperation(asExpression.Expression);
            else
                _operand = _bound switch
                {
                    BoundConversionExpression conversionExpression => OperationUtilities.CreateOperationFromBound(SemanticModel, conversionExpression.Expression, Syntax),
                    BoundAsExpression boundAsExpression => OperationUtilities.CreateOperationFromBound(SemanticModel, boundAsExpression.Expression, Syntax),
                    _ => null
                };

            return _operand;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Operand is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Operand);
    }
}

internal sealed class PropagationOperationImplementation : OperationBase, PropagationOperation
{
    private readonly BoundPropagateExpression _bound;
    private Operation? _operand;
    private bool _operandInitialized;

    internal PropagationOperationImplementation(
        SemanticModel semanticModel,
        BoundPropagateExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Propagate, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public TypeSymbol OkType => _bound.OkType;

    public TypeSymbol? ErrorType => _bound.ErrorType;

    public NamedTypeSymbol EnclosingResultType => _bound.EnclosingResultType;

    public MethodSymbol EnclosingErrorConstructor => _bound.EnclosingErrorConstructor;

    public Operation? Operand
    {
        get
        {
            if (_operandInitialized)
                return _operand;

            _operandInitialized = true;
            var fallback = Syntax is PropagateExpressionSyntax propagateExpression ? propagateExpression.Expression : Syntax;
            _operand = OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.Operand, fallback);
            return _operand;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Operand is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Operand);
    }
}

internal sealed class DereferenceOperationImplementation : OperationBase, DereferenceOperation
{
    private readonly BoundDereferenceExpression _bound;
    private Operation? _operand;
    private bool _operandInitialized;

    internal DereferenceOperationImplementation(
        SemanticModel semanticModel,
        BoundDereferenceExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Dereference, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Operand
    {
        get
        {
            if (_operandInitialized)
                return _operand;

            _operandInitialized = true;
            var fallback = Syntax is PrefixOperatorExpressionSyntax prefix ? prefix.Expression : Syntax;
            _operand = OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.Reference, fallback);
            return _operand;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Operand is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Operand);
    }
}

internal sealed class ConditionalOperationImplementation : OperationBase, ConditionalOperation
{
    private readonly BoundNode _bound;
    private Operation? _condition;
    private Operation? _whenTrue;
    private Operation? _whenFalse;

    internal ConditionalOperationImplementation(SemanticModel semanticModel, BoundNode bound, SyntaxNode syntax, TypeSymbol? type, bool isImplicit)
        : base(semanticModel, OperationKind.Conditional, syntax, type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Condition
    {
        get
        {
            if (_condition is not null)
                return _condition;

            _condition = Syntax switch
            {
                IfStatementSyntax ifStatement => SemanticModel.GetOperation(ifStatement.Condition),
                IfPatternStatementSyntax ifPatternStatement when _bound is BoundIfStatement boundIf
                    => OperationUtilities.CreateOperationFromBound(SemanticModel, boundIf.Condition, ifPatternStatement.Expression),
                IfExpressionSyntax ifExpression => SemanticModel.GetOperation(ifExpression.Condition),
                IfPatternExpressionSyntax ifPatternExpression when _bound is BoundIfExpression boundIf
                    => OperationUtilities.CreateOperationFromBound(SemanticModel, boundIf.Condition, ifPatternExpression.Value),
                _ => null
            };

            return _condition;
        }
    }

    public Operation? WhenTrue
    {
        get
        {
            if (_whenTrue is not null)
                return _whenTrue;

            _whenTrue = Syntax switch
            {
                IfStatementSyntax ifStatement => SemanticModel.GetOperation(ifStatement.ThenStatement),
                IfPatternStatementSyntax ifPatternStatement => SemanticModel.GetOperation(ifPatternStatement.ThenStatement),
                IfExpressionSyntax ifExpression => SemanticModel.GetOperation(ifExpression.Expression),
                IfPatternExpressionSyntax ifPatternExpression => SemanticModel.GetOperation(ifPatternExpression.Expression),
                _ => null
            };

            return _whenTrue;
        }
    }

    public Operation? WhenFalse
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
                IfPatternStatementSyntax ifPatternStatement => ifPatternStatement.ElseClause is { } elseClause
                    ? SemanticModel.GetOperation(elseClause.Statement)
                    : null,
                IfExpressionSyntax ifExpression => ifExpression.ElseClause is { } elseClause
                    ? SemanticModel.GetOperation(elseClause.Expression)
                    : null,
                IfPatternExpressionSyntax ifPatternExpression => ifPatternExpression.ElseClause is { } elseClause
                    ? SemanticModel.GetOperation(elseClause.Expression)
                    : null,
                _ => null
            };

            return _whenFalse;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Condition)
            .AddIfNotNull(WhenTrue)
            .AddIfNotNull(WhenFalse)
            .ToImmutable();
    }
}

internal sealed class ConditionalAccessOperationImplementation : OperationBase, ConditionalAccessOperation
{
    private readonly BoundExpression _bound;
    private Operation? _receiver;
    private Operation? _whenNotNull;

    internal ConditionalAccessOperationImplementation(SemanticModel semanticModel, BoundExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.ConditionalAccess, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Receiver
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

    public Operation? WhenNotNull
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

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Receiver)
            .AddIfNotNull(WhenNotNull)
            .ToImmutable();
    }

    private Operation? CreateOperationForBoundExpression(BoundExpression expression)
    {
        var syntax = SemanticModel.GetSyntax(expression) ?? Syntax;
        return OperationFactory.Create(SemanticModel, syntax, expression);
    }
}

internal sealed class AwaitOperationImplementation : OperationBase, AwaitOperation
{
    private readonly BoundAwaitExpression _bound;
    private Operation? _operation;

    internal AwaitOperationImplementation(
        SemanticModel semanticModel,
        BoundAwaitExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Await, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Operation => _operation ??= SemanticModel.GetOperation(((PrefixOperatorExpressionSyntax)Syntax).Expression);

    public MethodSymbol GetAwaiterMethod => _bound.GetAwaiterMethod;

    public MethodSymbol GetResultMethod => _bound.GetResultMethod;

    public PropertySymbol IsCompletedProperty => _bound.IsCompletedProperty;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Operation is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Operation);
    }
}

internal sealed class ArgumentOperationImplementation : OperationBase, ArgumentOperation
{
    private Operation? _value;
    private string? _name;

    internal ArgumentOperationImplementation(
        SemanticModel semanticModel,
        BoundExpression bound,
        ArgumentSyntax syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Argument, syntax, bound.Type, isImplicit)
    {
    }

    public Operation? Value => _value ??= SemanticModel.GetOperation(((ArgumentSyntax)Syntax).Expression);

    public string? Name => _name ??= ((ArgumentSyntax)Syntax).NameColon is { Name: IdentifierNameSyntax identifier }
        ? identifier.Identifier.ValueText
        : null;

    public bool IsNamed => Name is not null;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Value is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Value);
    }
}

internal sealed class InvocationOperationImplementation : OperationBase, InvocationOperation
{
    private readonly BoundInvocationExpression _bound;
    private ImmutableArray<Operation>? _arguments;
    private Operation? _instance;

    internal InvocationOperationImplementation(
        SemanticModel semanticModel,
        BoundInvocationExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Invocation, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public MethodSymbol TargetMethod => _bound.Method;

    public Operation? Instance
    {
        get
        {
            if (TargetMethod.IsStatic)
                return null;
            if (_instance is not null || _bound.Receiver is null)
                return _instance;

            var syntax = SemanticModel.GetSyntax(_bound.Receiver) ?? Syntax;
            return _instance = OperationFactory.Create(SemanticModel, syntax, _bound.Receiver);
        }
    }

    public ImmutableArray<Operation> Arguments
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
                _arguments = ImmutableArray<Operation>.Empty;
            }

            return _arguments.Value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();

        if (Instance is not null)
            builder.Add(Instance);

        builder.AddRange(Arguments);
        return builder.ToImmutable();
    }
}

internal sealed class DelegateCreationOperationImplementation : OperationBase, DelegateCreationOperation
{
    internal DelegateCreationOperationImplementation(SemanticModel semanticModel, BoundDelegateCreationExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.DelegateCreation, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class AddressOfOperationImplementation : OperationBase, AddressOfOperation
{
    internal AddressOfOperationImplementation(SemanticModel semanticModel, BoundAddressOfExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.AddressOf, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class StackAllocOperationImplementation : OperationBase, StackAllocOperation
{
    private readonly StackAllocExpressionSyntax _syntax;
    private Operation? _count;

    internal StackAllocOperationImplementation(
        SemanticModel semanticModel,
        BoundStackAllocExpression bound,
        StackAllocExpressionSyntax syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.StackAlloc, syntax, bound.Type, isImplicit)
    {
        _syntax = syntax;
        ElementType = bound.ElementType;
    }

    public TypeSymbol ElementType { get; }

    public Operation Count => _count ??= SemanticModel.GetOperation(_syntax.Count)
        ?? throw new InvalidOperationException("Stack allocation count operation is unavailable.");

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray.Create(Count);
}

internal sealed class ElementAccessOperationImplementation : OperationBase, ElementAccessOperation
{
    private readonly BoundExpression _bound;
    private Operation? _instance;
    private ImmutableArray<Operation>? _arguments;

    internal ElementAccessOperationImplementation(SemanticModel semanticModel, BoundExpression bound, OperationKind kind, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, kind, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Instance
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

    public ImmutableArray<Operation> Arguments
    {
        get
        {
            if (_arguments.HasValue)
                return _arguments.Value;

            _arguments = Syntax is ElementAccessExpressionSyntax elementAccess
                ? OperationUtilities.CreateChildOperations(SemanticModel, elementAccess.ArgumentList.Arguments)
                : ImmutableArray<Operation>.Empty;

            return _arguments.Value;
        }
    }

    public PropertySymbol? Indexer => _bound is BoundIndexerAccessExpression indexerAccess
        ? indexerAccess.Indexer
        : null;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();
        builder.AddIfNotNull(Instance);
        builder.AddRange(Arguments);
        return builder.ToImmutable();
    }
}

internal sealed class IndexOperationImplementation : OperationBase, IndexOperation
{
    private readonly BoundIndexExpression _bound;
    private Operation? _value;

    internal IndexOperationImplementation(SemanticModel semanticModel, BoundIndexExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Index, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public bool IsFromEnd => _bound.IsFromEnd;

    public Operation? Value => _value ??= SemanticModel.GetOperation(((IndexExpressionSyntax)Syntax).Expression);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Value is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Value);
    }
}

internal sealed class RangeOperationImplementation : OperationBase, RangeOperation
{
    private Operation? _left;
    private Operation? _right;

    internal RangeOperationImplementation(SemanticModel semanticModel, BoundRangeExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Range, syntax, bound.Type, isImplicit)
    {
    }

    public Operation? Left => _left ??= ((RangeExpressionSyntax)Syntax).LeftExpression is { } left ? SemanticModel.GetOperation(left) : null;

    public Operation? Right => _right ??= ((RangeExpressionSyntax)Syntax).RightExpression is { } right ? SemanticModel.GetOperation(right) : null;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();

        builder.AddIfNotNull(Left);
        builder.AddIfNotNull(Right);

        return builder.ToImmutable();
    }
}

internal sealed class TypeOfOperationImplementation : OperationBase, TypeOfOperation
{
    internal TypeOfOperationImplementation(SemanticModel semanticModel, BoundTypeOfExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.TypeOf, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class ObjectCreationOperationImplementation : OperationBase, ObjectCreationOperation
{
    private readonly BoundObjectCreationExpression _bound;
    private ImmutableArray<Operation>? _arguments;
    private Operation? _initializer;
    private bool _initializerInitialized;

    internal ObjectCreationOperationImplementation(
        SemanticModel semanticModel,
        BoundObjectCreationExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ObjectCreation, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public MethodSymbol? Constructor => _bound.Constructor;

    public ImmutableArray<Operation> Arguments => _arguments ??= Syntax is InvocationExpressionSyntax creation
        ? OperationUtilities.CreateChildOperations(SemanticModel, creation.ArgumentList.Arguments)
        : ImmutableArray<Operation>.Empty;

    public Operation? Initializer
    {
        get
        {
            if (_initializerInitialized)
                return _initializer;

            _initializerInitialized = true;

            SyntaxNode? syntaxInitializer = Syntax switch
            {
                InvocationExpressionSyntax invocation => invocation.Initializer,
                WithExpressionSyntax withExpression => withExpression,
                _ => null
            };

            if (_bound.Initializer is null || syntaxInitializer is null)
                return null;

            _initializer = OperationFactory.Create(SemanticModel, syntaxInitializer, _bound.Initializer);
            return _initializer;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();
        builder.AddRange(Arguments);
        builder.AddIfNotNull(Initializer);
        return builder.ToImmutable();
    }
}

internal sealed class ObjectInitializerOperationImplementation : OperationBase, ObjectInitializerOperation
{
    private readonly BoundObjectInitializer _bound;
    private ImmutableArray<Operation>? _entries;

    internal ObjectInitializerOperationImplementation(
        SemanticModel semanticModel,
        BoundObjectInitializer bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ObjectInitializer, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public ImmutableArray<Operation> Entries
    {
        get
        {
            if (_entries.HasValue)
                return _entries.Value;

            var syntaxEntries = Syntax switch
            {
                WithExpressionSyntax withExpression => withExpression.Entries.ToArray<SyntaxNode>(),
                _ => []
            };
            var boundEntries = _bound.Entries.ToArray();
            var builder = ImmutableArray.CreateBuilder<Operation>();

            if (boundEntries.Length == syntaxEntries.Length)
            {
                for (var i = 0; i < boundEntries.Length; i++)
                {
                    builder.AddIfNotNull(OperationFactory.Create(SemanticModel, syntaxEntries[i], boundEntries[i]));
                }
            }
            else
            {
                foreach (var boundEntry in boundEntries)
                {
                    var syntax = SemanticModel.GetSyntax(boundEntry) ?? Syntax;
                    builder.AddIfNotNull(OperationFactory.Create(SemanticModel, syntax, boundEntry));
                }
            }

            _entries = builder.ToImmutable();
            return _entries.Value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => Entries;
}

internal sealed class ObjectInitializerAssignmentOperationImplementation : OperationBase, ObjectInitializerAssignmentOperation
{
    private readonly BoundObjectInitializerAssignmentEntry _bound;
    private Operation? _value;
    private bool _valueInitialized;

    internal ObjectInitializerAssignmentOperationImplementation(
        SemanticModel semanticModel,
        BoundObjectInitializerAssignmentEntry bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ObjectInitializerAssignment, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public Symbol Member => _bound.Member;

    public SyntaxKind OperatorTokenKind => _bound.OperatorTokenKind;

    public Operation? Value
    {
        get
        {
            if (_valueInitialized)
                return _value;

            _valueInitialized = true;
            var fallback = Syntax switch
            {
                ObjectInitializerAssignmentEntrySyntax assignmentEntry => assignmentEntry.Expression,
                WithAssignmentSyntax withAssignment => withAssignment.Expression,
                WithExpressionEntrySyntax withExpressionEntry => withExpressionEntry.Expression,
                _ => Syntax
            };
            _value = OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.Value, fallback);
            return _value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Value is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Value);
    }
}

internal sealed class ObjectInitializerExpressionEntryOperationImplementation : OperationBase, ObjectInitializerExpressionEntryOperation
{
    private readonly BoundObjectInitializerExpressionEntry _bound;
    private Operation? _expression;
    private bool _expressionInitialized;

    internal ObjectInitializerExpressionEntryOperationImplementation(
        SemanticModel semanticModel,
        BoundObjectInitializerExpressionEntry bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ObjectInitializerExpressionEntry, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Expression
    {
        get
        {
            if (_expressionInitialized)
                return _expression;

            _expressionInitialized = true;
            var fallback = Syntax switch
            {
                ObjectInitializerExpressionEntrySyntax expressionEntry => expressionEntry.Expression,
                WithExpressionEntrySyntax withExpressionEntry => withExpressionEntry.Expression,
                _ => Syntax
            };
            _expression = OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.Expression, fallback);
            return _expression;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Expression is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Expression);
    }
}

internal sealed class WithOperationImplementation : OperationBase, WithOperation
{
    private readonly BoundWithExpression _bound;
    private Operation? _receiver;
    private ImmutableArray<Operation>? _values;

    internal WithOperationImplementation(
        SemanticModel semanticModel,
        BoundWithExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.With, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Receiver => _receiver ??= SemanticModel.GetOperation(((WithExpressionSyntax)Syntax).Expression);

    public ImmutableArray<Symbol> Members => _bound.Assignments.Select(assignment => assignment.Member).ToImmutableArray();

    public ImmutableArray<Operation> Values => _values ??= OperationUtilities.CreateChildOperations(
        SemanticModel,
        ((WithExpressionSyntax)Syntax).Entries.OfType<WithAssignmentSyntax>().Select(assignment => assignment.Expression));

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();
        builder.AddIfNotNull(Receiver);
        builder.AddRange(Values);
        return builder.ToImmutable();
    }
}

internal sealed class AssignmentOperationImplementation : OperationBase, AssignmentOperation
{
    private readonly BoundExpression _bound;
    private Operation? _target;
    private Operation? _value;

    internal AssignmentOperationImplementation(
        SemanticModel semanticModel,
        BoundExpression bound,
        SyntaxNode syntax,
        TypeSymbol? type,
        bool isImplicit)
        : base(semanticModel, OperationKind.Assignment, syntax, type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Target
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

    public Operation? Value
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

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Target)
            .AddIfNotNull(Value)
            .ToImmutable();
    }
}

internal abstract class LoopOperationBase : OperationBase, LoopOperation
{
    private Operation? _body;

    protected LoopOperationBase(
        SemanticModel semanticModel,
        OperationKind kind,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, kind, syntax, null, isImplicit)
    {
    }

    protected abstract StatementSyntax BodySyntax { get; }

    public Operation? Body => _body ??= SemanticModel.GetOperation(BodySyntax);
}

internal sealed class WhileLoopOperationImplementation : LoopOperationBase, WhileLoopOperation
{
    private readonly BoundWhileStatement _bound;
    private Operation? _condition;

    internal WhileLoopOperationImplementation(SemanticModel semanticModel, BoundWhileStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.WhileLoop, syntax, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Condition => _condition ??= Syntax switch
    {
        WhileStatementSyntax whileStatement => SemanticModel.GetOperation(whileStatement.Condition),
        WhilePatternStatementSyntax whilePatternStatement
            => OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.Condition, whilePatternStatement.Expression),
        _ => null
    };

    protected override StatementSyntax BodySyntax => Syntax switch
    {
        WhileStatementSyntax whileStatement => whileStatement.Statement,
        WhilePatternStatementSyntax whilePatternStatement => whilePatternStatement.Statement,
        _ => throw new InvalidOperationException($"Unexpected while loop syntax '{Syntax.Kind}'.")
    };

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Condition)
            .AddIfNotNull(Body)
            .ToImmutable();
    }
}

internal sealed class LoopStatementOperationImplementation : LoopOperationBase
{
    internal LoopStatementOperationImplementation(SemanticModel semanticModel, BoundLoopStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Loop, syntax, isImplicit)
    {
    }

    protected override StatementSyntax BodySyntax => ((LoopStatementSyntax)Syntax).Statement;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Body)
            .ToImmutable();
    }
}

internal sealed class ForLoopOperationImplementation : LoopOperationBase, ForLoopOperation
{
    private readonly BoundForStatement _bound;
    private Operation? _collection;
    private Operation? _pattern;
    private bool _patternComputed;

    internal ForLoopOperationImplementation(SemanticModel semanticModel, BoundForStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.ForLoop, syntax, isImplicit)
    {
        _bound = bound;
    }

    public LocalSymbol? Local => ((ForStatementSyntax)Syntax).Target is IdentifierNameSyntax or
        VariablePatternSyntax
    {
        BindingKeyword.Kind: SyntaxKind.None,
        Designation: TypedVariableDesignationSyntax
        {
            Designation: SingleVariableDesignationSyntax
        }
    }
        ? _bound.Local
        : null;

    public TypeSymbol ElementType => _bound.Iteration.ElementType;

    public Operation? Collection => _collection ??= SemanticModel.GetOperation(((ForStatementSyntax)Syntax).Expression);

    public Operation? Pattern
    {
        get
        {
            if (_patternComputed)
                return _pattern;

            _pattern = ((ForStatementSyntax)Syntax).Target is PatternSyntax pattern
                ? SemanticModel.GetOperation(pattern)
                : null;
            _patternComputed = true;
            return _pattern;
        }
    }

    protected override StatementSyntax BodySyntax => ((ForStatementSyntax)Syntax).Body;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Collection)
            .AddIfNotNull(Pattern)
            .AddIfNotNull(Body)
            .ToImmutable();
    }
}

internal sealed class TupleOperationImplementation : OperationBase, TupleOperation
{
    private ImmutableArray<Operation>? _elements;

    internal TupleOperationImplementation(
        SemanticModel semanticModel,
        BoundTupleExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Tuple, syntax, bound.Type, isImplicit)
    {
    }

    public ImmutableArray<Operation> Elements
    {
        get
        {
            if (_elements.HasValue)
                return _elements.Value;

            _elements = Syntax is TupleExpressionSyntax tuple
                ? OperationUtilities.CreateChildOperations(SemanticModel, tuple.Arguments)
                : ImmutableArray<Operation>.Empty;

            return _elements.Value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => Elements;
}

internal sealed class TryOperationImplementation : OperationBase, TryOperation
{
    private readonly BoundTryStatement _bound;
    private ImmutableArray<CatchClauseOperation>? _catches;
    private Operation? _body;
    private Operation? _finally;

    internal TryOperationImplementation(SemanticModel semanticModel, BoundTryStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Try, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Body => _body ??= SemanticModel.GetOperation(((TryStatementSyntax)Syntax).Block);

    public ImmutableArray<CatchClauseOperation> Catches
    {
        get
        {
            if (_catches.HasValue)
                return _catches.Value;

            var catches = ((TryStatementSyntax)Syntax).CatchClauses;
            var builder = ImmutableArray.CreateBuilder<CatchClauseOperation>();
            for (var i = 0; i < _bound.CatchClauses.Length && i < catches.Count; i++)
            {
                builder.Add(new CatchClauseOperationImplementation(SemanticModel, _bound.CatchClauses[i], catches[i], isImplicit: false));
            }

            _catches = builder.ToImmutable();

            return _catches.Value;
        }
    }

    public Operation? Finally => _finally ??= ((TryStatementSyntax)Syntax).FinallyClause is { } finallyClause
        ? SemanticModel.GetOperation(finallyClause.Block)
        : null;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();
        builder.AddIfNotNull(Body);
        builder.AddRange(Catches.Cast<Operation>());
        builder.AddIfNotNull(Finally);
        return builder.ToImmutable();
    }
}

internal sealed class LockOperationImplementation : OperationBase, LockOperation
{
    private Operation? _lockedValue;
    private Operation? _body;

    internal LockOperationImplementation(
        SemanticModel semanticModel,
        BoundLockStatement bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Lock, syntax, null, isImplicit)
    {
    }

    public Operation? LockedValue => _lockedValue ??=
        SemanticModel.GetOperation(((LockStatementSyntax)Syntax).Expression);

    public Operation? Body => _body ??=
        SemanticModel.GetOperation(((LockStatementSyntax)Syntax).Statement);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();
        builder.AddIfNotNull(LockedValue);
        builder.AddIfNotNull(Body);
        return builder.ToImmutable();
    }
}

internal sealed class CatchClauseOperationImplementation : OperationBase, CatchClauseOperation
{
    private readonly BoundCatchClause _bound;
    private Operation? _body;

    internal CatchClauseOperationImplementation(SemanticModel semanticModel, BoundCatchClause bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.CatchClause, syntax, bound.ExceptionType, isImplicit)
    {
        _bound = bound;
    }

    public TypeSymbol ExceptionType => _bound.ExceptionType;

    public LocalSymbol? Local => _bound.Local;

    public Operation? Body => _body ??= SemanticModel.GetOperation(((CatchClauseSyntax)Syntax).Block);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Body is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Body);
    }
}

internal sealed class TryExpressionOperationImplementation : OperationBase, TryExpressionOperation
{
    private readonly BoundTryExpression _bound;
    private Operation? _operation;

    internal TryExpressionOperationImplementation(SemanticModel semanticModel, BoundTryExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.TryExpression, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Operation => _operation ??= SemanticModel.GetOperation(((TryExpressionSyntax)Syntax).Expression);

    public TypeSymbol ExceptionType => _bound.ExceptionType;

    public MethodSymbol OkConstructor => _bound.OkConstructor;

    public MethodSymbol ErrorConstructor => _bound.ErrorConstructor;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Operation is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Operation);
    }
}

internal sealed class LambdaOperationImplementation : OperationBase, LambdaOperation
{
    private readonly BoundFunctionExpression _bound;
    private Operation? _body;
    private ImmutableArray<ParameterSymbol>? _parameters;
    private ImmutableArray<Symbol>? _capturedVariables;

    internal LambdaOperationImplementation(
        SemanticModel semanticModel,
        BoundFunctionExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Lambda, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public ImmutableArray<ParameterSymbol> Parameters => _parameters ??= _bound.Parameters.ToImmutableArray();

    public TypeSymbol ReturnType => _bound.ReturnType;

    public Operation? Body
    {
        get
        {
            if (_body is not null)
                return _body;

            if (Syntax is FunctionExpressionSyntax lambda)
                _body = SemanticModel.GetOperation((SyntaxNode?)lambda.Body ?? lambda.ExpressionBody?.Expression);

            return _body;
        }
    }

    public ImmutableArray<NamedTypeSymbol> CandidateDelegates => _bound.CandidateDelegates;

    public ImmutableArray<Symbol> CapturedVariables => _capturedVariables ??= _bound.CapturedVariables.ToImmutableArray();

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Body is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Body);
    }
}

internal sealed class UnionCaseOperationImplementation : OperationBase, UnionCaseOperation
{
    private readonly BoundUnionCaseExpression _bound;
    private ImmutableArray<Operation>? _arguments;

    internal UnionCaseOperationImplementation(
        SemanticModel semanticModel,
        BoundUnionCaseExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.UnionCase, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public NamedTypeSymbol UnionType => _bound.UnionType;

    public NamedTypeSymbol CaseType => _bound.CaseType;

    public MethodSymbol? Constructor => _bound.CaseConstructor;

    public ImmutableArray<Operation> Arguments
    {
        get
        {
            if (_arguments.HasValue)
                return _arguments.Value;

            var builder = ImmutableArray.CreateBuilder<Operation>();
            foreach (var argument in _bound.Arguments)
            {
                builder.AddIfNotNull(OperationUtilities.CreateOperationFromBound(SemanticModel, argument, Syntax));
            }

            _arguments = builder.ToImmutable();
            return _arguments.Value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => Arguments;
}

internal sealed class MatchOperationImplementation : OperationBase, SwitchOperation
{
    private readonly ImmutableArray<BoundMatchArm> _boundArms;
    private Operation? _value;
    private ImmutableArray<Operation>? _patterns;
    private ImmutableArray<Operation>? _guards;
    private ImmutableArray<Operation>? _armValues;

    internal MatchOperationImplementation(SemanticModel semanticModel, BoundMatchExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Switch, syntax, bound.Type, isImplicit)
    {
        _boundArms = bound.Arms;
    }

    internal MatchOperationImplementation(SemanticModel semanticModel, BoundMatchStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Switch, syntax, null, isImplicit)
    {
        _boundArms = bound.Arms;
    }

    private ExpressionSyntax? MatchInput => Syntax switch
    {
        MatchExpressionSyntax matchExpression => matchExpression.Expression,
        PostfixMatchExpressionSyntax matchExpression => matchExpression.Expression,
        MatchStatementSyntax matchStatement => matchStatement.Expression,
        _ => null
    };

    private SyntaxList<MatchArmSyntax> MatchArms => Syntax switch
    {
        MatchExpressionSyntax matchExpression => matchExpression.Arms,
        PostfixMatchExpressionSyntax matchExpression => matchExpression.Arms,
        MatchStatementSyntax matchStatement => matchStatement.Arms,
        _ => default
    };

    public Operation? Value => _value ??= MatchInput is null ? null : SemanticModel.GetOperation(MatchInput);

    public ImmutableArray<Operation> Patterns
    {
        get
        {
            if (_patterns.HasValue)
                return _patterns.Value;

            var builder = ImmutableArray.CreateBuilder<Operation>();
            foreach (var arm in MatchArms)
            {
                builder.AddIfNotNull(SemanticModel.GetOperation(arm.Pattern));
            }

            _patterns = builder.ToImmutable();
            return _patterns.Value;
        }
    }

    public ImmutableArray<Operation> Guards
    {
        get
        {
            if (_guards.HasValue)
                return _guards.Value;

            var builder = ImmutableArray.CreateBuilder<Operation>();
            foreach (var arm in MatchArms)
            {
                if (arm.WhenClause is null)
                    continue;

                builder.AddIfNotNull(SemanticModel.GetOperation(arm.WhenClause.Guard));
            }

            _guards = builder.ToImmutable();
            return _guards.Value;
        }
    }

    public ImmutableArray<Operation> ArmValues
    {
        get
        {
            if (_armValues.HasValue)
                return _armValues.Value;

            var builder = ImmutableArray.CreateBuilder<Operation>();
            var matchArms = MatchArms;

            if (_boundArms.Length == matchArms.Count)
            {
                for (var i = 0; i < matchArms.Count; i++)
                {
                    builder.AddIfNotNull(OperationUtilities.CreateOperationFromBound(SemanticModel, _boundArms[i].Expression, matchArms[i].Expression));
                }
            }
            else
            {
                foreach (var arm in matchArms)
                {
                    builder.AddIfNotNull(SemanticModel.GetOperation(arm.Expression));
                }
            }

            _armValues = builder.ToImmutable();
            return _armValues.Value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();
        builder.AddIfNotNull(Value);
        builder.AddRange(Patterns);
        builder.AddRange(Guards);
        builder.AddRange(ArmValues);
        return builder.ToImmutable();
    }
}

internal sealed class IsPatternOperationImplementation : OperationBase, IsPatternOperation
{
    private readonly BoundIsPatternExpression _bound;
    private Operation? _value;
    private Operation? _pattern;

    internal IsPatternOperationImplementation(SemanticModel semanticModel, BoundIsPatternExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.IsPattern, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Value => _value ??= OperationUtilities.CreateOperationFromBound(
        SemanticModel,
        _bound.Expression,
        Syntax switch
        {
            IsPatternExpressionSyntax isPattern => isPattern.Expression,
            IfPatternStatementSyntax ifPattern => ifPattern.Expression,
            IfPatternExpressionSyntax ifPattern => ifPattern.Value,
            WhilePatternStatementSyntax whilePattern => whilePattern.Expression,
            _ => Syntax
        });

    public Operation? Pattern => _pattern ??= OperationUtilities.CreateOperationFromBound(
        SemanticModel,
        _bound.Pattern,
        Syntax switch
        {
            IsPatternExpressionSyntax isPattern => isPattern.Pattern,
            IfPatternStatementSyntax ifPattern => ifPattern.Pattern,
            IfPatternExpressionSyntax ifPattern => ifPattern.Pattern,
            WhilePatternStatementSyntax whilePattern => whilePattern.Pattern,
            _ => Syntax
        });

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();

        builder.AddIfNotNull(Value);
        builder.AddIfNotNull(Pattern);

        return builder.ToImmutable();
    }
}

internal abstract class PatternOperationBase : OperationBase, PatternOperation
{
    protected PatternOperationBase(
        SemanticModel semanticModel,
        OperationKind kind,
        SyntaxNode syntax,
        TypeSymbol? type,
        bool isImplicit)
        : base(semanticModel, kind, syntax, type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class CasePatternOperationImplementation : PatternOperationBase, CasePatternOperation
{
    private readonly BoundCasePattern _bound;
    private ImmutableArray<Operation>? _arguments;

    internal CasePatternOperationImplementation(
        SemanticModel semanticModel,
        BoundCasePattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.CasePattern, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public UnionCaseTypeSymbol CaseSymbol => _bound.CaseSymbol;

    public MethodSymbol TryGetMethod => _bound.TryGetMethod;

    public ImmutableArray<Operation> Arguments
    {
        get
        {
            if (_arguments.HasValue)
                return _arguments.Value;

            var builder = ImmutableArray.CreateBuilder<Operation>();
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

    protected override ImmutableArray<Operation> GetChildrenCore() => Arguments;
}

internal sealed class DeclarationPatternOperationImplementation : PatternOperationBase, DeclarationPatternOperation
{
    private readonly BoundDeclarationPattern _bound;
    private Operation? _designator;

    internal DeclarationPatternOperationImplementation(
        SemanticModel semanticModel,
        BoundDeclarationPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.DeclarationPattern, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public TypeSymbol DeclaredType => _bound.DeclaredType;

    public Operation? Designator
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

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();

        builder.AddRange(OperationUtilities.CreateChildOperations(SemanticModel, Syntax));
        builder.AddIfNotNull(Designator);

        return builder.ToImmutable();
    }
}

internal sealed class ConstantPatternOperationImplementation : PatternOperationBase, ConstantPatternOperation
{
    private readonly BoundConstantPattern _bound;
    private Operation? _value;

    internal ConstantPatternOperationImplementation(
        SemanticModel semanticModel,
        BoundConstantPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ConstantPattern, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public object ConstantValue => _bound.ConstantValue;

    public Operation? Value
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
            else if (Syntax is ComparisonPatternSyntax comparisonPattern)
            {
                _value = SemanticModel.GetOperation(comparisonPattern.Expression);
            }

            return _value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Value is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Value);
    }
}

internal sealed class ComparisonPatternOperationImplementation : PatternOperationBase, ComparisonPatternOperation
{
    private readonly BoundComparisonPattern _bound;
    private Operation? _value;
    private bool _valueInitialized;

    internal ComparisonPatternOperationImplementation(
        SemanticModel semanticModel,
        BoundComparisonPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.ComparisonPattern, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public SyntaxKind OperatorKind => Syntax is ComparisonPatternSyntax comparisonPattern
        ? comparisonPattern.OperatorToken.Kind
        : _bound.Operator switch
        {
            BoundComparisonPatternOperator.Equals => SyntaxKind.EqualsEqualsToken,
            BoundComparisonPatternOperator.NotEquals => SyntaxKind.NotEqualsToken,
            BoundComparisonPatternOperator.LessThan => SyntaxKind.LessThanExpression,
            BoundComparisonPatternOperator.LessThanOrEqual => SyntaxKind.LessThanOrEqualsExpression,
            BoundComparisonPatternOperator.GreaterThan => SyntaxKind.GreaterThanExpression,
            BoundComparisonPatternOperator.GreaterThanOrEqual => SyntaxKind.GreaterThanOrEqualsExpression,
            _ => SyntaxKind.None
        };

    public Operation? Value
    {
        get
        {
            if (_valueInitialized)
                return _value;

            _valueInitialized = true;
            var fallback = Syntax is ComparisonPatternSyntax comparisonPattern ? comparisonPattern.Expression : Syntax;
            _value = OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.Value, fallback);
            return _value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Value is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Value);
    }
}

internal sealed class PositionalPatternOperationImplementation : PatternOperationBase, PositionalPatternOperation
{
    private ImmutableArray<Operation>? _subpatterns;

    internal PositionalPatternOperationImplementation(
        SemanticModel semanticModel,
        BoundPositionalPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.PositionalPattern, syntax, bound.Type, isImplicit)
    {
    }

    public ImmutableArray<Operation> Subpatterns
    {
        get
        {
            if (_subpatterns.HasValue)
                return _subpatterns.Value;

            _subpatterns = Syntax is PositionalPatternSyntax positionalPattern
                ? OperationUtilities.CreateChildOperations(SemanticModel, positionalPattern.Elements.Select(element => element.Pattern))
                : ImmutableArray<Operation>.Empty;

            return _subpatterns.Value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => Subpatterns;
}

internal sealed class RecursivePatternOperationImplementation : PatternOperationBase, RecursivePatternOperation
{
    private readonly BoundDeconstructPattern _bound;
    private ImmutableArray<Operation>? _arguments;

    internal RecursivePatternOperationImplementation(
        SemanticModel semanticModel,
        BoundDeconstructPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.RecursivePattern, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public TypeSymbol ReceiverType => _bound.ReceiverType;

    public TypeSymbol? NarrowedType => _bound.NarrowedType;

    public MethodSymbol DeconstructMethod => _bound.DeconstructMethod;

    public ImmutableArray<Operation> Arguments
    {
        get
        {
            if (_arguments.HasValue)
                return _arguments.Value;

            var syntaxArguments = Syntax switch
            {
                NominalDeconstructionPatternSyntax nominalPattern => nominalPattern.ArgumentList.Arguments.Select(argument => argument.Pattern).ToImmutableArray(),
                PositionalPatternSyntax positionalPattern => positionalPattern.Elements.Select(element => element.Pattern).ToImmutableArray(),
                _ => ImmutableArray<PatternSyntax>.Empty
            };

            var builder = ImmutableArray.CreateBuilder<Operation>();

            if (_bound.Arguments.Length == syntaxArguments.Length)
            {
                for (var i = 0; i < _bound.Arguments.Length; i++)
                {
                    builder.AddIfNotNull(OperationUtilities.CreateOperationFromBound(
                        SemanticModel,
                        _bound.Arguments[i],
                        syntaxArguments[i]));
                }
            }
            else
            {
                foreach (var argumentSyntax in syntaxArguments)
                    builder.AddIfNotNull(SemanticModel.GetOperation(argumentSyntax));
            }

            _arguments = builder.ToImmutable();
            return _arguments.Value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => Arguments;
}

internal sealed class RangePatternOperationImplementation : PatternOperationBase, RangePatternOperation
{
    private readonly BoundRangePattern _bound;
    private Operation? _lowerBound;
    private Operation? _upperBound;
    private bool _lowerBoundInitialized;
    private bool _upperBoundInitialized;

    internal RangePatternOperationImplementation(
        SemanticModel semanticModel,
        BoundRangePattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.RangePattern, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? LowerBound
    {
        get
        {
            if (_lowerBoundInitialized)
                return _lowerBound;

            _lowerBoundInitialized = true;
            var fallback = Syntax is RangePatternSyntax rangePattern && rangePattern.LowerBound is not null
                ? rangePattern.LowerBound
                : Syntax;
            _lowerBound = OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.LowerBound, fallback);
            return _lowerBound;
        }
    }

    public Operation? UpperBound
    {
        get
        {
            if (_upperBoundInitialized)
                return _upperBound;

            _upperBoundInitialized = true;
            var fallback = Syntax is RangePatternSyntax rangePattern && rangePattern.UpperBound is not null
                ? rangePattern.UpperBound
                : Syntax;
            _upperBound = OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.UpperBound, fallback);
            return _upperBound;
        }
    }

    public bool IsUpperExclusive => _bound.IsUpperExclusive;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(LowerBound)
            .AddIfNotNull(UpperBound)
            .ToImmutable();
    }
}

internal sealed class PropertyPatternOperationImplementation : PatternOperationBase, PropertyPatternOperation
{
    private readonly BoundPropertyPattern _bound;
    private Operation? _designator;
    private bool _designatorInitialized;
    private ImmutableArray<Symbol>? _members;
    private ImmutableArray<Operation>? _subpatterns;

    internal PropertyPatternOperationImplementation(
        SemanticModel semanticModel,
        BoundPropertyPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.PropertyPattern, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public TypeSymbol ReceiverType => _bound.ReceiverType;

    public TypeSymbol? NarrowedType => _bound.NarrowedType;

    public Operation? Designator
    {
        get
        {
            if (_designatorInitialized)
                return _designator;

            _designatorInitialized = true;
            _designator = _bound.Designator is null
                ? null
                : OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.Designator, Syntax);
            return _designator;
        }
    }

    public ImmutableArray<Symbol> Members => _members ??= _bound.Properties.Select(property => property.Member).ToImmutableArray();

    public ImmutableArray<Operation> Subpatterns
    {
        get
        {
            if (_subpatterns.HasValue)
                return _subpatterns.Value;

            var syntaxSubpatterns = Syntax is PropertyPatternSyntax propertyPattern
                ? propertyPattern.PropertyPatternClause.Properties.Select(property => property.Pattern).ToImmutableArray()
                : ImmutableArray<PatternSyntax>.Empty;

            var builder = ImmutableArray.CreateBuilder<Operation>();

            if (syntaxSubpatterns.IsEmpty && _bound.Properties.Length > 0)
            {
                foreach (var property in _bound.Properties)
                {
                    builder.AddIfNotNull(OperationUtilities.CreateOperationFromBound(
                        SemanticModel,
                        property.Pattern,
                        Syntax));
                }

                _subpatterns = builder.ToImmutable();
                return _subpatterns.Value;
            }

            if (_bound.Properties.Length == syntaxSubpatterns.Length)
            {
                for (var i = 0; i < _bound.Properties.Length; i++)
                {
                    builder.AddIfNotNull(OperationUtilities.CreateOperationFromBound(
                        SemanticModel,
                        _bound.Properties[i].Pattern,
                        syntaxSubpatterns[i]));
                }
            }
            else
            {
                foreach (var pattern in syntaxSubpatterns)
                    builder.AddIfNotNull(SemanticModel.GetOperation(pattern));
            }

            _subpatterns = builder.ToImmutable();
            return _subpatterns.Value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();
        builder.AddIfNotNull(Designator);
        builder.AddRange(Subpatterns);
        return builder.ToImmutable();
    }
}

internal sealed class DictionaryPatternOperationImplementation : PatternOperationBase, DictionaryPatternOperation
{
    private readonly BoundDictionaryPattern _bound;
    private Operation? _designator;
    private bool _designatorInitialized;
    private ImmutableArray<Operation>? _keys;
    private ImmutableArray<Operation>? _subpatterns;

    internal DictionaryPatternOperationImplementation(
        SemanticModel semanticModel,
        BoundDictionaryPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.DictionaryPattern, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public TypeSymbol ReceiverType => _bound.ReceiverType;

    public TypeSymbol? NarrowedType => null;

    public TypeSymbol KeyType => _bound.KeyType;

    public TypeSymbol ValueType => _bound.ValueType;

    public Operation? Designator
    {
        get
        {
            if (_designatorInitialized)
                return _designator;

            _designatorInitialized = true;
            _designator = _bound.Designator is null
                ? null
                : OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.Designator, Syntax);
            return _designator;
        }
    }

    public ImmutableArray<Operation> Keys
    {
        get
        {
            if (_keys.HasValue)
                return _keys.Value;

            var syntaxEntries = Syntax is DictionaryPatternSyntax dictionaryPattern
                ? dictionaryPattern.Entries.ToImmutableArray()
                : ImmutableArray<DictionaryPatternEntrySyntax>.Empty;

            var builder = ImmutableArray.CreateBuilder<Operation>();

            if (_bound.Entries.Length == syntaxEntries.Length)
            {
                for (var i = 0; i < _bound.Entries.Length; i++)
                {
                    builder.AddIfNotNull(OperationUtilities.CreateOperationFromBound(
                        SemanticModel,
                        _bound.Entries[i].Key,
                        syntaxEntries[i].Key));
                }
            }
            else
            {
                foreach (var entry in syntaxEntries)
                    builder.AddIfNotNull(SemanticModel.GetOperation(entry.Key));
            }

            _keys = builder.ToImmutable();
            return _keys.Value;
        }
    }

    public ImmutableArray<Operation> Subpatterns
    {
        get
        {
            if (_subpatterns.HasValue)
                return _subpatterns.Value;

            var syntaxEntries = Syntax is DictionaryPatternSyntax dictionaryPattern
                ? dictionaryPattern.Entries.ToImmutableArray()
                : ImmutableArray<DictionaryPatternEntrySyntax>.Empty;

            var builder = ImmutableArray.CreateBuilder<Operation>();

            if (_bound.Entries.Length == syntaxEntries.Length)
            {
                for (var i = 0; i < _bound.Entries.Length; i++)
                {
                    builder.AddIfNotNull(OperationUtilities.CreateOperationFromBound(
                        SemanticModel,
                        _bound.Entries[i].Pattern,
                        syntaxEntries[i].Pattern));
                }
            }
            else
            {
                foreach (var entry in syntaxEntries)
                    builder.AddIfNotNull(SemanticModel.GetOperation(entry.Pattern));
            }

            _subpatterns = builder.ToImmutable();
            return _subpatterns.Value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();
        builder.AddIfNotNull(Designator);
        builder.AddRange(Keys);
        builder.AddRange(Subpatterns);
        return builder.ToImmutable();
    }
}

internal sealed class DiscardPatternOperationImplementation : PatternOperationBase, DiscardPatternOperation
{
    internal DiscardPatternOperationImplementation(
        SemanticModel semanticModel,
        BoundDiscardPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.DiscardPattern, syntax, bound.Type, isImplicit)
    {
    }
}

internal sealed class NotPatternOperationImplementation : PatternOperationBase, NotPatternOperation
{
    private Operation? _pattern;

    internal NotPatternOperationImplementation(
        SemanticModel semanticModel,
        BoundNotPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.NotPattern, syntax, bound.Type, isImplicit)
    {
    }

    public Operation? Pattern => _pattern ??= Syntax is UnaryPatternSyntax unaryPattern
        ? SemanticModel.GetOperation(unaryPattern.Pattern)
        : null;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Pattern is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Pattern);
    }
}

internal sealed class AndPatternOperationImplementation : PatternOperationBase, AndPatternOperation
{
    private Operation? _left;
    private Operation? _right;

    internal AndPatternOperationImplementation(
        SemanticModel semanticModel,
        BoundAndPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.AndPattern, syntax, bound.Type, isImplicit)
    {
    }

    public Operation? Left => _left ??= Syntax is BinaryPatternSyntax binaryPattern
        ? SemanticModel.GetOperation(binaryPattern.Left)
        : null;

    public Operation? Right => _right ??= Syntax is BinaryPatternSyntax binaryPattern
        ? SemanticModel.GetOperation(binaryPattern.Right)
        : null;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Left)
            .AddIfNotNull(Right)
            .ToImmutable();
    }
}

internal sealed class OrPatternOperationImplementation : PatternOperationBase, OrPatternOperation
{
    private Operation? _left;
    private Operation? _right;

    internal OrPatternOperationImplementation(
        SemanticModel semanticModel,
        BoundOrPattern bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.OrPattern, syntax, bound.Type, isImplicit)
    {
    }

    public Operation? Left => _left ??= Syntax is BinaryPatternSyntax binaryPattern
        ? SemanticModel.GetOperation(binaryPattern.Left)
        : null;

    public Operation? Right => _right ??= Syntax is BinaryPatternSyntax binaryPattern
        ? SemanticModel.GetOperation(binaryPattern.Right)
        : null;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Left)
            .AddIfNotNull(Right)
            .ToImmutable();
    }
}

internal abstract class DesignatorOperationBase : OperationBase, DesignatorOperation
{
    protected DesignatorOperationBase(
        SemanticModel semanticModel,
        OperationKind kind,
        SyntaxNode syntax,
        TypeSymbol? type,
        bool isImplicit)
        : base(semanticModel, kind, syntax, type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class SingleVariableDesignatorOperationImplementation : DesignatorOperationBase, SingleVariableDesignatorOperation
{
    private readonly BoundSingleVariableDesignator _bound;

    internal SingleVariableDesignatorOperationImplementation(
        SemanticModel semanticModel,
        BoundSingleVariableDesignator bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.SingleVariableDesignator, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public LocalSymbol Local => _bound.Local;
}

internal sealed class DiscardDesignatorOperationImplementation : DesignatorOperationBase, DiscardDesignatorOperation
{
    internal DiscardDesignatorOperationImplementation(
        SemanticModel semanticModel,
        BoundDiscardDesignator bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.DiscardDesignator, syntax, bound.Type, isImplicit)
    {
    }
}

internal sealed class CollectionOperationImplementation : OperationBase, CollectionOperation
{
    private readonly BoundCollectionExpression _bound;
    private ImmutableArray<Operation>? _elements;

    internal CollectionOperationImplementation(SemanticModel semanticModel, BoundCollectionExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Collection, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        if (_elements.HasValue)
            return _elements.Value;

        var syntaxElements = Syntax is CollectionExpressionSyntax collectionSyntax
            ? collectionSyntax.Elements.ToArray()
            : [];
        var boundElements = _bound.Elements.ToArray();
        var builder = ImmutableArray.CreateBuilder<Operation>();

        if (boundElements.Length == syntaxElements.Length)
        {
            for (var i = 0; i < boundElements.Length; i++)
                builder.AddIfNotNull(OperationUtilities.CreateOperationFromBound(SemanticModel, boundElements[i], syntaxElements[i]));
        }
        else
        {
            foreach (var boundElement in boundElements)
                builder.AddIfNotNull(OperationUtilities.CreateOperationFromBound(SemanticModel, boundElement, Syntax));
        }

        _elements = builder.ToImmutable();
        return _elements.Value;
    }
}

internal sealed class CollectionComprehensionOperationImplementation : OperationBase, CollectionComprehensionOperation
{
    private readonly BoundCollectionComprehensionExpression _bound;
    private Operation? _source;
    private Operation? _condition;
    private Operation? _selector;
    private bool _conditionInitialized;

    internal CollectionComprehensionOperationImplementation(
        SemanticModel semanticModel,
        BoundCollectionComprehensionExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.CollectionComprehension, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Source
    {
        get
        {
            if (_source is not null)
                return _source;

            var fallback = Syntax is CollectionComprehensionElementSyntax comprehension
                ? comprehension.Source
                : Syntax;
            _source = OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.Source, fallback);
            return _source;
        }
    }

    public Operation? Condition
    {
        get
        {
            if (_conditionInitialized)
                return _condition;

            _conditionInitialized = true;
            if (_bound.Condition is null)
                return null;

            var fallback = Syntax is CollectionComprehensionElementSyntax comprehension && comprehension.Condition is not null
                ? comprehension.Condition
                : Syntax;
            _condition = OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.Condition, fallback);
            return _condition;
        }
    }

    public Operation? Selector
    {
        get
        {
            if (_selector is not null)
                return _selector;

            var fallback = Syntax is CollectionComprehensionElementSyntax comprehension
                ? comprehension.Selector
                : Syntax;
            _selector = OperationUtilities.CreateOperationFromBound(SemanticModel, _bound.Selector, fallback);
            return _selector;
        }
    }

    public LocalSymbol IterationLocal => _bound.IterationLocal;

    public TypeSymbol ElementType => _bound.ElementType;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Source)
            .AddIfNotNull(Condition)
            .AddIfNotNull(Selector)
            .ToImmutable();
    }
}

internal sealed class DictionaryOperationImplementation : OperationBase, DictionaryOperation
{
    private readonly BoundDictionaryExpression _bound;
    private ImmutableArray<Operation>? _elements;

    internal DictionaryOperationImplementation(SemanticModel semanticModel, BoundDictionaryExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Dictionary, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public ImmutableArray<Operation> Elements
    {
        get
        {
            if (_elements.HasValue)
                return _elements.Value;

            var syntaxElements = Syntax is CollectionExpressionSyntax collectionSyntax
                ? collectionSyntax.Elements.ToArray()
                : [];
            var boundElements = _bound.Elements.ToArray();
            var builder = ImmutableArray.CreateBuilder<Operation>();

            if (boundElements.Length == syntaxElements.Length)
            {
                for (var i = 0; i < boundElements.Length; i++)
                {
                    Operation? operation = boundElements[i] switch
                    {
                        DictionaryEntryBinding entry => new DictionaryElementOperationImplementation(SemanticModel, entry, syntaxElements[i], isImplicit: false),
                        DictionarySpreadBinding spread => new DictionarySpreadElementOperationImplementation(SemanticModel, spread, syntaxElements[i], isImplicit: false),
                        DictionaryComprehensionBinding comprehension => new DictionaryComprehensionOperationImplementation(SemanticModel, comprehension, syntaxElements[i], isImplicit: false),
                        _ => null
                    };

                    builder.AddIfNotNull(operation);
                }
            }

            _elements = builder.ToImmutable();
            return _elements.Value;
        }
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => Elements;
}

internal sealed class DictionaryElementOperationImplementation : OperationBase, DictionaryElementOperation
{
    private readonly DictionaryEntryBinding _bound;
    private Operation? _key;
    private Operation? _value;

    internal DictionaryElementOperationImplementation(SemanticModel semanticModel, DictionaryEntryBinding bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.DictionaryElement, syntax, bound.Value.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Key => _key ??= OperationUtilities.CreateOperationFromBound(
        SemanticModel,
        _bound.Key,
        Syntax switch
        {
            DictionaryElementSyntax entry => entry.Key,
            DictionarySpreadElementSyntax spreadEntry => spreadEntry.Key,
            _ => Syntax
        });

    public Operation? Value => _value ??= OperationUtilities.CreateOperationFromBound(
        SemanticModel,
        _bound.Value,
        Syntax switch
        {
            DictionaryElementSyntax entry => entry.Value,
            DictionarySpreadElementSyntax spreadEntry => spreadEntry.Value,
            _ => Syntax
        });

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Key)
            .AddIfNotNull(Value)
            .ToImmutable();
    }
}

internal sealed class DictionarySpreadElementOperationImplementation : OperationBase, DictionarySpreadElementOperation
{
    private readonly DictionarySpreadBinding _bound;
    private Operation? _expression;

    internal DictionarySpreadElementOperationImplementation(SemanticModel semanticModel, DictionarySpreadBinding bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.DictionarySpreadElement, syntax, bound.Expression.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Expression => _expression ??= OperationUtilities.CreateOperationFromBound(
        SemanticModel,
        _bound.Expression,
        Syntax is SpreadElementSyntax spreadElement ? spreadElement.Expression : Syntax);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Expression is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Expression);
    }
}

internal sealed class DictionaryComprehensionOperationImplementation : OperationBase, DictionaryComprehensionOperation
{
    private readonly DictionaryComprehensionBinding _bound;
    private Operation? _source;
    private Operation? _condition;
    private Operation? _keySelector;
    private Operation? _valueSelector;
    private bool _conditionInitialized;

    internal DictionaryComprehensionOperationImplementation(SemanticModel semanticModel, DictionaryComprehensionBinding bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.DictionaryComprehension, syntax, null, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Source => _source ??= OperationUtilities.CreateOperationFromBound(
        SemanticModel,
        _bound.Source,
        Syntax is DictionaryComprehensionElementSyntax comprehension ? comprehension.Source : Syntax);

    public Operation? Condition
    {
        get
        {
            if (_conditionInitialized)
                return _condition;

            _conditionInitialized = true;
            if (_bound.Condition is null)
                return null;

            _condition = OperationUtilities.CreateOperationFromBound(
                SemanticModel,
                _bound.Condition,
                Syntax is DictionaryComprehensionElementSyntax comprehension && comprehension.Condition is not null
                    ? comprehension.Condition
                    : Syntax);
            return _condition;
        }
    }

    public Operation? KeySelector => _keySelector ??= OperationUtilities.CreateOperationFromBound(
        SemanticModel,
        _bound.KeySelector,
        Syntax is DictionaryComprehensionElementSyntax comprehension ? comprehension.KeySelector : Syntax);

    public Operation? ValueSelector => _valueSelector ??= OperationUtilities.CreateOperationFromBound(
        SemanticModel,
        _bound.ValueSelector,
        Syntax is DictionaryComprehensionElementSyntax comprehension ? comprehension.ValueSelector : Syntax);

    public LocalSymbol IterationLocal => _bound.IterationLocal;

    public TypeSymbol KeyType => _bound.KeyType;

    public TypeSymbol ValueType => _bound.ValueType;

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return ImmutableArray.CreateBuilder<Operation>()
            .AddIfNotNull(Source)
            .AddIfNotNull(Condition)
            .AddIfNotNull(KeySelector)
            .AddIfNotNull(ValueSelector)
            .ToImmutable();
    }
}

internal sealed class EmptyCollectionOperationImplementation : OperationBase, EmptyCollectionOperation
{
    internal EmptyCollectionOperationImplementation(SemanticModel semanticModel, BoundEmptyCollectionExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.EmptyCollection, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class SpreadElementOperationImplementation : OperationBase, SpreadElementOperation
{
    private readonly BoundSpreadElement _bound;
    private Operation? _expression;

    internal SpreadElementOperationImplementation(SemanticModel semanticModel, BoundSpreadElement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.SpreadElement, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
    }

    public Operation? Expression => _expression ??= OperationUtilities.CreateOperationFromBound(
        SemanticModel,
        _bound.Expression,
        Syntax is SpreadElementSyntax spreadElement ? spreadElement.Expression : Syntax);

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return Expression is null
            ? ImmutableArray<Operation>.Empty
            : ImmutableArray.Create(Expression);
    }
}

internal sealed class TypeOperationImplementation : OperationBase, TypeOperation
{
    internal TypeOperationImplementation(SemanticModel semanticModel, BoundTypeExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.TypeExpression, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class NamespaceOperationImplementation : OperationBase, NamespaceOperation
{
    internal NamespaceOperationImplementation(SemanticModel semanticModel, BoundNamespaceExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.NamespaceExpression, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class SelfOperationImplementation : OperationBase, SelfOperation
{
    internal SelfOperationImplementation(SemanticModel semanticModel, BoundSelfExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.SelfReference, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class UnitOperationImplementation : OperationBase, UnitOperation
{
    internal UnitOperationImplementation(
        SemanticModel semanticModel,
        BoundUnitExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Unit, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}

internal sealed class InvalidOperationImplementation : OperationBase, InvalidOperation
{
    internal InvalidOperationImplementation(
        SemanticModel semanticModel,
        BoundErrorExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Invalid, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        return OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
    }
}

internal sealed class ArrayCreationOperationImplementation : OperationBase, ArrayCreationOperation
{
    private readonly BoundArrayCreationExpression _bound;
    private readonly InvocationExpressionSyntax _syntax;
    private Operation? _length;

    internal ArrayCreationOperationImplementation(SemanticModel semanticModel, BoundArrayCreationExpression bound,
        InvocationExpressionSyntax syntax, bool isImplicit)
        : base(semanticModel, OperationKind.ArrayCreation, syntax, bound.Type, isImplicit)
    {
        _bound = bound;
        _syntax = syntax;
    }

    public Operation Length => _length ??= OperationUtilities.CreateOperationFromBound(
        SemanticModel, _bound.Length, _syntax.ArgumentList.Arguments[0].Expression)
        ?? throw new InvalidOperationException("Array length operation is unavailable.");

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray.Create(Length);
}

internal sealed class ConditionalAccessInstanceOperationImplementation : OperationBase, ConditionalAccessInstanceOperation
{
    internal ConditionalAccessInstanceOperationImplementation(SemanticModel semanticModel, BoundConditionalReceiverExpression bound, SyntaxNode syntax)
        : base(semanticModel, OperationKind.ConditionalAccessInstance, syntax, bound.Type, isImplicit: true)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore() => ImmutableArray<Operation>.Empty;
}
