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

internal sealed class ExpressionStatementOperation : Operation
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

internal sealed class FunctionOperation : Operation
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

internal sealed class YieldReturnOperation : Operation
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

internal sealed class YieldBreakOperation : Operation
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

internal sealed class ThrowOperation : Operation
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

internal sealed class LiteralOperation : Operation
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

internal sealed class DefaultValueOperation : Operation
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

internal sealed class BreakOperation : Operation
{
    internal BreakOperation(SemanticModel semanticModel, BoundBreakStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Break, syntax, null, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class ContinueOperation : Operation
{
    internal ContinueOperation(SemanticModel semanticModel, BoundContinueStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Continue, syntax, null, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class GotoOperation : Operation
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

internal sealed class LabeledOperation : Operation
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

public abstract class SymbolReferenceOperation<TSymbol> : Operation where TSymbol : ISymbol
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

internal sealed class LocalReferenceOperation : SymbolReferenceOperation<ILocalSymbol>
{
    internal LocalReferenceOperation(SemanticModel semanticModel, BoundLocalAccess bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.LocalReference, syntax, bound.Local, bound.Type, isImplicit)
    {
    }
}

internal sealed class ParameterReferenceOperation : SymbolReferenceOperation<IParameterSymbol>
{
    internal ParameterReferenceOperation(SemanticModel semanticModel, BoundParameterAccess bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.ParameterReference, syntax, bound.Parameter, bound.Type, isImplicit)
    {
    }
}

internal sealed class FieldReferenceOperation : SymbolReferenceOperation<IFieldSymbol>
{
    internal FieldReferenceOperation(SemanticModel semanticModel, BoundFieldAccess bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.FieldReference, syntax, bound.Field, bound.Type, isImplicit)
    {
    }
}

internal sealed class PropertyReferenceOperation : SymbolReferenceOperation<IPropertySymbol>
{
    internal PropertyReferenceOperation(SemanticModel semanticModel, BoundPropertyAccess bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.PropertyReference, syntax, bound.Property, bound.Type, isImplicit)
    {
    }
}

internal sealed class MethodReferenceOperation : SymbolReferenceOperation<IMethodSymbol>
{
    internal MethodReferenceOperation(SemanticModel semanticModel, BoundMethodGroupExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.MethodReference, syntax, bound.Methods.Single(), bound.Type, isImplicit)
    {
    }
}

internal sealed class MemberReferenceOperation : SymbolReferenceOperation<ISymbol>
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

internal sealed class UnaryOperation : Operation
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

internal sealed class BinaryOperation : Operation
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

internal sealed class ParenthesizedOperation : Operation
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

internal sealed class ConversionOperation : Operation
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

internal sealed class ConditionalOperation : Operation
{
    internal ConditionalOperation(SemanticModel semanticModel, BoundNode bound, SyntaxNode syntax, ITypeSymbol? type, bool isImplicit)
        : base(semanticModel, OperationKind.Conditional, syntax, type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
    }
}

internal sealed class ConditionalAccessOperation : Operation
{
    internal ConditionalAccessOperation(SemanticModel semanticModel, BoundConditionalAccessExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.ConditionalAccess, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
    }
}

internal sealed class AwaitOperation : Operation
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

    public IOperation? Operation => _operation ??= SemanticModel.GetOperation(((UnaryExpressionSyntax)Syntax).Operand);

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

internal sealed class InvocationOperation : Operation
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

internal sealed class DelegateCreationOperation : Operation
{
    internal DelegateCreationOperation(SemanticModel semanticModel, BoundDelegateCreationExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.DelegateCreation, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class AddressOfOperation : Operation
{
    internal AddressOfOperation(SemanticModel semanticModel, BoundAddressOfExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.AddressOf, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class ElementAccessOperation : Operation
{
    internal ElementAccessOperation(SemanticModel semanticModel, BoundExpression bound, OperationKind kind, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, kind, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class IndexOperation : Operation
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

internal sealed class RangeOperation : Operation
{
    private IOperation? _left;
    private IOperation? _right;

    internal RangeOperation(SemanticModel semanticModel, BoundRangeExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Range, syntax, bound.Type, isImplicit)
    {
    }

    public IOperation? Left => _left ??= ((RangeExpressionSyntax)Syntax).Left is { } left ? SemanticModel.GetOperation(left) : null;

    public IOperation? Right => _right ??= ((RangeExpressionSyntax)Syntax).Right is { } right ? SemanticModel.GetOperation(right) : null;

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<IOperation>();

        builder.AddIfNotNull(Left);
        builder.AddIfNotNull(Right);

        return builder.ToImmutable();
    }
}

internal sealed class TypeOfOperation : Operation
{
    internal TypeOfOperation(SemanticModel semanticModel, BoundTypeOfExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.TypeOf, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class ObjectCreationOperation : Operation
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

internal sealed class AssignmentOperation : Operation
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

internal sealed class WhileOperation : Operation
{
    internal WhileOperation(SemanticModel semanticModel, BoundWhileStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.WhileLoop, syntax, null, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class ForOperation : Operation
{
    internal ForOperation(SemanticModel semanticModel, BoundForStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.ForLoop, syntax, null, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class TupleOperation : Operation
{
    internal TupleOperation(
        SemanticModel semanticModel,
        BoundTupleExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Tuple, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
    }
}

internal sealed class TryOperation : Operation
{
    internal TryOperation(SemanticModel semanticModel, BoundTryStatement bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Try, syntax, null, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class CatchClauseOperation : Operation
{
    private readonly BoundCatchClause _bound;

    internal CatchClauseOperation(SemanticModel semanticModel, BoundCatchClause bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.CatchClause, syntax, bound.ExceptionType, isImplicit)
    {
        _bound = bound;
    }

    public ITypeSymbol ExceptionType => _bound.ExceptionType;

    public ILocalSymbol? Local => _bound.Local;

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class TryExpressionOperation : Operation
{
    internal TryExpressionOperation(SemanticModel semanticModel, BoundTryExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.TryExpression, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class LambdaOperation : Operation
{
    internal LambdaOperation(
        SemanticModel semanticModel,
        BoundLambdaExpression bound,
        SyntaxNode syntax,
        bool isImplicit)
        : base(semanticModel, OperationKind.Lambda, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        return OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
    }
}

internal sealed class SwitchOperation : Operation
{
    internal SwitchOperation(SemanticModel semanticModel, BoundMatchExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Switch, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class CollectionOperation : Operation
{
    internal CollectionOperation(SemanticModel semanticModel, BoundCollectionExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.Collection, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class EmptyCollectionOperation : Operation
{
    internal EmptyCollectionOperation(SemanticModel semanticModel, BoundEmptyCollectionExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.EmptyCollection, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class SpreadElementOperation : Operation
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

internal sealed class TypeOperation : Operation
{
    internal TypeOperation(SemanticModel semanticModel, BoundTypeExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.TypeExpression, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class NamespaceOperation : Operation
{
    internal NamespaceOperation(SemanticModel semanticModel, BoundNamespaceExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.NamespaceExpression, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => OperationUtilities.CreateChildOperations(SemanticModel, Syntax);
}

internal sealed class SelfOperation : Operation
{
    internal SelfOperation(SemanticModel semanticModel, BoundSelfExpression bound, SyntaxNode syntax, bool isImplicit)
        : base(semanticModel, OperationKind.SelfReference, syntax, bound.Type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore() => ImmutableArray<IOperation>.Empty;
}

internal sealed class UnitOperation : Operation
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

internal sealed class InvalidOperation : Operation
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
