using System.Collections.Immutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
namespace Raven.CodeAnalysis.Operations;

public interface IBlockOperation : IOperation
{
    /// <summary>
    /// Operations contained within the block.
    /// </summary>
    ImmutableArray<IOperation> Operations { get; }

    /// <summary>
    /// Local declarations contained within the block.
    /// </summary>
    ImmutableArray<ILocalSymbol> Locals { get; }
}

public interface IExpressionStatementOperation : IOperation
{
    /// <summary>
    /// The operation represented by the statement.
    /// </summary>
    IOperation? Operation { get; }
}

public interface IFunctionOperation : IOperation
{
}

public interface IVariableDeclarationOperation : IOperation
{
    /// <summary>
    /// Variable declaration in the statement.
    /// </summary>
    /// <remarks>
    /// In C#, this will always be a single declaration, with all variables in <see cref="IVariableDeclarationOperation.Declarators" />.
    /// </remarks>
    ImmutableArray<IVariableDeclaratorOperation> Declarators { get; }
}

public interface IVariableDeclaratorOperation : IOperation
{
    /// <summary>
    /// Symbol declared by this variable declaration
    /// </summary>
    ILocalSymbol Symbol { get; }
    /// <summary>
    /// Optional initializer of the variable.
    /// </summary>
    /// <remarks>
    /// If this variable is in an <see cref="IVariableDeclarationOperation" />, the initializer may be located
    /// in the parent operation. Call <see cref="OperationExtensions.GetVariableInitializer(IVariableDeclaratorOperation)" />
    /// to check in all locations. It is only possible to have initializers in both locations in VB invalid code scenarios.
    /// </remarks>
    IVariableInitializerOperation? Initializer { get; }
    /// <summary>
    /// Additional arguments supplied to the declarator in error cases, ignored by the compiler. This only used for the C# case of
    /// DeclaredArgumentSyntax nodes on a VariableDeclaratorSyntax.
    /// </summary>
    ImmutableArray<IOperation> IgnoredArguments { get; }
}

public interface IVariableInitializerOperation : IOperation
{
    /// <summary>
    /// Value assigned during initialization.
    /// </summary>
    IOperation? Value { get; }
}

public interface IReturnOperation : IOperation
{
    /// <summary>
    /// Value to be returned.
    /// </summary>
    IOperation? ReturnedValue { get; }
}

public interface IYieldReturnOperation : IOperation
{
    IOperation? ReturnedValue { get; }

    ITypeSymbol ElementType { get; }
}

public interface IYieldBreakOperation : IOperation
{
    ITypeSymbol ElementType { get; }
}

public interface IThrowOperation : IOperation
{
    IOperation? Exception { get; }
}

public interface ILiteralOperation : IOperation
{
    object Value { get; }
}

public interface IDefaultValueOperation : IOperation
{
}

public interface IBreakOperation : IOperation
{
}

public interface IContinueOperation : IOperation
{
}

public interface IGotoOperation : IOperation
{
    ILabelSymbol Target { get; }
}

public interface IConditionalGotoOperation : IOperation
{
    ILabelSymbol Target { get; }

    bool JumpIfTrue { get; }

    IOperation? Condition { get; }
}

public interface ILabeledOperation : IOperation
{
    ILabelSymbol Label { get; }
}

public interface ISymbolReferenceOperation<out TSymbol> : IOperation where TSymbol : ISymbol
{
    TSymbol Symbol { get; }
}

public interface ILocalReferenceOperation : ISymbolReferenceOperation<ILocalSymbol>
{
}

public interface IVariableReferenceOperation : ISymbolReferenceOperation<ILocalSymbol>
{
}

public interface IParameterReferenceOperation : ISymbolReferenceOperation<IParameterSymbol>
{
}

public interface IFieldReferenceOperation : ISymbolReferenceOperation<IFieldSymbol>
{
}

public interface IPropertyReferenceOperation : ISymbolReferenceOperation<IPropertySymbol>
{
}

public interface IMethodReferenceOperation : ISymbolReferenceOperation<IMethodSymbol>
{
}

public interface IMemberReferenceOperation : ISymbolReferenceOperation<ISymbol>
{
}

public interface IUnaryOperation : IOperation
{
    IOperation? Operand { get; }
}

public interface IBinaryOperation : IOperation
{
    IOperation? Left { get; }

    IOperation? Right { get; }
}

public interface IParenthesizedOperation : IOperation
{
    IOperation? Operand { get; }
}

public interface IConversionOperation : IOperation
{
    Conversion Conversion { get; }

    IOperation? Operand { get; }
}

public interface IConditionalOperation : IOperation
{
}

public interface IConditionalAccessOperation : IOperation
{
}

public interface IAwaitOperation : IOperation
{
    IOperation? Operation { get; }

    IMethodSymbol GetAwaiterMethod { get; }

    IMethodSymbol GetResultMethod { get; }

    IPropertySymbol IsCompletedProperty { get; }
}

public interface IInvocationOperation : IOperation
{
    IMethodSymbol TargetMethod { get; }

    IOperation? Instance { get; }

    ImmutableArray<IOperation> Arguments { get; }
}

public interface IDelegateCreationOperation : IOperation
{
}

public interface IAddressOfOperation : IOperation
{
}

public interface IElementAccessOperation : IOperation
{
}

public interface IIndexOperation : IOperation
{
    bool IsFromEnd { get; }

    IOperation? Value { get; }
}

public interface IRangeOperation : IOperation
{
    IOperation? Left { get; }

    IOperation? Right { get; }
}

public interface ITypeOfOperation : IOperation
{
}

public interface IObjectCreationOperation : IOperation
{
    IMethodSymbol? Constructor { get; }

    ImmutableArray<IOperation> Arguments { get; }
}

public interface IAssignmentOperation : IOperation
{
    IOperation? Target { get; }

    IOperation? Value { get; }
}

public interface ILoopOperation : IOperation
{
    IOperation? Body { get; }
}

public interface IWhileLoopOperation : ILoopOperation
{
    IOperation? Condition { get; }
}

public interface IForLoopOperation : ILoopOperation
{
    IOperation? Collection { get; }

    ILocalSymbol? Local { get; }

    ITypeSymbol ElementType { get; }
}

public interface ITupleOperation : IOperation
{
}

public interface ITryOperation : IOperation
{
}

public interface ICatchClauseOperation : IOperation
{
    ITypeSymbol ExceptionType { get; }

    ILocalSymbol? Local { get; }
}

public interface ITryExpressionOperation : IOperation
{
}

public interface ILambdaOperation : IOperation
{
}

public interface ISwitchOperation : IOperation
{
}

public interface IIsPatternOperation : IOperation
{
    IOperation? Value { get; }

    IOperation? Pattern { get; }
}

public interface IPatternOperation : IOperation
{
}

public interface ICasePatternOperation : IPatternOperation
{
    IDiscriminatedUnionCaseSymbol CaseSymbol { get; }

    IMethodSymbol TryGetMethod { get; }
}

public interface IDeclarationPatternOperation : IPatternOperation
{
    ITypeSymbol DeclaredType { get; }

    IOperation? Designator { get; }
}

public interface IConstantPatternOperation : IPatternOperation
{
    object ConstantValue { get; }
}

public interface ITuplePatternOperation : IPatternOperation
{
}

public interface IDiscardPatternOperation : IPatternOperation
{
}

public interface INotPatternOperation : IPatternOperation
{
}

public interface IAndPatternOperation : IPatternOperation
{
}

public interface IOrPatternOperation : IPatternOperation
{
}

public interface IDesignatorOperation : IOperation
{
}

public interface ISingleVariableDesignatorOperation : IDesignatorOperation
{
    ILocalSymbol Local { get; }
}

public interface IDiscardDesignatorOperation : IDesignatorOperation
{
}

public interface ICollectionOperation : IOperation
{
}

public interface IEmptyCollectionOperation : IOperation
{
}

public interface ISpreadElementOperation : IOperation
{
    IOperation? Expression { get; }
}

public interface ITypeOperation : IOperation
{
}

public interface INamespaceOperation : IOperation
{
}

public interface ISelfOperation : IOperation
{
}

public interface IUnitOperation : IOperation
{
}

public interface IInvalidOperation : IOperation
{
}
