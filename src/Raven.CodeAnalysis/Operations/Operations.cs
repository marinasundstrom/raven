using System.Collections.Immutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
namespace Raven.CodeAnalysis.Operations;

public interface BlockOperation : Operation
{
    /// <summary>
    /// Operations contained within the block.
    /// </summary>
    ImmutableArray<Operation> Operations { get; }

    /// <summary>
    /// Local declarations contained within the block.
    /// </summary>
    ImmutableArray<LocalSymbol> Locals { get; }
}

public interface ExpressionStatementOperation : Operation
{
    /// <summary>
    /// The operation represented by the statement.
    /// </summary>
    Operation? Operation { get; }
}

public interface FunctionOperation : Operation
{
}

public interface VariableDeclarationOperation : Operation
{
    /// <summary>
    /// Variable declaration in the statement.
    /// </summary>
    /// <remarks>
    /// In C#, this will always be a single declaration, with all variables in <see cref="VariableDeclarationOperation.Declarators" />.
    /// </remarks>
    ImmutableArray<VariableDeclaratorOperation> Declarators { get; }
}

public interface VariableDeclaratorOperation : Operation
{
    /// <summary>
    /// Symbol declared by this variable declaration
    /// </summary>
    LocalSymbol Symbol { get; }
    /// <summary>
    /// Optional initializer of the variable.
    /// </summary>
    /// <remarks>
    /// If this variable is in an <see cref="VariableDeclarationOperation" />, the initializer may be located
    /// in the parent operation. Use the operation helpers to check all locations.
    /// It is only possible to have initializers in both locations in VB invalid code scenarios.
    /// </remarks>
    VariableInitializerOperation? Initializer { get; }
    /// <summary>
    /// Additional arguments supplied to the declarator in error cases, ignored by the compiler. This only used for the C# case of
    /// DeclaredArgumentSyntax nodes on a VariableDeclaratorSyntax.
    /// </summary>
    ImmutableArray<Operation> IgnoredArguments { get; }
}

public interface VariableInitializerOperation : Operation
{
    /// <summary>
    /// Value assigned during initialization.
    /// </summary>
    Operation? Value { get; }
}

public interface ReturnOperation : Operation
{
    /// <summary>
    /// Value to be returned.
    /// </summary>
    Operation? ReturnedValue { get; }
}

public interface YieldOperation : Operation
{
    /// <summary>Whether this yield delegates to a sequence.</summary>
    bool IsDelegating { get; }

    Operation? ReturnedValue { get; }

    TypeSymbol ElementType { get; }
}

public interface ThrowOperation : Operation
{
    Operation? Exception { get; }
}

public interface LiteralOperation : Operation
{
    object Value { get; }
}

public interface InterpolatedStringOperation : Operation
{
    ImmutableArray<InterpolatedStringContentOperation> Contents { get; }
}

public interface InterpolatedStringContentOperation : Operation
{
}

public interface InterpolatedStringTextOperation : InterpolatedStringContentOperation
{
    string Text { get; }
}

public interface InterpolationOperation : InterpolatedStringContentOperation
{
    Operation? Expression { get; }
}

public interface DefaultValueOperation : Operation
{
}

public interface BreakOperation : Operation
{
    LabelSymbol? TargetLabel { get; }
}

public interface ContinueOperation : Operation
{
    LabelSymbol? TargetLabel { get; }
}

public interface GotoOperation : Operation
{
    LabelSymbol Target { get; }
}

public interface ConditionalGotoOperation : Operation
{
    LabelSymbol Target { get; }

    bool JumpIfTrue { get; }

    Operation? Condition { get; }
}

public interface LabeledOperation : Operation
{
    LabelSymbol Label { get; }
}

public interface SymbolReferenceOperation<out TSymbol> : Operation where TSymbol : Symbol
{
    TSymbol Symbol { get; }
}

public interface LocalReferenceOperation : SymbolReferenceOperation<LocalSymbol>
{
    LocalSymbol Local { get; }
}

public interface VariableReferenceOperation : SymbolReferenceOperation<LocalSymbol>
{
    LocalSymbol Variable { get; }
}

public interface ParameterReferenceOperation : SymbolReferenceOperation<ParameterSymbol>
{
    ParameterSymbol Parameter { get; }
}

public interface FieldReferenceOperation : SymbolReferenceOperation<FieldSymbol>
{
    FieldSymbol Field { get; }
}

public interface PropertyReferenceOperation : SymbolReferenceOperation<PropertySymbol>
{
    PropertySymbol Property { get; }
}

public interface MethodReferenceOperation : SymbolReferenceOperation<MethodSymbol>
{
    MethodSymbol Method { get; }
}

public interface MemberReferenceOperation : SymbolReferenceOperation<Symbol>
{
    /// <summary>The receiver of an instance member, or null for a static member.</summary>
    Operation? Instance => null;
}

public interface UnaryOperation : Operation
{
    Operation? Operand { get; }

    /// <summary>Gets whether the built-in unary operator checks integer overflow.</summary>
    bool IsChecked => false;
}

public interface BinaryOperation : Operation
{
    /// <summary>Gets the bound operator, not an interpretation of its syntax token.</summary>
    BinaryOperatorKind OperatorKind { get; }

    /// <summary>Gets whether the bound operator is lifted over nullable values.</summary>
    bool IsLifted { get; }

    /// <summary>Gets whether the bound operator carries checked arithmetic semantics.</summary>
    bool IsChecked { get; }

    /// <summary>Gets the selected operator method, or null for an intrinsic operation.</summary>
    MethodSymbol? OperatorMethod { get; }

    Operation? Left { get; }

    Operation? Right { get; }
}

public interface CoalesceOperation : BinaryOperation
{
}

public interface NameOfOperation : Operation
{
    string Name { get; }

    Operation? Operand { get; }
}

public interface ParenthesizedOperation : Operation
{
    Operation? Operand { get; }
}

public interface ConversionOperation : Operation
{
    Conversion Conversion { get; }

    bool IsChecked { get; }

    Operation? Operand { get; }
}

public interface PropagationOperation : UnaryOperation
{
    TypeSymbol OkType { get; }

    TypeSymbol? ErrorType { get; }

    NamedTypeSymbol EnclosingResultType { get; }

    MethodSymbol EnclosingErrorConstructor { get; }
}

public interface DereferenceOperation : UnaryOperation
{
}

public interface ConditionalOperation : Operation
{
    Operation? Condition { get; }

    Operation? WhenTrue { get; }

    Operation? WhenFalse { get; }
}

public interface ConditionalAccessOperation : Operation
{
    Operation? Receiver { get; }

    Operation? WhenNotNull { get; }
}

/// <summary>The non-null receiver supplied by an enclosing conditional access.</summary>
public interface ConditionalAccessInstanceOperation : Operation
{
}

public interface AwaitOperation : Operation
{
    Operation? Operation { get; }

    MethodSymbol GetAwaiterMethod { get; }

    MethodSymbol GetResultMethod { get; }

    PropertySymbol IsCompletedProperty { get; }
}

public interface InvocationOperation : Operation
{
    MethodSymbol TargetMethod { get; }

    Operation? Instance { get; }

    ImmutableArray<Operation> Arguments { get; }
}

public interface ArgumentOperation : Operation
{
    Operation? Value { get; }

    string? Name { get; }

    bool IsNamed { get; }
}

public interface DelegateCreationOperation : Operation
{
}

public interface WithOperation : Operation
{
    Operation? Receiver { get; }

    ImmutableArray<Symbol> Members { get; }

    ImmutableArray<Operation> Values { get; }
}

public interface AddressOfOperation : Operation
{
}

public interface StackAllocOperation : Operation
{
    TypeSymbol ElementType { get; }

    Operation Count { get; }
}

public interface ElementAccessOperation : Operation
{
    Operation? Instance { get; }

    ImmutableArray<Operation> Arguments { get; }

    PropertySymbol? Indexer { get; }
}

public interface IndexOperation : Operation
{
    bool IsFromEnd { get; }

    Operation? Value { get; }
}

public interface RangeOperation : Operation
{
    Operation? Left { get; }

    Operation? Right { get; }
}

public interface TypeOfOperation : Operation
{
}

public interface ObjectCreationOperation : Operation
{
    MethodSymbol? Constructor { get; }

    ImmutableArray<Operation> Arguments { get; }

    Operation? Initializer { get; }
}

public interface ObjectInitializerOperation : Operation
{
    ImmutableArray<Operation> Entries { get; }
}

public interface ObjectInitializerAssignmentOperation : Operation
{
    Symbol Member { get; }

    SyntaxKind OperatorTokenKind { get; }

    Operation? Value { get; }
}

public interface ObjectInitializerExpressionEntryOperation : Operation
{
    Operation? Expression { get; }
}

public interface AssignmentOperation : Operation
{
    Operation? Target { get; }

    Operation? Value { get; }
}

public interface LoopOperation : Operation
{
    Operation? Body { get; }
}

public interface WhileLoopOperation : LoopOperation
{
    Operation? Condition { get; }
}

public interface ForLoopOperation : LoopOperation
{
    Operation? Collection { get; }

    Operation? Pattern { get; }

    LocalSymbol? Local { get; }

    TypeSymbol ElementType { get; }
}

public interface TupleOperation : Operation
{
    ImmutableArray<Operation> Elements { get; }
}

public interface TryOperation : Operation
{
    Operation? Body { get; }

    ImmutableArray<CatchClauseOperation> Catches { get; }

    Operation? Finally { get; }
}

public interface LockOperation : Operation
{
    Operation? LockedValue { get; }

    Operation? Body { get; }
}

public interface CatchClauseOperation : Operation
{
    TypeSymbol ExceptionType { get; }

    LocalSymbol? Local { get; }

    Operation? Body { get; }
}

public interface TryExpressionOperation : Operation
{
    Operation? Operation { get; }

    TypeSymbol ExceptionType { get; }

    MethodSymbol OkConstructor { get; }

    MethodSymbol ErrorConstructor { get; }
}

public interface LambdaOperation : Operation
{
    ImmutableArray<ParameterSymbol> Parameters { get; }

    TypeSymbol ReturnType { get; }

    Operation? Body { get; }

    ImmutableArray<NamedTypeSymbol> CandidateDelegates { get; }

    ImmutableArray<Symbol> CapturedVariables { get; }
}

public interface UnionCaseOperation : Operation
{
    NamedTypeSymbol UnionType { get; }

    NamedTypeSymbol CaseType { get; }

    MethodSymbol? Constructor { get; }

    ImmutableArray<Operation> Arguments { get; }
}

public interface SwitchOperation : Operation
{
    Operation? Value { get; }

    ImmutableArray<Operation> Patterns { get; }

    ImmutableArray<Operation> Guards { get; }

    ImmutableArray<Operation> ArmValues { get; }
}

public interface IsPatternOperation : Operation
{
    Operation? Value { get; }

    Operation? Pattern { get; }
}

public interface PatternOperation : Operation
{
}

public interface CasePatternOperation : PatternOperation
{
    UnionCaseTypeSymbol CaseSymbol { get; }

    MethodSymbol TryGetMethod { get; }

    ImmutableArray<Operation> Arguments { get; }
}

public interface DeclarationPatternOperation : PatternOperation
{
    TypeSymbol DeclaredType { get; }

    Operation? Designator { get; }
}

public interface ConstantPatternOperation : PatternOperation
{
    object ConstantValue { get; }

    Operation? Value { get; }
}

public interface ComparisonPatternOperation : PatternOperation
{
    SyntaxKind OperatorKind { get; }

    Operation? Value { get; }
}

public interface PositionalPatternOperation : PatternOperation
{
    ImmutableArray<Operation> Subpatterns { get; }
}

public interface ReceiverPatternOperation : PatternOperation
{
    TypeSymbol ReceiverType { get; }

    TypeSymbol? NarrowedType { get; }
}

public interface RecursivePatternOperation : ReceiverPatternOperation
{
    MethodSymbol DeconstructMethod { get; }

    ImmutableArray<Operation> Arguments { get; }
}

public interface RangePatternOperation : PatternOperation
{
    Operation? LowerBound { get; }

    Operation? UpperBound { get; }

    bool IsUpperExclusive { get; }
}

public interface PropertyPatternOperation : ReceiverPatternOperation
{
    Operation? Designator { get; }

    ImmutableArray<Symbol> Members { get; }

    ImmutableArray<Operation> Subpatterns { get; }
}

public interface DictionaryPatternOperation : ReceiverPatternOperation
{
    TypeSymbol KeyType { get; }

    TypeSymbol ValueType { get; }

    Operation? Designator { get; }

    ImmutableArray<Operation> Keys { get; }

    ImmutableArray<Operation> Subpatterns { get; }
}

public interface DiscardPatternOperation : PatternOperation
{
}

public interface NotPatternOperation : PatternOperation
{
    Operation? Pattern { get; }
}

public interface AndPatternOperation : PatternOperation
{
    Operation? Left { get; }

    Operation? Right { get; }
}

public interface OrPatternOperation : PatternOperation
{
    Operation? Left { get; }

    Operation? Right { get; }
}

public interface DesignatorOperation : Operation
{
}

public interface SingleVariableDesignatorOperation : DesignatorOperation
{
    LocalSymbol Local { get; }
}

public interface DiscardDesignatorOperation : DesignatorOperation
{
}

public interface CollectionOperation : Operation
{
}

public interface DictionaryOperation : Operation
{
    ImmutableArray<Operation> Elements { get; }
}

public interface CollectionComprehensionOperation : Operation
{
    Operation? Source { get; }

    Operation? Condition { get; }

    Operation? Selector { get; }

    LocalSymbol IterationLocal { get; }

    TypeSymbol ElementType { get; }
}

public interface DictionaryElementOperation : Operation
{
    Operation? Key { get; }

    Operation? Value { get; }
}

public interface DictionarySpreadElementOperation : Operation
{
    Operation? Expression { get; }
}

public interface DictionaryComprehensionOperation : Operation
{
    Operation? Source { get; }

    Operation? Condition { get; }

    Operation? KeySelector { get; }

    Operation? ValueSelector { get; }

    LocalSymbol IterationLocal { get; }

    TypeSymbol KeyType { get; }

    TypeSymbol ValueType { get; }
}

public interface EmptyCollectionOperation : Operation
{
}

public interface SpreadElementOperation : Operation
{
    Operation? Expression { get; }
}

public interface TypeOperation : Operation
{
}

public interface NamespaceOperation : Operation
{
}

public interface SelfOperation : Operation
{
}

public interface UnitOperation : Operation
{
}

public interface InvalidOperation : Operation
{
}

public interface ArrayCreationOperation : Operation
{
    Operation Length { get; }
}
