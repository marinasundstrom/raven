namespace Raven.CodeAnalysis.Operations;

/// <summary>
/// Describes the semantic shape of an <see cref="IOperation"/>.
/// </summary>
public enum OperationKind
{
    /// <summary>
    /// No specific semantic meaning is available for the operation.
    /// </summary>
    None = 0,

    /// <summary>
    /// Represents an invalid or erroneous operation.
    /// </summary>
    Invalid,

    /// <summary>
    /// A block statement or expression containing nested operations.
    /// </summary>
    Block,

    /// <summary>
    /// A block expression evaluated as a value.
    /// </summary>
    BlockExpression,

    /// <summary>
    /// A single expression used as a statement.
    /// </summary>
    ExpressionStatement,

    /// <summary>
    /// A local variable declaration statement.
    /// </summary>
    LocalDeclaration,

    /// <summary>
    /// A function declaration statement.
    /// </summary>
    Function,

    /// <summary>
    /// An individual variable declarator.
    /// </summary>
    VariableDeclarator,

    /// <summary>
    /// A return statement.
    /// </summary>
    Return,

    /// <summary>
    /// A yield return statement.
    /// </summary>
    YieldReturn,

    /// <summary>
    /// A yield break statement.
    /// </summary>
    YieldBreak,

    /// <summary>
    /// A throw statement.
    /// </summary>
    Throw,

    /// <summary>
    /// A break statement.
    /// </summary>
    Break,

    /// <summary>
    /// A continue statement.
    /// </summary>
    Continue,

    /// <summary>
    /// A goto statement.
    /// </summary>
    Goto,

    /// <summary>
    /// A conditional goto statement.
    /// </summary>
    ConditionalGoto,

    /// <summary>
    /// A labeled statement.
    /// </summary>
    Labeled,

    /// <summary>
    /// A try statement.
    /// </summary>
    Try,

    /// <summary>
    /// A catch clause.
    /// </summary>
    CatchClause,

    /// <summary>
    /// A literal value.
    /// </summary>
    Literal,

    /// <summary>
    /// An interpolated string expression.
    /// </summary>
    InterpolatedString,

    /// <summary>
    /// Literal text inside an interpolated string.
    /// </summary>
    InterpolatedStringText,

    /// <summary>
    /// An interpolation inside an interpolated string.
    /// </summary>
    Interpolation,

    /// <summary>
    /// A default value expression.
    /// </summary>
    DefaultValue,

    /// <summary>
    /// A reference to a local variable.
    /// </summary>
    LocalReference,

    /// <summary>
    /// A reference to a variable expression.
    /// </summary>
    VariableReference,

    /// <summary>
    /// A reference to a method parameter.
    /// </summary>
    ParameterReference,

    /// <summary>
    /// A reference to a field symbol.
    /// </summary>
    FieldReference,

    /// <summary>
    /// A reference to a property symbol.
    /// </summary>
    PropertyReference,

    /// <summary>
    /// A reference to a method or method group.
    /// </summary>
    MethodReference,

    /// <summary>
    /// A unary operation.
    /// </summary>
    Unary,

    /// <summary>
    /// A binary operation.
    /// </summary>
    Binary,

    /// <summary>
    /// A parenthesized expression.
    /// </summary>
    Parenthesized,

    /// <summary>
    /// A conversion operation (explicit or implicit).
    /// </summary>
    Conversion,

    /// <summary>
    /// A conditional statement or expression.
    /// </summary>
    Conditional,

    /// <summary>
    /// A conditional access expression.
    /// </summary>
    ConditionalAccess,

    /// <summary>
    /// A try expression.
    /// </summary>
    TryExpression,

    /// <summary>
    /// An await expression.
    /// </summary>
    Await,

    /// <summary>
    /// A while loop statement or expression.
    /// </summary>
    WhileLoop,

    /// <summary>
    /// A for loop statement or expression.
    /// </summary>
    ForLoop,

    /// <summary>
    /// An invocation expression.
    /// </summary>
    Invocation,

    /// <summary>
    /// An argument in a call or object creation.
    /// </summary>
    Argument,

    /// <summary>
    /// An object creation expression.
    /// </summary>
    ObjectCreation,

    /// <summary>
    /// A simple assignment expression or statement.
    /// </summary>
    Assignment,

    /// <summary>
    /// A delegate creation operation.
    /// </summary>
    DelegateCreation,

    /// <summary>
    /// A tuple expression.
    /// </summary>
    Tuple,

    /// <summary>
    /// An address-of expression.
    /// </summary>
    AddressOf,

    /// <summary>
    /// An array element access expression.
    /// </summary>
    ArrayElement,

    /// <summary>
    /// An indexer element access expression.
    /// </summary>
    IndexerElement,

    /// <summary>
    /// An index expression.
    /// </summary>
    Index,

    /// <summary>
    /// A range expression.
    /// </summary>
    Range,

    /// <summary>
    /// A typeof expression.
    /// </summary>
    TypeOf,

    /// <summary>
    /// A lambda expression.
    /// </summary>
    Lambda,

    /// <summary>
    /// A switch or match expression.
    /// </summary>
    Switch,

    /// <summary>
    /// An is-pattern expression.
    /// </summary>
    IsPattern,

    /// <summary>
    /// A case pattern.
    /// </summary>
    CasePattern,

    /// <summary>
    /// A declaration pattern.
    /// </summary>
    DeclarationPattern,

    /// <summary>
    /// A constant pattern.
    /// </summary>
    ConstantPattern,

    /// <summary>
    /// A tuple pattern.
    /// </summary>
    TuplePattern,

    /// <summary>
    /// A discard pattern.
    /// </summary>
    DiscardPattern,

    /// <summary>
    /// A negated pattern.
    /// </summary>
    NotPattern,

    /// <summary>
    /// A pattern conjunction.
    /// </summary>
    AndPattern,

    /// <summary>
    /// A pattern disjunction.
    /// </summary>
    OrPattern,

    /// <summary>
    /// A single-variable designator.
    /// </summary>
    SingleVariableDesignator,

    /// <summary>
    /// A discard designator.
    /// </summary>
    DiscardDesignator,

    /// <summary>
    /// A collection expression.
    /// </summary>
    Collection,

    /// <summary>
    /// An empty collection expression.
    /// </summary>
    EmptyCollection,

    /// <summary>
    /// A collection spread element.
    /// </summary>
    SpreadElement,

    /// <summary>
    /// A type expression.
    /// </summary>
    TypeExpression,

    /// <summary>
    /// A namespace expression.
    /// </summary>
    NamespaceExpression,

    /// <summary>
    /// A reference to the current instance.
    /// </summary>
    SelfReference,

    /// <summary>
    /// The unit literal value.
    /// </summary>
    Unit
}
