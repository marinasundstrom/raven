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
    /// An individual variable declarator.
    /// </summary>
    VariableDeclarator,

    /// <summary>
    /// A return statement.
    /// </summary>
    Return,

    /// <summary>
    /// A throw statement.
    /// </summary>
    Throw,

    /// <summary>
    /// A literal value.
    /// </summary>
    Literal,

    /// <summary>
    /// A reference to a local variable.
    /// </summary>
    LocalReference,

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
    /// A collection expression.
    /// </summary>
    Collection,

    /// <summary>
    /// A type expression.
    /// </summary>
    TypeExpression,

    /// <summary>
    /// The unit literal value.
    /// </summary>
    Unit
}
