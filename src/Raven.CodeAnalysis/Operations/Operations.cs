using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

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

public interface IReturnOperation : IOperation
{
    /// <summary>
    /// Value to be returned.
    /// </summary>
    IOperation? ReturnedValue { get; }
}

public interface IVariableDeclarationOperation : IOperation
{
    /// <summary>
    /// Variable declaration in the statement.
    /// </summary>
    /// <remarks>
    /// In C#, this will always be a single declaration, with all variables in <see cref="IVariableDeclarationOperation.Declarators" />.
    /// </remarks>
    ImmutableArray<IVariableDeclarationOperation> Declarations { get; }
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

public interface IVariableInitializerOperation
{
}
