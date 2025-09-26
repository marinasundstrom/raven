using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Operations;

/// <summary>
/// Represents a basic operation with children derived from syntax nodes.
/// </summary>
internal sealed class SimpleOperation : Operation
{
    public SimpleOperation(
        SemanticModel semanticModel,
        OperationKind kind,
        SyntaxNode syntax,
        ITypeSymbol? type,
        bool isImplicit)
        : base(semanticModel, kind, syntax, type, isImplicit)
    {
    }

    protected override ImmutableArray<IOperation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<IOperation>();

        foreach (var childSyntax in Syntax.ChildNodes())
        {
            var childOperation = SemanticModel.GetOperation(childSyntax);
            if (childOperation is null)
                continue;

            builder.Add(childOperation);
        }

        return builder.ToImmutable();
    }
}
