using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Operations;

/// <summary>
/// Represents a basic operation with children derived from syntax nodes.
/// </summary>
internal sealed class SimpleOperationImplementation : OperationBase
{
    public SimpleOperationImplementation(
        SemanticModel semanticModel,
        OperationKind kind,
        SyntaxNode syntax,
        TypeSymbol? type,
        bool isImplicit)
        : base(semanticModel, kind, syntax, type, isImplicit)
    {
    }

    protected override ImmutableArray<Operation> GetChildrenCore()
    {
        var builder = ImmutableArray.CreateBuilder<Operation>();

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
