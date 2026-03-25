using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    /// <summary>
    /// Lowers a <see cref="BoundUnionCaseExpression"/> to direct union-carrier construction.
    /// </summary>
    public override BoundNode? VisitUnionCaseExpression(BoundUnionCaseExpression node)
    {
        // Lower the constructor arguments first.
        var loweredArgs = node.Arguments.IsDefaultOrEmpty
            ? ImmutableArray<BoundExpression>.Empty
            : node.Arguments.Select(a => (BoundExpression)Visit(a)!).ToImmutableArray();

        IMethodSymbol? ctor = node.CaseConstructor;

        // If no constructor was resolved yet (unit-payload cases), try to find one now.
        if (ctor is null)
        {
            ctor = node.CaseType.Constructors.FirstOrDefault(static c => c.Parameters.Length == 0)
                ?? node.CaseType.Constructors.FirstOrDefault(static c =>
                    c.Parameters.Length == 1 &&
                    c.Parameters[0].Type.SpecialType == SpecialType.System_Unit);

            if (ctor is null)
                throw new InvalidOperationException(
                    $"Cannot lower BoundUnionCaseExpression: no constructor found on '{node.CaseType.Name}'.");

            // If it's a unit-payload ctor and no args were provided, supply the unit value.
            if (ctor.Parameters.Length == 1 && loweredArgs.IsDefaultOrEmpty)
            {
                var unitType = ctor.Parameters[0].Type;
                loweredArgs = ImmutableArray.Create<BoundExpression>(new BoundUnitExpression(unitType));
            }
        }

        // Build: new CaseType(args...)
        var caseCreation = new BoundObjectCreationExpression(ctor, loweredArgs);

        if (node.UnionType.TypeKind == TypeKind.Class &&
            node.CaseType.BaseType is INamedTypeSymbol caseBase &&
            SymbolEqualityComparer.Default.Equals(caseBase, node.UnionType))
        {
            return caseCreation;
        }

        var unionCtor = node.UnionType.TryGetUnionCarrierConstructor(node.CaseType, out var resolvedCtor)
            ? resolvedCtor
            : null;
        if (unionCtor is null)
        {
            throw new InvalidOperationException(
                $"Cannot lower BoundUnionCaseExpression: no union constructor on '{node.UnionType.Name}' accepts case '{node.CaseType.Name}'.");
        }

        return new BoundObjectCreationExpression(unionCtor, ImmutableArray.Create<BoundExpression>(caseCreation));
    }
}
