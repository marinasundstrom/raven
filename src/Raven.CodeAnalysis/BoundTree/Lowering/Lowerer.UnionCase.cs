using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    /// <summary>
    /// Lowers a <see cref="BoundUnionCaseExpression"/> to a call to the union's static
    /// <c>Create(CaseType)</c> factory method.  Conversion (op_Implicit) is separate from
    /// creation; we use the named factory here so the emitted IL reads:
    /// <code>
    ///   UnionType.Create(new CaseType(args...))
    /// </code>
    /// If no <c>Create</c> method can be found (should not happen for well-formed DUs) we
    /// fall back to the old tag+payload assignment path.
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

        // Find the static Create(CaseType) factory method on the union type.
        // Match by original-definition equality so generic constructions work correctly.
        var caseOriginalDef = node.CaseType.OriginalDefinition;
        var createMethod = node.UnionType
            .GetMembers("Create")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m =>
                m.IsStatic &&
                m.Parameters.Length == 1 &&
                SymbolEqualityComparer.Default.Equals(
                    m.Parameters[0].Type.OriginalDefinition,
                    caseOriginalDef));

        if (createMethod is not null)
        {
            // Emit: UnionType.Create(new CaseType(args...))
            return new BoundInvocationExpression(createMethod, new[] { caseCreation });
        }

        // Fallback: raw tag+payload assignment (should not be reached for well-formed DUs).
        var conversion = new Conversion(
            isImplicit: true,
            isDiscriminatedUnion: true,
            isUserDefined: false,
            methodSymbol: null);

        var conversionExpr = new BoundConversionExpression(caseCreation, node.UnionType, conversion);
        return VisitConversionExpression(conversionExpr);
    }
}
