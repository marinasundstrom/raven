using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

// Preserve the compiler's canonical field order and bound initializer expressions.
// Constructor chaining/base initialization remain the target constructor driver's responsibility.
internal static class FieldInitializationPlan
{
    internal static IEnumerable<BoundAssignmentStatement> Create(Compilation compilation, IMethodSymbol method)
    {
        foreach (var field in method.ContainingType!.GetMembers().OfType<SourceFieldSymbol>())
        {
            if (field.IsStatic != method.IsStatic || field.Initializer is null) continue;
            if (field.Initializer is BoundParameterAccess parameter &&
                method.Parameters.All(p => !SymbolEqualityComparer.Default.Equals(p, parameter.Parameter))) continue;
            yield return new BoundAssignmentStatement(new BoundFieldAssignmentExpression(
                method.IsStatic ? null : new BoundSelfExpression(method.ContainingType), field,
                field.Initializer, compilation.GetSpecialType(SpecialType.System_Unit)));
        }
    }
}
