using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.NeoClr;

// Source-core managed intrinsic. This supplies the existing checked, uninitialized
// array operation without importing a bootstrap method or adding a host callback.
internal static class NeoClrStorageDeclaration
{
    internal static bool TryCreate(IMethodSymbol symbol, FunctionStatementSyntax syntax, out SourceCallablePlan? plan)
    {
        plan = null;
        if (!symbol.IsExtern || !symbol.IsStatic || symbol.DeclaredAccessibility != Accessibility.Internal ||
            symbol.Name != "ReserveArray" || symbol.Arity != 1 || symbol.Parameters.Length != 1 ||
            symbol.Parameters[0].RefKind != RefKind.None || symbol.Parameters[0].Type.SpecialType != SpecialType.System_Int32 ||
            symbol.ReturnType is not IArrayTypeSymbol { Rank: 1, IsFixedArray: false } array ||
            !SymbolEqualityComparer.Default.Equals(array.ElementType, symbol.TypeParameters[0]) ||
            symbol.TypeParameters[0].ConstraintTypes.Length != 0 || symbol.TypeParameters[0].ConstraintKind != TypeParameterConstraintKind.None ||
            symbol.TypeParameters[0].Variance != VarianceKind.None || symbol.GetAttributes().Length != 0 ||
            syntax.Body is not null || syntax.ExpressionBody is not null ||
            syntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.InternalKeyword or SyntaxKind.ExternKeyword)) ||
            !CallableSignature.TryCreate(symbol, out var signature, NeoClrCapabilities.Shared))
            return false;
        var candidate = new SourceCallablePlan(symbol, syntax, null, null, symbol.Name, signature);
        if (candidate.Namespace != "neoCLR.Runtime" || !candidate.IsSupportedBy(NeoClrCapabilities.Shared)) return false;
        plan = candidate;
        return true;
    }
}
