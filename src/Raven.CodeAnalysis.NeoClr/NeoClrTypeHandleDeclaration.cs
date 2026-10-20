using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.NeoClr;

// The selected handle owner supplies a managed ldtoken body, without a host call.
internal static class NeoClrTypeHandleDeclaration
{
    internal static bool TryCreate(NeoClrEmitOptions options, IMethodSymbol symbol,
        FunctionStatementSyntax syntax, out SourceCallablePlan? plan)
    {
        plan = null;
        if (!options.PrimitiveImplementations.Contains(PrimitiveType.RuntimeTypeHandle) ||
            !symbol.IsExtern || !symbol.IsStatic || symbol.DeclaredAccessibility != Accessibility.Internal ||
            symbol.Name != "TypeHandle" || symbol.Arity != 1 || !symbol.Parameters.IsEmpty ||
            symbol.ReturnType.SpecialType != SpecialType.System_RuntimeTypeHandle ||
            !SymbolEqualityComparer.Default.Equals(symbol.ReturnType.ContainingAssembly, symbol.ContainingAssembly) ||
            symbol.TypeParameters[0].ConstraintTypes.Length != 0 ||
            symbol.TypeParameters[0].ConstraintKind != TypeParameterConstraintKind.None ||
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
