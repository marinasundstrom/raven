using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.NeoClr;

// Only the explicitly selected native Value implementation may define these
// managed opcode bodies. They are not runtime InternalCalls or general externs.
internal static class NeoClrPayloadDeclaration
{
    internal static bool TryCreate(NeoClrEmitOptions options, IMethodSymbol symbol,
        FunctionStatementSyntax syntax, out SourceCallablePlan? plan)
    {
        plan = null;
        if (!options.PrimitiveImplementations.Contains(PrimitiveType.Value) ||
            !symbol.IsExtern || !symbol.IsStatic || symbol.DeclaredAccessibility != Accessibility.Internal ||
            symbol.Name is not ("IsValue" or "UnpackValue") || symbol.Arity != 1 ||
            symbol.Parameters.Length != 1 || symbol.Parameters[0].RefKind != RefKind.None ||
            symbol.Parameters[0].Type is not INamedTypeSymbol { IsValueType: true } payload ||
            payload.ToFullyQualifiedMetadataName() != "System.Value" ||
            !SymbolEqualityComparer.Default.Equals(payload.ContainingAssembly, symbol.ContainingAssembly) ||
            symbol.TypeParameters[0].ConstraintTypes.Length != 0 ||
            symbol.TypeParameters[0].ConstraintKind != TypeParameterConstraintKind.None ||
            symbol.TypeParameters[0].Variance != VarianceKind.None ||
            symbol.GetAttributes().Length != 0 || syntax.Body is not null || syntax.ExpressionBody is not null ||
            syntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.InternalKeyword or SyntaxKind.ExternKeyword)) ||
            (symbol.Name == "IsValue" ? symbol.ReturnType.SpecialType != SpecialType.System_Boolean :
                !SymbolEqualityComparer.Default.Equals(symbol.ReturnType, symbol.TypeParameters[0])) ||
            !CallableSignature.TryCreate(symbol, out var signature, NeoClrCapabilities.Shared))
            return false;
        var candidate = new SourceCallablePlan(symbol, syntax, null, null, symbol.Name, signature);
        if (candidate.Namespace != "neoCLR.Runtime" || !candidate.IsSupportedBy(NeoClrCapabilities.Shared)) return false;
        plan = candidate;
        return true;
    }
}
