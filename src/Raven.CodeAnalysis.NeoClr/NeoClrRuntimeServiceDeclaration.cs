using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.NeoClr;

// Runtime-service admission belongs to this target. The ordinary extern/PInvoke
// path and shared source-body planning do not acquire implicit runtime semantics.
internal static class NeoClrRuntimeServiceDeclaration
{
    internal static bool TryCreate(Compilation compilation, IMethodSymbol symbol,
        FunctionStatementSyntax syntax, out SourceCallablePlan? plan)
    {
        plan = null;
        if (!symbol.IsExtern || !symbol.IsStatic || symbol.Arity != 0 ||
            symbol.DeclaredAccessibility != Accessibility.Internal ||
            syntax.Body is not null || syntax.ExpressionBody is not null ||
            syntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.InternalKeyword or SyntaxKind.ExternKeyword)))
            return false;
        var attributes = symbol.GetAttributes();
        if (attributes.Length != 1) return false;
        var attribute = attributes[0];
        var core = compilation.GetSpecialType(SpecialType.System_Object).ContainingAssembly;
        if (attribute.AttributeClass.ToFullyQualifiedMetadataName() != "System.Runtime.CompilerServices.MethodImplAttribute" ||
            !SymbolEqualityComparer.Default.Equals(attribute.AttributeClass.ContainingAssembly, core) ||
            attribute.ConstructorArguments.Length != 1 || attribute.ConstructorArguments[0].Value is not int flags || flags != 0x1000 ||
            attribute.NamedArguments.Length != 0 ||
            !CallableSignature.TryCreate(symbol, out var signature, NeoClrCapabilities.Shared))
            return false;
        var candidate = new SourceCallablePlan(symbol, syntax, null, null, symbol.Name, signature);
        if (candidate.Namespace != "neoCLR.Runtime" || !candidate.IsSupportedBy(NeoClrCapabilities.Shared)) return false;
        plan = candidate;
        return true;
    }
}
