using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// A source declaration, not a backend definition. In particular, an assembly function
// has no logical type owner even when the CLI symbol model supplies a carrier type.
internal sealed record SourceCallablePlan(
    IMethodSymbol Symbol, SyntaxNode Syntax, BlockStatementSyntax? Body,
    INamedTypeSymbol? TypeOwner, string MetadataName, PrimitiveCallableSignature Signature)
{
    internal bool IsAssemblyFunction => TypeOwner is null;

    internal static bool TryCreate(IMethodSymbol symbol, out SourceCallablePlan? plan)
    {
        plan = null;
        if (!symbol.IsStatic || symbol.IsExtern || symbol.DeclaringSyntaxReferences.Length != 1 ||
            !PrimitiveCallableSignature.TryCreate(symbol, out var signature)) return false;
        var syntax = symbol.DeclaringSyntaxReferences[0].GetSyntax();
        switch (syntax)
        {
            case MethodDeclarationSyntax method when symbol.ContainingType is { } owner:
                plan = new(symbol, syntax, method.Body, owner, symbol.MetadataName, signature);
                return true;
            case FunctionStatementSyntax function when function.Parent is GlobalStatementSyntax { Parent: CompilationUnitSyntax }:
                plan = new(symbol, syntax, function.Body, null, symbol.Name, signature);
                return true;
            default:
                return false;
        }
    }

    // The CLI adapter can supply a carrier/lifted name; native emission uses the source
    // metadata name. Concrete owner selection and visibility remain backend policies.
    internal TMethod Define<TMethod>(ICallableDefinitionBuilder<TMethod> builder, string? emittedName = null)
        => builder.DefineMethod(emittedName ?? MetadataName, Signature);

    internal bool TryLowerBody(Compilation compilation, Func<BoundInvocationExpression, bool> permitsConsoleLiteral,
        out LinearMethodBody? body, out LinearBodyFailure? failure)
    {
        if (Body is null)
        {
            body = null;
            failure = new("only block-bodied source callables", Syntax);
            return false;
        }
        return LinearMethodBody.TryLower(Symbol, compilation.GetSemanticModel(Body.SyntaxTree), Body,
            permitsConsoleLiteral, out body, out failure);
    }
}
