using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// A source declaration, not a backend definition. In particular, an assembly function
// has no logical type owner even when the CLI symbol model supplies a carrier type.
internal sealed record SourceCallablePlan(
    IMethodSymbol Symbol, SyntaxNode Syntax, BlockStatementSyntax? Body,
    INamedTypeSymbol? TypeOwner, string MetadataName, PrimitiveCallableSignature Signature)
{
    internal EmissionDeclarationKind DeclarationKind => IsAssemblyFunction ? EmissionDeclarationKind.AssemblyFunction : EmissionDeclarationKind.StaticMethod;
    internal Accessibility Visibility => Symbol.DeclaredAccessibility;
    internal bool IsSupportedBy(EmissionCapabilities capabilities) => capabilities.Allows(DeclarationKind) && capabilities.Allows(Signature) &&
        (IsAssemblyFunction || capabilities.AllowsMethodVisibility(Visibility));

    internal bool IsAssemblyFunction => TypeOwner is null;

    internal static bool TryCreate(IMethodSymbol symbol, out SourceCallablePlan? plan, EmissionCapabilities? capabilities = null)
    {
        plan = null;
        if (!symbol.IsStatic || symbol.IsExtern || symbol.DeclaringSyntaxReferences.Length != 1 ||
            !PrimitiveCallableSignature.TryCreate(symbol, out var signature)) return false;
        var syntax = symbol.DeclaringSyntaxReferences[0].GetSyntax();
        switch (syntax)
        {
            case MethodDeclarationSyntax method when symbol.ContainingType is { } owner:
                plan = new(symbol, syntax, method.Body, owner, symbol.MetadataName, signature);
                if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
                plan = null;
                return false;
            case FunctionStatementSyntax function when function.Parent is GlobalStatementSyntax { Parent: CompilationUnitSyntax }:
                plan = new(symbol, syntax, function.Body, null, symbol.Name, signature);
                if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
                plan = null;
                return false;
            default:
                return false;
        }
    }

    // The CLI adapter can supply a carrier/lifted name; native emission uses the source
    // metadata name. Concrete owner selection and physical visibility encoding remain backend policies.
    internal TMethod Define<TMethod>(ICallableDefinitionBuilder<TMethod> builder, string? emittedName = null)
        => builder.DefineMethod(emittedName ?? MetadataName, this);

    internal bool TryLowerBody(Compilation compilation, Func<BoundInvocationExpression, bool> permitsConsoleWrite,
        out LinearMethodBody? body, out LinearBodyFailure? failure, EmissionCapabilities capabilities)
    {
        if (!IsSupportedBy(capabilities))
        {
            body = null;
            failure = new("target does not support declaration " + DeclarationKind + " or its signature", Syntax);
            return false;
        }
        if (Body is null)
        {
            body = null;
            failure = new("only block-bodied source callables", Syntax);
            return false;
        }
        return LinearMethodBody.TryLower(Symbol, compilation.GetSemanticModel(Body.SyntaxTree), Body,
            permitsConsoleWrite, out body, out failure, capabilities);
    }
}
