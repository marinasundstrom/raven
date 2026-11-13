namespace Raven.CodeAnalysis;

// Source method policy is owned by compilation and its selected runtime contract.
internal interface SourceMethodDeclarationServices : SourceDeclarationServices
{
    bool IsTerminalRuntimeCall(MethodSymbol method);
}
