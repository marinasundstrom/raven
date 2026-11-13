namespace Raven.CodeAnalysis;

public partial class Compilation : SourceMethodDeclarationServices
{
    bool SourceMethodDeclarationServices.IsTerminalRuntimeCall(MethodSymbol method)
        => RuntimeFailureContract.IsTerminal(method, Options);
}
