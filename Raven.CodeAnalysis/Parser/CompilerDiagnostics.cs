namespace Raven.CodeAnalysis.Parser;

public class CompilerDiagnostics
{
    public readonly static DiagnosticDescriptor SemicolonExpected = DiagnosticDescriptor.Create(
        id: "RAV1002",
        title: "Semicolon expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "; expected",
        category: "",
        DiagnosticSeverity.Error);
}