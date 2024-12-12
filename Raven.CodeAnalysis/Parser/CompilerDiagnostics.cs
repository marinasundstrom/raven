namespace Raven.CodeAnalysis.Parser;

public class CompilerDiagnostics
{
    public readonly static DiagnosticDescriptor SemicolonExpected = DiagnosticDescriptor.Create(
        id: "RAV001",
        title: "Semicolon expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "",
        category: "",
        DiagnosticSeverity.Error);
}