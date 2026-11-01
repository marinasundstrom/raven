using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class ExtensionRecoveryTests
{
    [Theory]
    [InlineData("private static readonly field cache: int = 1")]
    [InlineData("val Value: int\nval Value: int")]
    public void InvalidStorageDeclarations_ReportDiagnosticsWithoutCrashing(string members)
    {
        var compilation = Compilation.Create("ExtensionRecovery", [SyntaxTree.ParseText($$"""
            extension TextHelpers for string {
                {{members}}
                func LengthAgain() -> int => self.Length
            }
            """)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        Assert.Equal(diagnostics.Select(diagnostic => diagnostic.Id),
            compilation.GetDiagnostics().Select(diagnostic => diagnostic.Id));
    }
}
