using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class GenericTypeRecoveryTests : CompilationTestBase
{
    [Theory]
    [InlineData("class C<T + U> {}")]
    [InlineData("class C<T + + U> {}")]
    [InlineData("class C<+ T> {}")]
    [InlineData("class C<T, + > {}")]
    public void InvalidTypeParametersReportDiagnosticsWithoutCrashing(string source)
    {
        var (compilation, _) = CreateCompilation(source,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}
