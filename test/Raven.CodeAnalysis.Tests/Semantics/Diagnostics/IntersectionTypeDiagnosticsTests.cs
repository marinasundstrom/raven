using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class IntersectionTypeDiagnosticsTests : CompilationTestBase
{
    [Theory]
    [InlineData("func Accept(value: A & B) {}")]
    [InlineData("class Holder { field value: A & B }")]
    [InlineData("func Accept(value: System.Collections.Generic.List<A & B>) {}")]
    public void UnsupportedStoragePosition_ReportsIntersectionDiagnostic(string declaration)
    {
        var (compilation, _) = CreateCompilation("interface A {}\ninterface B {}\n" + declaration);
        Assert.Contains(compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.IntersectionTypeNotSupported);
    }
}
