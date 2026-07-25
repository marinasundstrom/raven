namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class AllowsRefStructConstraintDiagnosticsTests : CompilationTestBase
{
    [Theory]
    [InlineData("class Box<T> where T: allows ref struct, new() {}")]
    [InlineData("class Box<T: allows ref struct, new()> {}")]
    public void AllowsRefStruct_MustBeLast(string source)
    {
        AssertHasDiagnostic(source, CompilerDiagnostics.AllowsRefStructConstraintMustBeLast);
    }

    [Fact]
    public void AllowsRefStruct_CannotBeDuplicated()
    {
        const string source = """
            class Box<T> where T: allows ref struct, allows ref struct {}
            """;

        AssertHasDiagnostic(source, CompilerDiagnostics.DuplicateAllowsRefStructConstraint);
    }

    [Theory]
    [InlineData("class Box<T> where T: class, allows ref struct {}")]
    [InlineData("class Box<T: class, allows ref struct> {}")]
    public void AllowsRefStruct_CannotBeCombinedWithClass(string source)
    {
        AssertHasDiagnostic(source, CompilerDiagnostics.AllowsRefStructConstraintConflictsWithClass);
    }

    [Theory]
    [InlineData("class Box<T> where T: new(), allows ref struct {}")]
    [InlineData("class Box<T: new(), allows ref struct> {}")]
    public void AllowsRefStruct_LastAfterOtherCompatibleConstraint_IsValid(string source)
    {
        var (compilation, _) = CreateCompilation(source);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    private void AssertHasDiagnostic(string source, DiagnosticDescriptor descriptor)
    {
        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == descriptor);
    }
}
