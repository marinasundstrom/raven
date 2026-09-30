using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TargetInitializationDiagnosticTests
{
    private static Compilation Create(CompilationOptions? options = null, string source = "class Example {}")
        => Compilation.Create("MissingCore", [SyntaxTree.ParseText(source)], [],
            options ?? CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));

    [Fact]
    public void FailedInitializationIsStableAcrossDiagnosticEntryPointsAndConcurrentCalls()
    {
        var compilation = Create();
        var expected = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal("RAVT004", expected.Id);
        Assert.Equal(DiagnosticSeverity.Error, expected.Severity);
        var tree = compilation.SyntaxTrees.Single();
        Assert.Equal(expected, Assert.Single(compilation.GetDiagnostics(tree)));
        Assert.Equal(expected, Assert.Single(compilation.GetDocumentDiagnostics(tree)));
        Parallel.For(0, 4, _ => Assert.Equal(expected, Assert.Single(compilation.GetDiagnostics())));
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    public void EmitFailsWithoutTouchingEitherStreamEvenWithSuppliedDiagnostics(int suppliedDiagnostics)
    {
        var compilation = Create();
        using var pe = new MemoryStream();
        using var pdb = new MemoryStream();
        pe.Write([1, 2, 3]);
        pdb.Write([4, 5]);
        var result = suppliedDiagnostics switch
        {
            1 => compilation.Emit(pe, pdb, ImmutableArray<Diagnostic>.Empty),
            2 => compilation.Emit(pe, pdb, compilation.GetDiagnostics()),
            _ => compilation.Emit(pe, pdb)
        };
        Assert.False(result.Success);
        Assert.Equal("RAVT004", Assert.Single(result.Diagnostics).Id);
        Assert.Equal(new byte[] { 1, 2, 3 }, pe.ToArray());
        Assert.Equal(new byte[] { 4, 5 }, pdb.ToArray());
        Assert.Equal(3, pe.Position);
        Assert.Equal(2, pdb.Position);
    }

    [Fact]
    public void AddingReferencesToFailedSnapshotAllowsFreshSetup()
    {
        var failed = Create();
        Assert.Equal("RAVT004", Assert.Single(failed.GetDiagnostics()).Id);
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"))
            .Select(MetadataReference.CreateFromFile).ToArray();
        var corrected = failed.AddReferences(references);
        corrected.AdoptIncrementalReuseFrom(failed);
        Assert.DoesNotContain(corrected.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        using var pe = new MemoryStream();
        Assert.True(corrected.Emit(pe).Success);
        Assert.Equal("RAVT004", Assert.Single(failed.GetDiagnostics()).Id);
    }

    [Theory]
    [InlineData(ReportDiagnostic.Suppress)]
    [InlineData(ReportDiagnostic.Warn)]
    public void FatalSetupFailureCannotBeSuppressedOrDowngraded(ReportDiagnostic report)
    {
        var compilation = Create(CompilationOptions.DotNet.WithSpecificDiagnosticOption("RAVT004", report));
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
        Assert.False(diagnostic.IsSuppressed);
        using var pe = new MemoryStream();
        Assert.False(compilation.Emit(pe).Success);
        Assert.Equal(0, pe.Length);
    }

    [Fact]
    public void SyntaxDiagnosticsDoNotRequireTargetInitialization()
    {
        var compilation = Create(source: "class Example { public func M() -> {} }");
        var tree = compilation.SyntaxTrees.Single();
        var expected = tree.GetDiagnostics().ToArray();
        Assert.NotEmpty(expected);
        Assert.Equal(expected, compilation.GetSyntaxDiagnostics(tree));
    }

    [Fact]
    public void CancellationIsNotConvertedIntoTargetFailure()
    {
        var compilation = Create();
        var token = new CancellationToken(canceled: true);
        Assert.Throws<OperationCanceledException>(() => compilation.GetDiagnostics(cancellationToken: token));
    }
}
