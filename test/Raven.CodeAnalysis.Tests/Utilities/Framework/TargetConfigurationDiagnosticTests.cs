using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TargetConfigurationDiagnosticTests
{
    private static CompilationOptions NamedCore => new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
        .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
        .WithTargetCoreAssemblyName("System.Runtime");

    public static IEnumerable<object[]> InvalidConfigurations()
    {
        yield return [NamedCore.WithTargetCoreAssemblyName("Other.Core"), "metadata core"];
        yield return [new CompilationOptions().WithTargetCoreAssemblyName("System.Runtime"), "metadata core"];
        yield return [CompilationOptions.DotNet.WithTargetCoreAssemblyName(" "), "metadata core"];
        yield return [CompilationOptions.DotNet.WithRuntimeUnitContract(new("System.Runtime", "System.ValueTuple")), "unit contract"];
        yield return [NamedCore.WithRuntimeUnitContract(new("Other.Core", "System.ValueTuple")), "unit contract"];
        yield return [NamedCore.WithRuntimeUnitContract(new("System.Runtime", " ")), "unit contract"];
        yield return [CompilationOptions.DotNet.WithRuntimeTypeOfContract(new("", "Contracts.Info", "Contracts.Context")), "typeof contract"];
        yield return [CompilationOptions.DotNet.WithRuntimeTypeOfContract(new("Provider", " ", "Contracts.Context")), "typeof contract"];
        yield return [CompilationOptions.DotNet.WithRuntimeTypeOfContract(new("Provider", "Contracts.Info", "")), "typeof contract"];
    }

    [Theory]
    [MemberData(nameof(InvalidConfigurations))]
    public void ConfigurationErrorsPrecedeMissingCoreAndBlockEmission(CompilationOptions options, string message)
    {
        var tree = SyntaxTree.ParseText("class Example {}");
        var compilation = Compilation.Create("InvalidOptions", [tree], [], options);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal("RAVT003", diagnostic.Id);
        Assert.Contains(message, diagnostic.GetMessage());
        Assert.Equal(diagnostic, Assert.Single(compilation.GetDiagnostics(tree)));
        Assert.Equal(diagnostic, Assert.Single(compilation.GetDocumentDiagnostics(tree)));
        Assert.Empty(compilation.GetSyntaxDiagnostics(tree));

        using var pe = new MemoryStream();
        using var pdb = new MemoryStream();
        pe.Write([1, 2]);
        pdb.Write([3]);
        var result = compilation.Emit(pe, pdb, ImmutableArray<Diagnostic>.Empty);
        Assert.False(result.Success);
        Assert.Equal(diagnostic, Assert.Single(result.Diagnostics));
        Assert.Equal(new byte[] { 1, 2 }, pe.ToArray());
        Assert.Equal(new byte[] { 3 }, pdb.ToArray());
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void ResolvedContractErrorsPreserveStreamsAndSuppliedDiagnostics(bool typeOfContract, bool suppliedDiagnostics)
    {
        var options = typeOfContract
            ? NamedCore.WithRuntimeTypeOfContract(new("MissingProvider", "Contracts.Info", "Contracts.Context"))
            : NamedCore.WithRuntimeUnitContract(new("System.Runtime", "System.Int32"));
        var compilation = Compilation.Create("InvalidResolvedContract", [SyntaxTree.ParseText("class Example {}")],
            TestMetadataReferences.Default, options);
        var warning = Diagnostic.Create(DiagnosticDescriptor.Create(
            "TEST001", "Test warning", "", "", "Semantic warning", "test", DiagnosticSeverity.Warning, true), Location.None);
        using var output = new MemoryStream();
        using var debugOutput = new MemoryStream();
        output.Write([1, 2, 3]);
        debugOutput.Write([4, 5]);
        output.Position = 1;
        debugOutput.Position = 0;

        var result = compilation.Emit(output, debugOutput,
            diagnostics: suppliedDiagnostics ? ImmutableArray.Create(warning) : null);

        Assert.False(result.Success);
        var error = Assert.Single(result.Diagnostics.Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error));
        Assert.Equal("RAVT003", error.Id);
        Assert.Contains(typeOfContract ? "typeof contract" : "empty non-void value type", error.GetMessage());
        Assert.Equal(suppliedDiagnostics ? 2 : 1, result.Diagnostics.Length);
        if (suppliedDiagnostics)
            Assert.Same(warning, result.Diagnostics[0]);
        Assert.Equal(new byte[] { 1, 2, 3 }, output.ToArray());
        Assert.Equal(new byte[] { 4, 5 }, debugOutput.ToArray());
        Assert.Equal(1, output.Position);
        Assert.Equal(0, debugOutput.Position);
        Assert.True(output.CanWrite);
        Assert.True(debugOutput.CanWrite);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ConsistentCoreConfigurationStillRequiresSuppliedReferences(bool discovered)
    {
        var options = discovered ? CompilationOptions.DotNet.WithTargetCoreAssemblyName("System.Runtime") : NamedCore;
        var compilation = Compilation.Create("MissingReferences", [], [], options);
        Assert.Equal("RAVT004", Assert.Single(compilation.GetDiagnostics()).Id);
    }

    [Theory]
    [InlineData(ReportDiagnostic.Suppress)]
    [InlineData(ReportDiagnostic.Warn)]
    public void ContradictoryConfigurationCannotBeSuppressedToProceed(ReportDiagnostic report)
    {
        var options = NamedCore.WithTargetCoreAssemblyName("Other.Core")
            .WithSpecificDiagnosticOption("RAVT003", report);
        var compilation = Compilation.Create("InvalidOptions", [], [], options);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal("RAVT003", diagnostic.Id);
        Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
        Assert.False(diagnostic.IsSuppressed);
        using var pe = new MemoryStream();
        Assert.False(compilation.Emit(pe).Success);
        Assert.Equal(0, pe.Length);
    }
}
