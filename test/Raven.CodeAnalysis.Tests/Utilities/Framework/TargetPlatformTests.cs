using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests;

public sealed class TargetPlatformTests
{
    private const TargetPlatform Unsupported = (TargetPlatform)123;

    [Fact]
    public async Task CompilerDriverReportsInvalidProjectPlatformWithoutWritingOutput()
    {
        var repository = new DirectoryInfo(AppContext.BaseDirectory);
        while (repository is not null && !File.Exists(Path.Combine(repository.FullName, "Raven.sln")))
            repository = repository.Parent;
        Assert.NotNull(repository);
        var configuration = new DirectoryInfo(AppContext.BaseDirectory).Parent!.Name;
        var compiler = Path.Combine(repository!.FullName, "src/Raven.Compiler/bin", configuration, "net11.0/rvnc.dll");
        Assert.True(File.Exists(compiler), "Build the repository compiler before running driver tests.");
        var directory = Path.Combine(Path.GetTempPath(), "raven-platform-driver-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var project = Path.Combine(directory, "App.rvnproj");
            File.WriteAllText(project,
                "<Project><PropertyGroup><RavenTargetPlatform>UnknownPlatform</RavenTargetPlatform></PropertyGroup></Project>");
            var outputDirectory = Path.Combine(directory, "output");
            Directory.CreateDirectory(outputDirectory);
            var output = Path.Combine(outputDirectory, "App.dll");
            File.WriteAllBytes(output, [1, 2, 3]);
            var info = new System.Diagnostics.ProcessStartInfo("dotnet")
            {
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                WorkingDirectory = directory
            };
            foreach (var argument in new[] { compiler, project, "--framework", "net11.0", "--no-project-restore", "-o", outputDirectory })
                info.ArgumentList.Add(argument);
            using var process = System.Diagnostics.Process.Start(info)!;
            var stdout = process.StandardOutput.ReadToEndAsync();
            var stderr = process.StandardError.ReadToEndAsync();
            using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(60));
            try { await process.WaitForExitAsync(timeout.Token); }
            catch (OperationCanceledException) { process.Kill(entireProcessTree: true); throw; }
            var message = await stdout + await stderr;
            Assert.Equal(1, process.ExitCode);
            Assert.Contains("Cannot load project", message);
            Assert.Contains("RavenTargetPlatform 'UnknownPlatform'", message);
            Assert.DoesNotContain("Unhandled exception", message);
            Assert.Equal(new byte[] { 1, 2, 3 }, File.ReadAllBytes(output));
            Assert.Single(Directory.GetFiles(outputDirectory));
        }
        finally { Directory.Delete(directory, recursive: true); }
    }

    [Fact]
    public void DefaultsPreserveDotNetAndReferencePolicy()
    {
        Assert.Equal(TargetPlatform.DotNet, new CompilationOptions().TargetPlatform);
        Assert.Equal(TargetPlatform.DotNet, CompilationOptions.DotNet.TargetPlatform);
        Assert.Null(new CompilationOptions().MetadataImportOptions);
        Assert.NotNull(CompilationOptions.DotNet.MetadataImportOptions);
        Assert.Null(CompilationOptions.DotNet.MetadataImportOptions!.CoreAssemblyName);
    }

    [Fact]
    public void OptionCopiesPreserveExplicitPlatformAndContracts()
    {
        var original = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, targetPlatform: Unsupported)
            .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
            .WithTargetCoreAssemblyName("System.Runtime")
            .WithAllowNullableValueTypes(false)
            .WithAllowArrayCovariance(false);
        var copies = new[]
        {
            original.WithOutputKind(OutputKind.ConsoleApplication),
            original.WithOptimizationLevel(OptimizationLevel.Release),
            original.WithRunAnalyzers(false),
            original.WithAllowUnsafe(true),
            original.WithRuntimeUnitContract(new("System.Runtime", "System.ValueTuple")),
            original.WithRuntimeTypeOfContract(null),
            original.WithRuntimeIterationContract(null),
            original.WithRuntimePropagationContract(null),
            original.WithUnicodeScalarChar(true),
            original.WithGraphemeChar(true),
            original.WithHeapAsyncStateMachines(true),
            original.WithAsyncExceptionCapture(false),
            original.WithAsyncCancellationPropagation(true),
            original.WithSpecificDiagnosticOption("RAVT005", ReportDiagnostic.Suppress),
        };
        foreach (var copy in copies)
        {
            Assert.Equal(Unsupported, copy.TargetPlatform);
            Assert.Same(original.MetadataImportOptions, copy.MetadataImportOptions);
            Assert.Equal(original.TargetCoreAssemblyName, copy.TargetCoreAssemblyName);
            Assert.False(copy.AllowNullableValueTypes);
            Assert.False(copy.AllowArrayCovariance);
        }
        var changed = original.WithTargetPlatform(TargetPlatform.DotNet);
        Assert.Equal(TargetPlatform.DotNet, changed.TargetPlatform);
        Assert.Equal(Unsupported, original.TargetPlatform);
        Assert.Same(original.MetadataImportOptions, changed.MetadataImportOptions);
        Assert.Equal(original.TargetCoreAssemblyName, changed.TargetCoreAssemblyName);
        Assert.False(changed.AllowNullableValueTypes);
        Assert.False(changed.AllowArrayCovariance);
    }

    [Theory]
    [InlineData(ReportDiagnostic.Default)]
    [InlineData(ReportDiagnostic.Suppress)]
    [InlineData(ReportDiagnostic.Warn)]
    public void UnsupportedPlatformPrecedesReferenceLoadingAndCannotWriteOutput(ReportDiagnostic report)
    {
        var options = CompilationOptions.DotNet.WithTargetPlatform(Unsupported)
            .WithSpecificDiagnosticOption("RAVT005", report);
        var tree = SyntaxTree.ParseText("class Example {}");
        var compilation = Compilation.Create("InvalidPlatform", [tree], [], options);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal("RAVT005", diagnostic.Id);
        Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
        Assert.False(diagnostic.IsSuppressed);
        Assert.Equal(diagnostic, Assert.Single(compilation.GetDiagnostics(tree)));
        Assert.Equal(diagnostic, Assert.Single(compilation.GetDocumentDiagnostics(tree)));
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

    [Fact]
    public void WorkspacePlatformChangeRejectsThenRecoversWithoutStaleDiagnostics()
    {
        var options = CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary);
        var workspace = new AdhocWorkspace();
        var project = workspace.CurrentSolution.AddProject("test", compilationOptions: options).Projects.Single();
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        var document = project.AddDocument("main.rvn", SourceText.From("class Example {}"));
        Assert.True(workspace.TryApplyChanges(document.Project.Solution));
        var original = workspace.GetCompilation(project.Id);
        Assert.Empty(original.GetDiagnostics());
        Assert.NotNull(original.GetTypeByMetadataName("Example"));

        Assert.True(workspace.TryApplyChanges(workspace.CurrentSolution.WithCompilationOptions(
            project.Id, options.WithTargetPlatform(Unsupported))));
        var invalid = workspace.GetCompilation(project.Id);
        Assert.Equal("RAVT005", Assert.Single(invalid.GetDiagnostics()).Id);

        Assert.True(workspace.TryApplyChanges(workspace.CurrentSolution.WithCompilationOptions(project.Id, options)));
        var restored = workspace.GetCompilation(project.Id);
        Assert.NotSame(original, restored);
        Assert.Empty(restored.GetDiagnostics());
        Assert.NotNull(restored.GetTypeByMetadataName("Example"));
        using var output = new MemoryStream();
        Assert.True(restored.Emit(output).Success);
        Assert.Equal("RAVT005", Assert.Single(invalid.GetDiagnostics()).Id);
        Assert.Empty(original.GetDiagnostics());
    }
}
