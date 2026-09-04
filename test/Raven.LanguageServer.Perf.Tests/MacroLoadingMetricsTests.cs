using System.Diagnostics;
using System.Reflection;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

using Xunit.Abstractions;

namespace Raven.LanguageServer.Perf.Tests;

public sealed class MacroLoadingMetricsTests : IDisposable
{
    private const int RepetitionCount = 8;
    private readonly ITestOutputHelper _output;
    private readonly string _tempRoot = Path.Combine(Path.GetTempPath(), $"raven-macro-loading-metrics-{Guid.NewGuid():N}");
    private readonly List<string> _shadowDirectories = [];

    public MacroLoadingMetricsTests(ITestOutputHelper output)
    {
        _output = output;
    }

    [Fact]
    public void ProjectEvaluation_CachedRequestsReportWarmLatency()
    {
        var projectPath = WriteEvaluationLayout();

        // Register and JIT the evaluator before measuring either path.
        var warmup = new MsBuildProjectSystemService(RavenProjectConventions.Default, resolvePackageReferences: false);
        _ = warmup.GetProjectReferencePaths(projectPath);

        var uncachedSamples = new double[RepetitionCount];
        for (var i = 0; i < uncachedSamples.Length; i++)
        {
            var service = new MsBuildProjectSystemService(RavenProjectConventions.Default, resolvePackageReferences: false);
            uncachedSamples[i] = Measure(() => service.GetProjectReferencePaths(projectPath));
        }

        var instrumentation = new ProjectSystemPerformanceInstrumentation();
        var cachedService = new MsBuildProjectSystemService(
            RavenProjectConventions.Default,
            resolvePackageReferences: false,
            instrumentation);
        _ = cachedService.GetProjectReferencePaths(projectPath);
        var cachedSamples = Enumerable.Range(0, RepetitionCount)
            .Select(_ => Measure(() => cachedService.GetProjectReferencePaths(projectPath)))
            .ToArray();
        var snapshot = instrumentation.CaptureSnapshot();

        snapshot.EvaluationRequests.ShouldBe(RepetitionCount + 1);
        snapshot.Evaluations.ShouldBe(1);
        snapshot.EvaluationCacheHits.ShouldBe(RepetitionCount);
        snapshot.EvaluationFailures.ShouldBe(0);
        WriteComparison("projectEvaluation", uncachedSamples, cachedSamples);
    }

    [Fact]
    public void FileBackedMacroReferences_ReportSharedExportLatency()
    {
        Directory.CreateDirectory(_tempRoot);
        var sourceAssemblyPath = Path.Combine(AppContext.BaseDirectory, "Raven.Macros.dll");
        File.Exists(sourceAssemblyPath).ShouldBeTrue();

        var warmupPath = CopyMacroAssembly(sourceAssemblyPath, "warmup");
        MacroReference.CreateFromFile(warmupPath).Macros.ShouldNotBeEmpty();

        var retainedReferences = new List<MacroReference>();
        var uncachedSamples = new double[RepetitionCount];
        for (var i = 0; i < uncachedSamples.Length; i++)
        {
            var path = CopyMacroAssembly(sourceAssemblyPath, $"uncached-{i}");
            MacroReference? reference = null;
            uncachedSamples[i] = Measure(() =>
            {
                reference = MacroReference.CreateFromFile(path);
                reference.Macros.ShouldNotBeEmpty();
            });
            retainedReferences.Add(reference!);
        }

        var cachedPath = CopyMacroAssembly(sourceAssemblyPath, "cached");
        var seed = MacroReference.CreateFromFile(cachedPath);
        var seedAssembly = seed.Macros[0].GetType().Assembly;
        retainedReferences.Add(seed);
        var cachedSamples = new double[RepetitionCount];
        for (var i = 0; i < cachedSamples.Length; i++)
        {
            MacroReference? reference = null;
            cachedSamples[i] = Measure(() =>
            {
                reference = MacroReference.CreateFromFile(cachedPath);
                reference.Macros.ShouldNotBeEmpty();
                reference.Macros[0].GetType().Assembly.ShouldBeSameAs(seedAssembly);
            });
            retainedReferences.Add(reference!);
        }

        retainedReferences.Count.ShouldBe((RepetitionCount * 2) + 1);
        WriteComparison("fileMacroExports", uncachedSamples, cachedSamples);
    }

    [Fact]
    public async Task SourceMacroLifecycle_ReportsColdWarmEditRevertAndRestartLatencyAsync()
    {
        var layout = WriteSourceMacroLayout();
        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = CreateWorkspaceManager(workspace);
        var macroProject = GetProject(manager, layout.MacroProjectPath);
        var shadowDirectory = Invoke<string>("GetShadowMacroOutputDirectory", null, macroProject);
        _shadowDirectories.Add(shadowDirectory);
        if (Directory.Exists(shadowDirectory))
            Directory.Delete(shadowDirectory, recursive: true);

        var instrumentation = workspace.GetCompilation(macroProject.Id).PerformanceInstrumentation.Macros;
        instrumentation.Reset();
        string? coldPath = null;
        var coldMs = Measure(() => coldPath = Invoke<string>("EmitMacroProjectOutput", manager, macroProject));
        var warmSamples = Enumerable.Range(0, RepetitionCount)
            .Select(_ => Measure(() => Invoke<string>("EmitMacroProjectOutput", manager, macroProject).ShouldBe(coldPath)))
            .ToArray();

        instrumentation.ShadowOutputCacheMisses.ShouldBe(1);
        instrumentation.ShadowOutputCacheHits.ShouldBe(RepetitionCount);

        _ = await manager.UpsertDocumentAsync(layout.AppUri, File.ReadAllText(layout.AppPath));
        instrumentation.Reset();
        var consumerEditMs = await MeasureAsync(async () =>
        {
            _ = await manager.UpsertDocumentAsync(
                layout.AppUri,
                SourceText.From(File.ReadAllText(layout.AppPath) + Environment.NewLine),
                deferMacroConsumerRefresh: true);
            await manager.FlushPendingMacroConsumerRefreshesAsync();
        });
        instrumentation.ShadowOutputCacheHits.ShouldBe(0);
        instrumentation.ShadowOutputCacheMisses.ShouldBe(0);

        var macroEditMs = await MeasureAsync(async () =>
        {
            _ = await manager.UpsertDocumentAsync(
                layout.MacroUri,
                SourceText.From(CreateMacroSource("2")),
                deferMacroConsumerRefresh: true);
            await manager.FlushPendingMacroConsumerRefreshesAsync();
        });
        macroProject = GetProject(manager, layout.MacroProjectPath);
        instrumentation = workspace.GetCompilation(macroProject.Id).PerformanceInstrumentation.Macros;
        instrumentation.ShadowOutputCacheMisses.ShouldBe(1);

        instrumentation.Reset();
        var revertMs = await MeasureAsync(async () =>
        {
            _ = await manager.UpsertDocumentAsync(
                layout.MacroUri,
                SourceText.From(CreateMacroSource("1")),
                deferMacroConsumerRefresh: true);
            await manager.FlushPendingMacroConsumerRefreshesAsync();
        });
        macroProject = GetProject(manager, layout.MacroProjectPath);
        instrumentation = workspace.GetCompilation(macroProject.Id).PerformanceInstrumentation.Macros;
        instrumentation.ShadowOutputCacheHits.ShouldBe(1);
        instrumentation.ShadowOutputCacheMisses.ShouldBe(0);

        var restartedWorkspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var restartedManager = CreateWorkspaceManager(restartedWorkspace);
        var restartedProject = GetProject(restartedManager, layout.MacroProjectPath);
        var restartInstrumentation = restartedWorkspace.GetCompilation(restartedProject.Id).PerformanceInstrumentation.Macros;
        restartInstrumentation.Reset();
        var restartMs = Measure(
            () => Invoke<string>("EmitMacroProjectOutput", restartedManager, restartedProject).ShouldBe(coldPath));
        restartInstrumentation.ShadowOutputCacheHits.ShouldBe(1);
        restartInstrumentation.ShadowOutputCacheMisses.ShouldBe(0);

        var warmMedian = Median(warmSamples);
        _output.WriteLine(FormattableString.Invariant(
            $"sourceMacro coldMiss={coldMs:F2}ms warmHitMedian={warmMedian:F2}ms warmSpeedup={Ratio(coldMs, warmMedian):F1}x consumerEdit={consumerEditMs:F2}ms macroEdit={macroEditMs:F2}ms revertHit={revertMs:F2}ms restartHit={restartMs:F2}ms"));
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);

        foreach (var shadowDirectory in _shadowDirectories)
        {
            if (Directory.Exists(shadowDirectory))
                Directory.Delete(shadowDirectory, recursive: true);
        }
    }

    private string WriteEvaluationLayout()
    {
        var dependencyDirectory = Path.Combine(_tempRoot, "evaluation", "dependency");
        var appDirectory = Path.Combine(_tempRoot, "evaluation", "app");
        Directory.CreateDirectory(dependencyDirectory);
        Directory.CreateDirectory(appDirectory);
        File.WriteAllText(Path.Combine(dependencyDirectory, "Dependency.rvnproj"), ProjectText("Dependency"));
        var projectPath = Path.Combine(appDirectory, "App.rvnproj");
        File.WriteAllText(projectPath, ProjectText("App", "../dependency/Dependency.rvnproj"));
        return projectPath;
    }

    private SourceMacroLayout WriteSourceMacroLayout()
    {
        var id = Guid.NewGuid().ToString("N");
        var macroName = $"Macros{id}";
        var appDirectory = Path.Combine(_tempRoot, "lifecycle", "app");
        var macroDirectory = Path.Combine(_tempRoot, "lifecycle", "macros");
        Directory.CreateDirectory(appDirectory);
        Directory.CreateDirectory(macroDirectory);

        var macroProjectPath = Path.Combine(macroDirectory, $"{macroName}.rvnproj");
        File.WriteAllText(macroProjectPath, $$"""
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>{{macroName}}</AssemblyName>
    <OutputType>Library</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="main.rvn" />
    <Reference Include="Raven.CodeAnalysis">
      <HintPath>{{typeof(RavenWorkspace).Assembly.Location}}</HintPath>
    </Reference>
  </ItemGroup>
</Project>
""");
        var macroPath = Path.Combine(macroDirectory, "main.rvn");
        File.WriteAllText(macroPath, CreateMacroSource("1"));

        var appProjectPath = Path.Combine(appDirectory, $"App{id}.rvnproj");
        File.WriteAllText(appProjectPath, $$"""
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App{{id}}</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="main.rvn" />
    <ProjectReference Include="../macros/{{macroName}}.rvnproj" />
  </ItemGroup>
</Project>
""");
        var appPath = Path.Combine(appDirectory, "main.rvn");
        File.WriteAllText(appPath, "func Main() -> int => answer!()");

        return new SourceMacroLayout(
            macroProjectPath,
            macroPath,
            DocumentUri.FromFileSystemPath(macroPath),
            appPath,
            DocumentUri.FromFileSystemPath(appPath));
    }

    private WorkspaceManager CreateWorkspaceManager(RavenWorkspace workspace)
    {
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "macro-loading-metrics",
                Uri = DocumentUri.FromFileSystemPath(Path.Combine(_tempRoot, "lifecycle"))
            })
        });
        return manager;
    }

    private string CopyMacroAssembly(string sourcePath, string name)
    {
        var path = Path.Combine(_tempRoot, $"{name}-{Guid.NewGuid():N}.dll");
        File.Copy(sourcePath, path);
        return path;
    }

    private static Project GetProject(WorkspaceManager manager, string projectPath)
        => manager.GetProjectsSnapshot().Single(project =>
            string.Equals(project.FilePath, projectPath, StringComparison.OrdinalIgnoreCase));

    private static T Invoke<T>(string methodName, object? instance, params object[] arguments)
    {
        var flags = BindingFlags.NonPublic |
                    (instance is null ? BindingFlags.Static : BindingFlags.Instance);
        var method = typeof(WorkspaceManager).GetMethod(methodName, flags);
        method.ShouldNotBeNull();
        return (T)method.Invoke(instance, arguments)!;
    }

    private void WriteComparison(string operation, double[] uncachedSamples, double[] cachedSamples)
    {
        var uncachedMedian = Median(uncachedSamples);
        var cachedMedian = Median(cachedSamples);
        _output.WriteLine(FormattableString.Invariant(
            $"{operation} uncachedMedian={uncachedMedian:F2}ms cachedMedian={cachedMedian:F2}ms speedup={Ratio(uncachedMedian, cachedMedian):F1}x samples={RepetitionCount}"));
    }

    private static double Measure(Action action)
    {
        var stopwatch = Stopwatch.StartNew();
        action();
        stopwatch.Stop();
        return stopwatch.Elapsed.TotalMilliseconds;
    }

    private static async Task<double> MeasureAsync(Func<Task> action)
    {
        var stopwatch = Stopwatch.StartNew();
        await action();
        stopwatch.Stop();
        return stopwatch.Elapsed.TotalMilliseconds;
    }

    private static double Median(double[] values)
    {
        var ordered = values.Order().ToArray();
        var middle = ordered.Length / 2;
        return ordered.Length % 2 == 0
            ? (ordered[middle - 1] + ordered[middle]) / 2
            : ordered[middle];
    }

    private static double Ratio(double baseline, double optimized)
        => optimized <= 0 ? double.PositiveInfinity : baseline / optimized;

    private static string ProjectText(string assemblyName, string? projectReference = null)
        => $$"""
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>{{assemblyName}}</AssemblyName>
    <OutputType>Library</OutputType>
  </PropertyGroup>
  {{(projectReference is null ? string.Empty : $"<ItemGroup><ProjectReference Include=\"{projectReference}\" /></ItemGroup>")}}
</Project>
""";

    private static string CreateMacroSource(string expansionText)
        => $$"""
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Syntax.*
import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

[assembly: RavenCompilerPlugin(typeof(AnswerMacro))]

class AnswerMacro: IMacroDefinition {
    val Name: string => "answer"

    func Expand(context: FreestandingMacroContext) -> FreestandingMacroExpansionResult {
        FreestandingMacroExpansionResult.FromExpression(ParseExpression("{{expansionText}}"))
    }
}
""";

    private sealed record SourceMacroLayout(
        string MacroProjectPath,
        string MacroPath,
        DocumentUri MacroUri,
        string AppPath,
        DocumentUri AppUri);
}
