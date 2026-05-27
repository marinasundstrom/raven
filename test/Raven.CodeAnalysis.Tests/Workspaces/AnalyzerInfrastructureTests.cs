using System.Threading;

using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public class AnalyzerInfrastructureTests
{
    private sealed class CollectingWorkspaceEventSink : IWorkspaceEventSink
    {
        public List<WorkspaceEvent> Events { get; } = [];

        public void Report(WorkspaceEvent workspaceEvent)
            => Events.Add(workspaceEvent);
    }

    private sealed class ReservedPrefixAnalyzer : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "RAV9999",
            title: "Reserved prefix",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "Reserved prefix",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxTreeAction(ctx =>
            {
                ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, Location.None));
            });
        }
    }

    private sealed class TodoAnalyzer : DiagnosticAnalyzer
    {
        public static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "AN0001",
            title: "TODO found",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "TODO found",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxTreeAction(ctx =>
            {
                var text = ctx.SyntaxTree.GetText()?.ToString();
                if (text is not null && text.Contains("TODO"))
                    ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, Location.None));
            });
        }
    }

    private sealed class CountingAnalyzer : DiagnosticAnalyzer
    {
        public static int AnalyzeCount;

        private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "AN0003",
            title: "Counted",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "Counted",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxTreeAction(ctx =>
            {
                Interlocked.Increment(ref AnalyzeCount);
                ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, Location.None));
            });
        }
    }

    private sealed class ConcurrentCountingAnalyzer : DiagnosticAnalyzer
    {
        public static int AnalyzeCount;

        private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "AN0005",
            title: "Concurrent counted",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "Concurrent counted",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info);

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();

            context.RegisterSyntaxTreeAction(ctx =>
            {
                Interlocked.Increment(ref AnalyzeCount);
                ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, Location.None));
            });
        }
    }

    private abstract class ConcurrentSyntaxNodeAnalyzerBase : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "AN0006",
            title: "Concurrent syntax node",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "Concurrent syntax node",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info);

        public static int ActiveActions;
        public static int MaxActiveActions;

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();

            context.RegisterSyntaxNodeAction(ctx =>
            {
                var active = Interlocked.Increment(ref ActiveActions);
                try
                {
                    UpdateMaxActiveActions(active);
                    _ = ctx.SemanticModel.GetDeclaredSymbol(ctx.Node);
                    Thread.Sleep(25);
                    ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, ctx.Node.GetLocation()));
                }
                finally
                {
                    Interlocked.Decrement(ref ActiveActions);
                }
            }, SyntaxKind.CompilationUnit);
        }

        private static void UpdateMaxActiveActions(int active)
        {
            int current;
            do
            {
                current = Volatile.Read(ref MaxActiveActions);
                if (active <= current)
                    return;
            } while (Interlocked.CompareExchange(ref MaxActiveActions, active, current) != current);
        }
    }

    private sealed class ConcurrentSyntaxNodeAnalyzerA : ConcurrentSyntaxNodeAnalyzerBase
    {
    }

    private sealed class ConcurrentSyntaxNodeAnalyzerB : ConcurrentSyntaxNodeAnalyzerBase
    {
    }

    private sealed class NodeKindAnalyzer : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "AN0002",
            title: "Node match",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "Node kind match",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(
                ctx => ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, ctx.Node.GetLocation())),
                SyntaxKind.MethodDeclaration);
        }
    }

    private sealed class NodeScopedAnalyzer : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "AN0004",
            title: "Node scoped",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "Node scoped",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(
                ctx => ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, ctx.Node.GetLocation())),
                SyntaxNodeAnalysisScope.Node,
                SyntaxKind.MethodDeclaration);
        }
    }

    private sealed class MethodSymbolAnalyzer : DiagnosticAnalyzer
    {
        public static int AnalyzeCount;

        private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "AN0007",
            title: "Method symbol",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "Method symbol",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info);

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();

            context.RegisterSymbolAction(ctx =>
            {
                Interlocked.Increment(ref AnalyzeCount);
                ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, ctx.Symbol.Locations.First()));
            }, SymbolKind.Method);
        }
    }

    private sealed class BlockingMethodSymbolAnalyzer : DiagnosticAnalyzer
    {
        public static readonly ManualResetEventSlim Entered = new(false);
        public static readonly ManualResetEventSlim Release = new(false);

        private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
            id: "AN0008",
            title: "Blocking method symbol",
            description: null,
            helpLinkUri: string.Empty,
            messageFormat: "Blocking method symbol",
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Info);

        public static void Reset()
        {
            Entered.Reset();
            Release.Reset();
        }

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();

            context.RegisterSymbolAction(ctx =>
            {
                Entered.Set();
                Release.Wait(ctx.CancellationToken);
                ctx.ReportDiagnostic(Diagnostic.Create(Descriptor, ctx.Symbol.Locations.First()));
            }, SymbolKind.Method);
        }
    }

    [Fact]
    public void GetDiagnostics_IncludesCompilerAndAnalyzerDiagnostics()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var initial = SourceText.From("\"unterminated");
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rvn", initial);
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new TodoAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var diagnostics1 = workspace.GetDiagnostics(projectId);
        Assert.Contains(diagnostics1, d => d.Descriptor.Id == "RAV1010");
        Assert.DoesNotContain(diagnostics1, d => d.Descriptor.Id == TodoAnalyzer.Descriptor.Id);

        var updated = workspace.CurrentSolution.WithDocumentText(docId, SourceText.From("TODO \"unterminated"));
        workspace.TryApplyChanges(updated);

        var diagnostics2 = workspace.GetDiagnostics(projectId);
        Assert.Contains(diagnostics2, d => d.Descriptor.Id == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id);
        Assert.Contains(diagnostics2, d => d.Descriptor.Id == TodoAnalyzer.Descriptor.Id);
    }

    [Fact]
    public void GetDiagnostics_ReusesAnalyzerDiagnosticsUntilProjectVersionChanges()
    {
        CountingAnalyzer.AnalyzeCount = 0;

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From("val x = 1"));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new CountingAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        _ = workspace.GetDiagnostics(projectId);
        _ = workspace.GetDiagnostics(projectId);

        Assert.Equal(1, CountingAnalyzer.AnalyzeCount);

        var updated = workspace.CurrentSolution.WithDocumentText(docId, SourceText.From("val x = 2"));
        workspace.TryApplyChanges(updated);
        _ = workspace.GetDiagnostics(projectId);

        Assert.Equal(2, CountingAnalyzer.AnalyzeCount);
    }

    [Fact]
    public void GetDocumentAnalyzerDiagnostics_ReusesAnalyzerDiagnosticsForUnchangedSnapshot()
    {
        CountingAnalyzer.AnalyzeCount = 0;

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From("val x = 1"));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new CountingAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        _ = workspace.GetDocumentAnalyzerDiagnostics(projectId, docId);
        _ = workspace.GetDocumentAnalyzerDiagnostics(projectId, docId);

        Assert.Equal(1, CountingAnalyzer.AnalyzeCount);

        var updated = workspace.CurrentSolution.WithDocumentText(docId, SourceText.From("val x = 2"));
        workspace.TryApplyChanges(updated);
        _ = workspace.GetDocumentAnalyzerDiagnostics(projectId, docId);

        Assert.Equal(2, CountingAnalyzer.AnalyzeCount);
    }

    [Fact]
    public void GetDocumentAnalyzerDiagnostics_WithExistingCompilation_ReusesAnalyzerDiagnosticsForUnchangedSnapshot()
    {
        CountingAnalyzer.AnalyzeCount = 0;

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From("val x = 1"));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new CountingAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var document = workspace.CurrentSolution.GetDocument(docId)!;
        var compilation = workspace.CreateAnalysisCompilation(projectId);

        _ = workspace.GetDocumentAnalyzerDiagnostics(document, compilation);
        _ = workspace.GetDocumentAnalyzerDiagnostics(document, compilation);

        Assert.Equal(1, CountingAnalyzer.AnalyzeCount);
    }

    [Fact]
    public void GetDiagnostics_ExternalAnalyzerWithReservedRavPrefix_Throws()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From("TODO"));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new ReservedPrefixAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        Should.Throw<InvalidOperationException>(() => workspace.GetDiagnostics(projectId))
            .Message.ShouldContain("reserved 'RAV' prefix");
    }

    [Fact]
    public void GetDiagnostics_SyntaxNodeAction_OnlyRunsForRegisteredKinds()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var code = """
class C {
    public func M() -> unit { }
}

func F() -> unit { }
""";
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new NodeKindAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var diagnostics = workspace.GetDiagnostics(projectId)
            .Where(d => d.Descriptor.Id == "AN0002")
            .ToList();

        diagnostics.Count.ShouldBe(1);
    }

    [Fact]
    public void GetDiagnostics_SyntaxNodeActionPlan_ReportsInvalidationScopeCounts()
    {
        var eventSink = new CollectingWorkspaceEventSink();
        var workspace = RavenWorkspace.Create(
            targetFramework: TestMetadataReferences.TargetFramework,
            workspaceEventSink: eventSink);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var code = """
class C {
    public func M() -> unit { }
}
""";
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new NodeKindAnalyzer()));
        project = project.AddAnalyzerReference(new AnalyzerReference(new NodeScopedAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        _ = workspace.GetDocumentAnalyzerDiagnostics(projectId, docId);

        var actionPlan = eventSink.Events.Single(e => e.Operation == "documentAnalyzer.actionPlan");
        actionPlan.Detail.ShouldContain("documentScopedSyntaxNodeActions=1");
        actionPlan.Detail.ShouldContain("nodeScopedSyntaxNodeActions=1");
    }

    [Fact]
    public void GetDocumentAnalyzerDiagnostics_SymbolAction_RunsForDeclaredSymbolsInDocument()
    {
        MethodSymbolAnalyzer.AnalyzeCount = 0;

        var eventSink = new CollectingWorkspaceEventSink();
        var workspace = RavenWorkspace.Create(
            targetFramework: TestMetadataReferences.TargetFramework,
            workspaceEventSink: eventSink);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var code = """
class C {
    public func M() -> unit { }
}

func F() -> unit { }
""";
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new MethodSymbolAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var diagnostics = workspace.GetDocumentAnalyzerDiagnostics(projectId, docId)
            .Where(d => d.Descriptor.Id == "AN0007")
            .ToList();

        diagnostics.Count.ShouldBe(2);
        MethodSymbolAnalyzer.AnalyzeCount.ShouldBe(2);

        var actionPlan = eventSink.Events.Single(e => e.Operation == "documentAnalyzer.actionPlan");
        actionPlan.Detail.ShouldContain("symbolActions=1");
        actionPlan.Detail.ShouldContain("symbolKinds=1");
        eventSink.Events.Single(e => e.Operation == "documentAnalyzer.symbolEnumeration")
            .Detail.ShouldContain("symbols=2");
    }

    [Fact]
    public async Task GetDocumentAnalyzerDiagnostics_ReleasesSemanticGateBeforeSymbolActionsRun()
    {
        BlockingMethodSymbolAnalyzer.Reset();

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var code = """
class C {
    public func M() -> unit { }
}
""";
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new BlockingMethodSymbolAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var syntaxTree = compilation.SyntaxTrees.Single();
        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        var diagnosticsTask = Task.Run(() => workspace.GetDocumentAnalyzerDiagnostics(projectId, docId));

        try
        {
            BlockingMethodSymbolAnalyzer.Entered.Wait(TimeSpan.FromSeconds(5)).ShouldBeTrue();

            var lease = await semanticModel.TryEnterSemanticAccessAsync(CancellationToken.None);
            lease.ShouldNotBeNull();
            lease.Dispose();
        }
        finally
        {
            BlockingMethodSymbolAnalyzer.Release.Set();
        }

        var diagnostics = await diagnosticsTask;
        diagnostics.Count(diagnostic => diagnostic.Id == "AN0008").ShouldBe(1);
    }

    [Fact]
    public void SemanticModel_PublicQueries_AreReentrantUnderSemanticAccess()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var code = """
class C {
    public func M() -> unit {
        val value = 42
        _ = value
    }
}
""";
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var syntaxTree = compilation.SyntaxTrees.Single();
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var identifier = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(identifier => identifier.Identifier.ValueText == "value");

        using var lease = semanticModel.EnterSemanticAccess(CancellationToken.None);

        var symbolInfo = semanticModel.GetSymbolInfo(identifier);

        symbolInfo.Symbol.ShouldNotBeNull();
        symbolInfo.Symbol.Name.ShouldBe("value");
    }

    [Fact]
    public void GetDiagnostics_ActionPlan_ReportsConcurrentAnalyzers()
    {
        ConcurrentCountingAnalyzer.AnalyzeCount = 0;
        var eventSink = new CollectingWorkspaceEventSink();
        var workspace = RavenWorkspace.Create(
            targetFramework: TestMetadataReferences.TargetFramework,
            workspaceEventSink: eventSink);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From("val x = 1"));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new CountingAnalyzer()));
        project = project.AddAnalyzerReference(new AnalyzerReference(new ConcurrentCountingAnalyzer()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        _ = workspace.GetDocumentAnalyzerDiagnostics(projectId, docId);

        var actionPlan = eventSink.Events.Single(e => e.Operation == "documentAnalyzer.actionPlan");
        actionPlan.Detail.ShouldContain("analyzers=2");
        actionPlan.Detail.ShouldContain("concurrentAnalyzers=1");
        Assert.Equal(1, ConcurrentCountingAnalyzer.AnalyzeCount);
    }

    [Fact]
    public void GetDocumentAnalyzerDiagnostics_SerializesConcurrentSyntaxNodeSemanticActions()
    {
        ConcurrentSyntaxNodeAnalyzerBase.ActiveActions = 0;
        ConcurrentSyntaxNodeAnalyzerBase.MaxActiveActions = 0;

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var docId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rvn", SourceText.From("""
class C {
    public func M() -> unit { }
}
"""));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new ConcurrentSyntaxNodeAnalyzerA()));
        project = project.AddAnalyzerReference(new AnalyzerReference(new ConcurrentSyntaxNodeAnalyzerB()));
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        _ = workspace.GetDocumentAnalyzerDiagnostics(projectId, docId);

        Assert.Equal(1, ConcurrentSyntaxNodeAnalyzerBase.MaxActiveActions);
    }

    [Fact]
    public void AddBuiltInAnalyzers_WhenCalledMultipleTimes_DoesNotDuplicateReferences()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var solutionWithProject = workspace.CurrentSolution.AddProject("Test");
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddBuiltInAnalyzers(enableSuggestions: true);
        workspace.TryApplyChanges(project.Solution);
        var countAfterFirstCall = workspace.CurrentSolution.GetProject(projectId)!.AnalyzerReferences.Count;

        project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddBuiltInAnalyzers(enableSuggestions: true);
        workspace.TryApplyChanges(project.Solution);
        var countAfterSecondCall = workspace.CurrentSolution.GetProject(projectId)!.AnalyzerReferences.Count;

        Assert.Equal(countAfterFirstCall, countAfterSecondCall);
    }

    [Fact]
    public void AddBuiltInAnalyzers_RespectsDisabledAnalyzerNames()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithDisabledAnalyzers(["UnusedVariableAnalyzer"]);
        var solutionWithProject = workspace.CurrentSolution.AddProject(
            "Test",
            compilationOptions: options);
        var projectId = solutionWithProject.Projects.Single().Id;
        workspace.TryApplyChanges(solutionWithProject);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddBuiltInAnalyzers(enableSuggestions: true);

        var analyzers = project.AnalyzerReferences.SelectMany(static reference => reference.GetAnalyzers()).ToArray();
        analyzers.ShouldNotContain(static analyzer => analyzer is UnusedVariableAnalyzer);
        analyzers.ShouldNotContain(static analyzer => analyzer is UnusedLocalAnalyzer);
        analyzers.ShouldNotContain(static analyzer => analyzer is UnusedParameterAnalyzer);
        analyzers.ShouldContain(static analyzer => analyzer is VarCanBeValAnalyzer);
    }
}
