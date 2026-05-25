using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class ProjectFileNuGetReferenceTests
{
    [Fact]
    public void OpenProject_PackageReference_ResolvesFromGlobalCache()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var globalPackages = Path.Combine(root, "packages");
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(globalPackages);
        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var packageAssemblyPath = Path.Combine(
            globalPackages,
            "fake.package",
            "1.0.0",
            "ref",
            TestMetadataReferences.TargetFramework,
            "Fake.Package.dll");
        Directory.CreateDirectory(Path.GetDirectoryName(packageAssemblyPath)!);
        File.Copy(typeof(object).Assembly.Location, packageAssemblyPath, overwrite: true);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(sourcePath, "System.Console.WriteLine(\"hi\")");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <PackageReference Include="Fake.Package" Version="1.0.0" />
            </Project>
            """);

        var originalPackages = Environment.GetEnvironmentVariable("NUGET_PACKAGES");
        Environment.SetEnvironmentVariable("NUGET_PACKAGES", globalPackages);
        try
        {
            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;
            Assert.Contains(project.Documents, static d => string.Equals(d.Name, "main.rvn", StringComparison.OrdinalIgnoreCase));

            Assert.Contains(
                project.MetadataReferences.OfType<PortableExecutableReference>(),
                reference => string.Equals(reference.FilePath, packageAssemblyPath, StringComparison.OrdinalIgnoreCase));
        }
        finally
        {
            Environment.SetEnvironmentVariable("NUGET_PACKAGES", originalPackages);
        }
    }

    [Fact]
    public void OpenProject_FrameworkReference_ResolvesFromInstalledPacks()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(sourcePath, "System.Console.WriteLine(\"hi\")");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        Assert.Contains(
            project.MetadataReferences.OfType<PortableExecutableReference>(),
            reference => reference.FilePath.Contains("Microsoft.AspNetCore", StringComparison.OrdinalIgnoreCase));
    }

    [Fact]
    public void OpenProject_FrameworkReference_AllowsMapGetWithParameterlessLambda()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(
            sourcePath,
            """
            import Microsoft.AspNetCore.Builder.*

            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()
            app.MapGet("/", () => "Hello from Raven Minimal API")
            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var diagnostics = workspace.GetDiagnostics(projectId);

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV1501");
    }


    [Fact]
    public void OpenProject_FrameworkReference_AllowsMapGetWithLambdaWithParameters()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(
            sourcePath,
            """
            import Microsoft.AspNetCore.Builder.*

            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()
            app.MapGet("/", (name: string) => "Hello ${name} from Raven Minimal API")
            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var diagnostics = workspace.GetDiagnostics(projectId);

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV1503");
    }

    [Fact]
    public void OpenProject_FrameworkReference_MapGetAvailableInvocationCandidates_ReusesBoundSymbolWithoutBinding()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(
            sourcePath,
            """
            import Microsoft.AspNetCore.Builder.*

            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()
            app.MapGet("/", () => "Hello from Raven Minimal API")
            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        workspace.TryApplyChanges(workspace.CurrentSolution.WithCompilationOptions(
            projectId,
            project.CompilationOptions!.WithPerformanceInstrumentation(instrumentation)));
        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation =>
                invocation.Expression is MemberAccessExpressionSyntax { Name: IdentifierNameSyntax { Identifier.ValueText: "MapGet" } });

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.Equal("MapGet", boundInvocation.Method.Name);

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetAvailableInvocationCandidates(invocation, out var candidates));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.Contains(candidates, method => SymbolEqualityComparer.Default.Equals(method, boundInvocation.Method));
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void OpenProject_FrameworkReference_MapGetAvailableInvocationCandidates_ColdLookupDoesNotBindInvocation()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(
            sourcePath,
            """
            import Microsoft.AspNetCore.Builder.*

            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()
            app.MapGet("/", () => "Hello from Raven Minimal API")
            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        workspace.TryApplyChanges(workspace.CurrentSolution.WithCompilationOptions(
            projectId,
            project.CompilationOptions!.WithPerformanceInstrumentation(instrumentation)));
        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation =>
                invocation.Expression is MemberAccessExpressionSyntax { Name: IdentifierNameSyntax { Identifier.ValueText: "MapGet" } });

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetAvailableInvocationCandidates(invocation, out var candidates));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.Contains(candidates, static method => method.Name == "MapGet");
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void OpenProject_FrameworkReference_MapGetInvocationTargetSymbolInfo_ColdLookupDoesNotBindInvocation()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(
            sourcePath,
            """
            import Microsoft.AspNetCore.Builder.*

            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()
            app.MapGet("/", () => "Hello from Raven Minimal API")
            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        workspace.TryApplyChanges(workspace.CurrentSolution.WithCompilationOptions(
            projectId,
            project.CompilationOptions!.WithPerformanceInstrumentation(instrumentation)));
        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation =>
                invocation.Expression is MemberAccessExpressionSyntax { Name: IdentifierNameSyntax { Identifier.ValueText: "MapGet" } });

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetInvocationTargetSymbolInfo(invocation, out var info));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        var method = Assert.IsAssignableFrom<IMethodSymbol>(info.Symbol);
        Assert.Equal("MapGet", method.Name);
        Assert.Equal("Delegate", method.Parameters.Last().Type.Name);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void OpenProject_EfCoreSample_GenericExtensionLambdaCandidates_DoNotBindColdBodies()
    {
        var repoRoot = FindRepositoryRoot();
        var projectPath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "VehicleCostsApi.rvnproj");
        var sourcePath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "src", "Api", "Main.rvn");

        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        workspace.TryApplyChanges(workspace.CurrentSolution.WithCompilationOptions(
            projectId,
            project.CompilationOptions!.WithPerformanceInstrumentation(instrumentation)));

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var semanticModelSetupBefore = compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
        var model = compilation.GetSemanticModel(tree);
        var semanticModelSetupDelta = CompilerSetupInstrumentation.Subtract(
            compilation.PerformanceInstrumentation.Setup.CaptureSnapshot(),
            semanticModelSetupBefore);
        Assert.Equal(0, semanticModelSetupDelta.EnsureSourceDeclarationsDeclaredCalls);
        Assert.Equal(0, semanticModelSetupDelta.DeclarationPasses);
        var root = tree.GetRoot();
        var addDbContext = FindMemberInvocation(root, "AddDbContext");
        var useNpgsql = FindMemberInvocation(root, "UseNpgsql");
        var getRequiredService = FindMemberInvocation(root, "GetRequiredService");

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetAvailableInvocationCandidates(addDbContext, out var dbContextCandidates));
        Assert.Contains(dbContextCandidates, static method => method.Name == "AddDbContext");
        Assert.True(model.TryGetAvailableInvocationCandidates(useNpgsql, out var npgsqlCandidates));
        Assert.Contains(npgsqlCandidates, static method => method.Name == "UseNpgsql");
        Assert.True(model.TryGetInvocationTargetSymbolInfo(useNpgsql, out var npgsqlInfo));
        var useNpgsqlMethod = Assert.IsAssignableFrom<IMethodSymbol>(npgsqlInfo.Symbol);
        Assert.Equal("UseNpgsql", useNpgsqlMethod.Name);
        Assert.Contains(
            useNpgsqlMethod.Parameters,
            static parameter => parameter.Name == "connectionString" &&
                parameter.Type.GetPlainType().SpecialType == SpecialType.System_String);

        var afterUseNpgsql = instrumentation.SemanticQuery.CaptureSnapshot();
        var useNpgsqlDelta = SemanticQueryInstrumentation.Subtract(afterUseNpgsql, before);
        Assert.Equal(0, useNpgsqlDelta.BoundNodeQueries);

        before = instrumentation.SemanticQuery.CaptureSnapshot();
        Assert.True(model.TryGetInvocationTargetSymbolInfo(getRequiredService, out var getRequiredServiceInfo));
        var getRequiredServiceMethod = Assert.IsAssignableFrom<IMethodSymbol>(getRequiredServiceInfo.Symbol);
        Assert.Equal("GetRequiredService", getRequiredServiceMethod.Name);
        Assert.Equal("VehicleDbContext", getRequiredServiceMethod.ReturnType.GetPlainType().Name);

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeQueries);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void OpenProject_EfCoreExpressionTrees_PipeChainSecondHopAfterEdit_DoesNotBindColdBody()
    {
        var repoRoot = FindRepositoryRoot();
        var projectPath = Path.Combine(repoRoot, "samples", "projects", "efcore-expression-trees", "EfCoreExpressionTrees.rvnproj");
        var sourcePath = Path.Combine(repoRoot, "samples", "projects", "efcore-expression-trees", "src", "main.rvn");

        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        workspace.TryApplyChanges(workspace.CurrentSolution.WithCompilationOptions(
            projectId,
            project.CompilationOptions!.WithPerformanceInstrumentation(instrumentation)));

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        initialCompilation.GetDocumentDiagnostics(initialTree, analyzerOptions: null, CancellationToken.None);

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document =>
            string.Equals(document.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var updatedText = SourceText.From(
            File.ReadAllText(sourcePath).Replace("val minAge = 21", "val minAge = 26", StringComparison.Ordinal));
        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(document.Id, updatedText));

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var orderBy = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation =>
                invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "OrderBy" });

        instrumentation.BinderReentry.Reset();
        var setupBefore = instrumentation.Setup.CaptureSnapshot();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetInvocationTargetSymbolInfo(orderBy, out var orderByInfo));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var setupDelta = CompilerSetupInstrumentation.Subtract(instrumentation.Setup.CaptureSnapshot(), setupBefore);
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        var method = Assert.IsAssignableFrom<IMethodSymbol>(orderByInfo.Symbol);
        Assert.Equal("OrderBy", method.Name);
        Assert.Contains("IOrderedQueryable<User>", method.ReturnType.ToDisplayString());
        Assert.Equal(0, setupDelta.EnsureSourceDeclarationsCompleteCalls);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
        Assert.True(
            instrumentation.BinderReentry.TotalBindExecutions == 0,
            instrumentation.BinderReentry.GetSummary());
    }

    [Fact]
    public void OpenProject_EfCoreSample_IncludeLambdaInvocationTarget_PrefersGenericExpressionOverStringOverload()
    {
        var repoRoot = FindRepositoryRoot();
        var projectPath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "VehicleCostsApi.rvnproj");
        var sourcePath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "src", "Api", "Main.rvn");

        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        workspace.TryApplyChanges(workspace.CurrentSolution.WithCompilationOptions(
            projectId,
            project.CompilationOptions!.WithPerformanceInstrumentation(instrumentation)));

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var includeInvocation = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .First(invocation =>
                invocation.Expression is MemberAccessExpressionSyntax { Name: SimpleNameSyntax { Identifier.ValueText: "Include" } } &&
                invocation.ArgumentList.Arguments.SingleOrDefault()?.Expression is FunctionExpressionSyntax);

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetInvocationTargetSymbolInfo(includeInvocation, out var info));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        var method = Assert.IsAssignableFrom<IMethodSymbol>(info.Symbol);
        Assert.Equal("Include", method.Name);
        Assert.Equal("Expression", method.Parameters[1].Type.GetPlainType().Name);
        Assert.NotEqual("String", method.Parameters[1].Type.GetPlainType().Name);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
    }

    [Fact]
    public void OpenProject_EfCoreSample_AddDbContextColdSymbolInfo_DoesNotBindBodies()
    {
        var repoRoot = FindRepositoryRoot();
        var projectPath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "VehicleCostsApi.rvnproj");
        var sourcePath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "src", "Api", "Main.rvn");

        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        workspace.TryApplyChanges(workspace.CurrentSolution.WithCompilationOptions(
            projectId,
            project.CompilationOptions!.WithPerformanceInstrumentation(instrumentation)));

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var semanticModelSetupBefore = compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
        var model = compilation.GetSemanticModel(tree);
        var semanticModelSetupDelta = CompilerSetupInstrumentation.Subtract(
            compilation.PerformanceInstrumentation.Setup.CaptureSnapshot(),
            semanticModelSetupBefore);
        Assert.Equal(0, semanticModelSetupDelta.EnsureSourceDeclarationsDeclaredCalls);
        Assert.Equal(0, semanticModelSetupDelta.DeclarationPasses);
        var root = tree.GetRoot();
        var addDbContext = FindMemberInvocation(root, "AddDbContext");
        var addDbContextName = FindMemberName(root, "AddDbContext");

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        var memberInfo = model.GetSymbolInfo(addDbContextName);
        var invocationInfo = model.GetSymbolInfo(addDbContext);
        var typeInfo = model.GetTypeInfo(addDbContext);

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        AssertSymbolInfoContains(memberInfo, "AddDbContext");
        AssertSymbolInfoContains(invocationInfo, "AddDbContext");
        Assert.NotNull(typeInfo.Type);
        Assert.True(
            instrumentation.BinderReentry.TotalBindExecutions == 0,
            instrumentation.BinderReentry.GetSummary());
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
        Assert.Equal(0, delta.TypeInfoBoundFallbacks);
        Assert.Equal(0, delta.TypeInfoDiagnosticFallbacks);
    }

    [Fact]
    public void OpenProject_EfCoreSample_LambdaParameterHoverApis_DoNotBindBodies()
    {
        var repoRoot = FindRepositoryRoot();
        var projectPath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "VehicleCostsApi.rvnproj");
        var sourcePath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "src", "Api", "Main.rvn");

        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        workspace.TryApplyChanges(workspace.CurrentSolution.WithCompilationOptions(
            projectId,
            project.CompilationOptions!.WithPerformanceInstrumentation(instrumentation)));

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var addDbContextParameter = FindFunctionParameter(root, "options", enclosingInvocationName: "AddDbContext");
        var includeParameter = FindFirstFunctionParameter(root, "candidate", enclosingInvocationName: "Include");
        var orderByParameter = FindFunctionParameter(root, "vehicle", enclosingInvocationName: "OrderBy");
        var includeFunction = includeParameter.Ancestors().OfType<FunctionExpressionSyntax>().First();
        var orderByFunction = orderByParameter.Ancestors().OfType<FunctionExpressionSyntax>().First();
        var contextReceiver = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(static identifier =>
                identifier.Identifier.ValueText == "context" &&
                identifier.Parent is MemberAccessExpressionSyntax
                {
                    Name: IdentifierNameSyntax { Identifier.ValueText: "Vehicles" }
                });
        var includeReceiver = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(static identifier =>
                identifier.Identifier.ValueText == "candidate" &&
                identifier.Parent is MemberAccessExpressionSyntax
                {
                    Name: IdentifierNameSyntax { Identifier.ValueText: "FuelConsumptions" }
                } &&
                identifier.Ancestors().OfType<InvocationExpressionSyntax>().FirstOrDefault() is
                {
                    Expression: MemberAccessExpressionSyntax { Name: SimpleNameSyntax { Identifier.ValueText: "Include" } }
                });
        var orderByReceiver = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(static identifier =>
                identifier.Identifier.ValueText == "vehicle" &&
                identifier.Parent is MemberAccessExpressionSyntax
                {
                    Name: IdentifierNameSyntax { Identifier.ValueText: "RegistrationNumber" }
                } &&
                identifier.Ancestors().OfType<InvocationExpressionSyntax>().FirstOrDefault() is
                {
                    Expression: MemberAccessExpressionSyntax { Name: SimpleNameSyntax { Identifier.ValueText: "OrderBy" } }
                });

        T QueryWithoutBinding<T>(string label, Func<T> query)
        {
            instrumentation.BinderReentry.Reset();
            var before = instrumentation.SemanticQuery.CaptureSnapshot();

            var result = query();

            var after = instrumentation.SemanticQuery.CaptureSnapshot();
            var delta = SemanticQueryInstrumentation.Subtract(after, before);
            Assert.True(delta.SymbolInfoBinderFallbacks == 0, $"{label} used {delta.SymbolInfoBinderFallbacks} symbol binder fallback(s).");
            Assert.True(delta.BoundNodeBindFallbacks == 0, $"{label} used {delta.BoundNodeBindFallbacks} bound-node fallback(s).");
            Assert.True(delta.TypeInfoBoundFallbacks == 0, $"{label} used {delta.TypeInfoBoundFallbacks} type-info bound fallback(s).");
            Assert.True(delta.TypeInfoDiagnosticFallbacks == 0, $"{label} used {delta.TypeInfoDiagnosticFallbacks} type-info diagnostic fallback(s).");
            return result;
        }

        IParameterSymbol? QueryParameterFromBinderOwnedState(
            string label,
            ParameterSyntax parameter)
        {
            var setupBefore = compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
            var before = instrumentation.FunctionExpressionParameters.CaptureSnapshot();
            var symbol = QueryWithoutBinding(label, () => model.GetFunctionExpressionParameterSymbol(parameter));
            var setupDelta = CompilerSetupInstrumentation.Subtract(
                compilation.PerformanceInstrumentation.Setup.CaptureSnapshot(),
                setupBefore);
            var delta = FunctionExpressionParameterInstrumentation.Subtract(
                instrumentation.FunctionExpressionParameters.CaptureSnapshot(),
                before);

            Assert.Equal(0, setupDelta.EnsureSourceDeclarationsCompleteCalls);
            Assert.True(
                delta.FastBoundCacheHits + delta.FastSymbolCacheHits + delta.FastDelegateHits > 0,
                $"{label} did not resolve from cached or binder-owned lambda state: {FunctionExpressionParameterInstrumentation.FormatDelta(delta)}");
            Assert.Equal(0, delta.ContextualHits);
            Assert.Equal(0, delta.DirectBoundHits);
            Assert.Equal(0, delta.SymbolFallbackHits);
            Assert.Equal(0, delta.Misses);
            return symbol;
        }

        AssertSymbolName(
            QueryParameterFromBinderOwnedState(
                "Cold Include GetFunctionExpressionParameterSymbol",
                includeParameter),
            "candidate");
        AssertSymbolName(
            QueryParameterFromBinderOwnedState(
                "Warm Include GetFunctionExpressionParameterSymbol",
                includeParameter),
            "candidate");

        var sourceTypeLookupSetupBefore = compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
        Assert.Equal(
            "VehicleDbContext",
            QueryWithoutBinding("Context receiver GetTypeInfo", () => model.GetTypeInfo(contextReceiver)).Type?.Name);
        var sourceTypeLookupSetupDelta = CompilerSetupInstrumentation.Subtract(
            compilation.PerformanceInstrumentation.Setup.CaptureSnapshot(),
            sourceTypeLookupSetupBefore);
        Assert.True(sourceTypeLookupSetupDelta.EnsureSourceDeclarationsDeclaredCalls <= 1);
        Assert.True(sourceTypeLookupSetupDelta.EnsureSourceDeclarationsCompleteCalls <= 1);

        AssertSymbolName(QueryWithoutBinding("AddDbContext GetFunctionExpressionParameterSymbol", () => model.GetFunctionExpressionParameterSymbol(addDbContextParameter)), "options");
        AssertSymbolName(
            QueryWithoutBinding("AddDbContext TryResolveFunctionExpressionParameterSymbolFast", () =>
            {
                Assert.True(model.TryResolveFunctionExpressionParameterSymbolFast(addDbContextParameter, out var symbol));
                return symbol;
            }),
            "options");
        AssertSymbolName(QueryWithoutBinding("AddDbContext GetDeclaredSymbol", () => model.GetDeclaredSymbol(addDbContextParameter)), "options");
        AssertSymbolInfoContains(QueryWithoutBinding("AddDbContext GetSymbolInfo", () => model.GetSymbolInfo(addDbContextParameter)), "options");

        AssertSymbolName(QueryParameterFromBinderOwnedState("Include GetFunctionExpressionParameterSymbol", includeParameter), "candidate");
        AssertSymbolName(
            QueryWithoutBinding("Include TryGetFunctionExpressionSymbol", () =>
            {
                Assert.True(model.TryGetFunctionExpressionSymbol(includeFunction, out var symbol));
                return symbol?.Parameters.FirstOrDefault();
            }),
            "candidate");
        AssertSymbolName(QueryWithoutBinding("Include GetDeclaredSymbol", () => model.GetDeclaredSymbol(includeParameter)), "candidate");
        AssertSymbolInfoContains(QueryWithoutBinding("Include GetSymbolInfo", () => model.GetSymbolInfo(includeParameter)), "candidate");
        AssertSymbolInfoContains(QueryWithoutBinding("Include receiver GetSymbolInfo", () => model.GetSymbolInfo(includeReceiver)), "candidate");
        Assert.Equal("VehicleEntity", QueryWithoutBinding("Include receiver GetTypeInfo", () => model.GetTypeInfo(includeReceiver)).Type?.Name);

        AssertSymbolName(QueryParameterFromBinderOwnedState("OrderBy GetFunctionExpressionParameterSymbol", orderByParameter), "vehicle");
        AssertSymbolName(
            QueryWithoutBinding("OrderBy TryGetFunctionExpressionSymbol", () =>
            {
                Assert.True(model.TryGetFunctionExpressionSymbol(orderByFunction, out var symbol));
                return symbol?.Parameters.FirstOrDefault();
            }),
            "vehicle");
        AssertSymbolName(QueryWithoutBinding("OrderBy GetDeclaredSymbol", () => model.GetDeclaredSymbol(orderByParameter)), "vehicle");
        AssertSymbolInfoContains(QueryWithoutBinding("OrderBy GetSymbolInfo", () => model.GetSymbolInfo(orderByParameter)), "vehicle");
        AssertSymbolInfoContains(QueryWithoutBinding("OrderBy receiver GetSymbolInfo", () => model.GetSymbolInfo(orderByReceiver)), "vehicle");
        Assert.Equal("VehicleEntity", QueryWithoutBinding("OrderBy receiver GetTypeInfo", () => model.GetTypeInfo(orderByReceiver)).Type?.Name);
    }

    [Fact]
    public void OpenProject_EfCoreExpressionTrees_PipeSelectLambdaParameterResolvesFromBinder()
    {
        var repoRoot = FindRepositoryRoot();
        var projectPath = Path.Combine(repoRoot, "samples", "projects", "efcore-expression-trees", "EfCoreExpressionTrees.rvnproj");
        var sourcePath = Path.Combine(repoRoot, "samples", "projects", "efcore-expression-trees", "src", "main.rvn");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var selectInvocation = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is IdentifierNameSyntax { Identifier.ValueText: "Select" });
        var lambda = Assert.IsType<SimpleFunctionExpressionSyntax>(selectInvocation.ArgumentList.Arguments.Single().Expression);
        var targetTypedLambda = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(static declarator => declarator.Identifier.ValueText == "onlyActiveAdults")
            .Initializer!.Value;
        var targetTypedFunction = Assert.IsType<SimpleFunctionExpressionSyntax>(targetTypedLambda);

        Assert.True(model.TryResolveFunctionExpressionParameterSymbolFast(targetTypedFunction.Parameter, out var targetTypedParameter));
        Assert.NotNull(targetTypedParameter);
        Assert.Equal("User", targetTypedParameter.Type.Name);

        if (model.TryResolveFunctionExpressionParameterSymbolFast(lambda.Parameter, out var fastParameter))
        {
            Assert.NotNull(fastParameter);
            Assert.Equal("User", fastParameter.Type.Name);
        }

        var parameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetFunctionExpressionParameterSymbol(lambda.Parameter));
        Assert.Equal("User", parameter.Type.Name);
    }

    [Fact]
    public void OpenProject_EfCoreExpressionTrees_PipeLinqOverloadsResolveFromBinder()
    {
        var repoRoot = FindRepositoryRoot();
        var projectPath = Path.Combine(repoRoot, "samples", "projects", "efcore-expression-trees", "EfCoreExpressionTrees.rvnproj");
        var sourcePath = Path.Combine(repoRoot, "samples", "projects", "efcore-expression-trees", "src", "main.rvn");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var invocations = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Where(static node => node.Expression is IdentifierNameSyntax)
            .ToArray();

        var where = AssertPipeMethod(model, invocations, "Where", "Queryable");
        Assert.Contains("Expression", where.Parameters[1].Type.ToDisplayString());
        Assert.DoesNotContain("Int32", where.Parameters[1].Type.ToDisplayString());

        var orderBy = AssertPipeMethod(model, invocations, "OrderBy", "Queryable");
        Assert.Contains("IOrderedQueryable", orderBy.ReturnType.ToDisplayString());

        var select = AssertPipeMethod(model, invocations, "Select", "Queryable");
        Assert.Contains("IQueryable", select.ReturnType.ToDisplayString());

        static IMethodSymbol AssertPipeMethod(
            SemanticModel model,
            InvocationExpressionSyntax[] invocations,
            string methodName,
            string containingTypeName)
        {
            var invocation = invocations.Single(node =>
                node.Expression is IdentifierNameSyntax { Identifier.ValueText: var name } &&
                string.Equals(name, methodName, StringComparison.Ordinal));
            var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
            Assert.Equal(methodName, method.Name);
            Assert.Equal(containingTypeName, method.ContainingType?.Name);

            var identifier = (IdentifierNameSyntax)invocation.Expression;
            var identifierMethod = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(identifier).Symbol);
            Assert.True(SymbolEqualityComparer.Default.Equals(method, identifierMethod));

            return method;
        }
    }

    [Fact]
    public void OpenProject_EfCoreSample_AwaitSymbolInfo_DoesNotBindBodies()
    {
        var repoRoot = FindRepositoryRoot();
        var projectPath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "VehicleCostsApi.rvnproj");
        var sourcePath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "src", "Api", "Main.rvn");

        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        workspace.TryApplyChanges(workspace.CurrentSolution.WithCompilationOptions(
            projectId,
            project.CompilationOptions!.WithPerformanceInstrumentation(instrumentation)));

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var sourceText = tree.GetText();
        var awaitExpression = root.DescendantNodes()
            .OfType<PrefixOperatorExpressionSyntax>()
            .First(expression =>
                expression.Kind == SyntaxKind.AwaitExpression &&
                sourceText.ToString(expression.Span).Contains("SingleOrDefaultAsync", StringComparison.Ordinal));
        var vehicleDeclarator = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .First(declarator =>
                string.Equals(declarator.Identifier.ValueText, "vehicle", StringComparison.Ordinal) &&
                declarator.Initializer is not null &&
                sourceText.ToString(declarator.Initializer.Span).Contains("SingleOrDefaultAsync", StringComparison.Ordinal));
        var vehiclesDeclarator = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .First(declarator =>
                string.Equals(declarator.Identifier.ValueText, "vehicles", StringComparison.Ordinal) &&
                declarator.Initializer is not null &&
                sourceText.ToString(declarator.Initializer.Span).Contains("ToListAsync", StringComparison.Ordinal));
        var vehiclesReference = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier =>
                string.Equals(identifier.Identifier.ValueText, "vehicles", StringComparison.Ordinal) &&
                identifier.Parent is MemberAccessExpressionSyntax
                {
                    Name: IdentifierNameSyntax { Identifier.ValueText: "Select" }
                });
        var toListAsync = vehiclesDeclarator.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation =>
                invocation.Expression is MemberAccessExpressionSyntax
                {
                    Name: IdentifierNameSyntax { Identifier.ValueText: "ToListAsync" }
                });
        var include = vehiclesDeclarator.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation =>
                invocation.Expression is MemberAccessExpressionSyntax
                {
                    Name: IdentifierNameSyntax { Identifier.ValueText: "Include" }
                });
        var orderBy = vehiclesDeclarator.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation =>
                invocation.Expression is MemberAccessExpressionSyntax
                {
                    Name: IdentifierNameSyntax { Identifier.ValueText: "OrderBy" }
                });
        var mapVehicleSelect = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .First(invocation =>
                invocation.Expression is MemberAccessExpressionSyntax
                {
                    Name: IdentifierNameSyntax { Identifier.ValueText: "Select" }
                } &&
                sourceText.ToString(invocation.Span).Contains("MapVehicle", StringComparison.Ordinal));
        var mapVehicleToArray = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .First(invocation =>
                invocation.Expression is MemberAccessExpressionSyntax
                {
                    Name: IdentifierNameSyntax { Identifier.ValueText: "ToArray" }
                } &&
                sourceText.ToString(invocation.Span).Contains("Select(MapVehicle)", StringComparison.Ordinal));
        var fuelConsumptionAdd = root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .First(invocation =>
                invocation.Expression is MemberAccessExpressionSyntax
                {
                    Name: IdentifierNameSyntax { Identifier.ValueText: "Add" }
                } &&
                sourceText.ToString(invocation.Span).Contains("FuelConsumptions.Add", StringComparison.Ordinal));

        foreach (var priorInvocation in root.DescendantNodes()
                     .OfType<InvocationExpressionSyntax>()
                     .Where(static invocation => invocation.Expression is MemberAccessExpressionSyntax)
                     .TakeWhile(invocation => !ReferenceEquals(invocation, fuelConsumptionAdd)))
        {
            _ = model.GetSymbolInfo(priorInvocation);
        }

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetAvailableInvocationCandidates(include, out var coldIncludeCandidates), "Include candidates");
        Assert.True(model.TryGetAvailableInvocationCandidates(orderBy, out var coldOrderByCandidates), "OrderBy candidates");
        Assert.Contains(coldOrderByCandidates, method => method.ReturnType.ToDisplayString().Contains("VehicleEntity", StringComparison.Ordinal));
        Assert.True(model.TryGetAvailableInvocationCandidates(toListAsync, out var coldToListCandidates), "ToListAsync candidates");
        Assert.Contains(coldToListCandidates, method =>
            method.ReturnType.ToDisplayString().Contains("VehicleEntity", StringComparison.Ordinal) &&
            !method.ReturnType.ToDisplayString().Contains("TSource", StringComparison.Ordinal));
        Assert.True(model.TryGetAvailableInvocationCandidates(mapVehicleSelect, out var coldSelectCandidates), "Select(MapVehicle) candidates");
        Assert.Contains(coldSelectCandidates, method =>
            method.ReturnType.ToDisplayString().Contains("VehicleResponse", StringComparison.Ordinal) &&
            !method.ReturnType.ToDisplayString().Contains("TResult", StringComparison.Ordinal));
        Assert.True(model.TryGetAvailableInvocationCandidates(mapVehicleToArray, out var coldToArrayCandidates), "ToArray after Select(MapVehicle) candidates");
        Assert.Contains(coldToArrayCandidates, method =>
            method.ReturnType.ToDisplayString().Contains("VehicleResponse", StringComparison.Ordinal) &&
            !method.ReturnType.ToDisplayString().Contains("TSource", StringComparison.Ordinal));
        var coldAddInfo = model.GetSymbolInfo(fuelConsumptionAdd);
        AssertSymbolInfoContains(coldAddInfo, "Add");
        Assert.True(model.TryGetAvailableInvocationCandidates(fuelConsumptionAdd, out var coldAddCandidates), "FuelConsumptions.Add candidates");
        Assert.Contains(coldAddCandidates, method =>
            string.Equals(method.Name, "Add", StringComparison.Ordinal) &&
            method.Parameters.Length == 1 &&
            method.Parameters[0].Type.ToDisplayString().Contains("FuelConsumptionRecord", StringComparison.Ordinal));

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.True(
            instrumentation.BinderReentry.TotalBindExecutions == 0,
            instrumentation.BinderReentry.GetSummary());
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);

        instrumentation.BinderReentry.Reset();
        before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(
            model.TryGetAvailableTypeInfo(toListAsync, out var cheapToListType),
            instrumentation.BinderReentry.GetSummary());
        Assert.Contains("VehicleEntity", cheapToListType.Type?.ToDisplayString() ?? string.Empty);

        Assert.True(
            model.TryGetAvailableTypeInfo(vehiclesDeclarator.Initializer!.Value, out var cheapInitializerType),
            instrumentation.BinderReentry.GetSummary());
        Assert.Equal("List", cheapInitializerType.Type?.Name);
        Assert.True(
            model.TryGetAvailableTypeInfo(mapVehicleSelect, out var cheapSelectType),
            instrumentation.BinderReentry.GetSummary());
        Assert.Contains("VehicleResponse", cheapSelectType.Type?.ToDisplayString() ?? string.Empty);
        Assert.True(
            model.TryGetAvailableTypeInfo(mapVehicleToArray, out var cheapToArrayType),
            instrumentation.BinderReentry.GetSummary());
        Assert.Equal("VehicleResponse[]", cheapToArrayType.Type?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        Assert.True(
            model.TryGetAvailableLocalDeclarationSymbol(
                vehicleDeclarator,
                out var cheapVehicleLocal,
                allowErrorType: true,
                allowInitializerBinding: true,
                allowBindingFallback: false),
            instrumentation.BinderReentry.GetSummary());
        Assert.Contains("VehicleEntity", cheapVehicleLocal!.Type.ToDisplayString());

        Assert.True(
            model.TryGetAvailableLocalDeclarationSymbol(
                vehiclesDeclarator,
                out var cheapVehiclesLocal,
                allowErrorType: true,
                allowInitializerBinding: true,
                allowBindingFallback: false),
            instrumentation.BinderReentry.GetSummary());
        Assert.Equal("List", cheapVehiclesLocal!.Type.Name);

        var coldVehiclesInfo = model.GetSymbolInfo(vehiclesReference);

        after = instrumentation.SemanticQuery.CaptureSnapshot();
        delta = SemanticQueryInstrumentation.Subtract(after, before);
        AssertSymbolInfoContains(coldVehiclesInfo, "vehicles");
        var coldVehiclesLocal = Assert.IsAssignableFrom<ILocalSymbol>(coldVehiclesInfo.Symbol ?? coldVehiclesInfo.CandidateSymbols.FirstOrDefault());
        Assert.Equal("List", coldVehiclesLocal.Type.Name);
        Assert.Contains("VehicleEntity", coldVehiclesLocal.Type.ToDisplayString());
        Assert.True(
            instrumentation.BinderReentry.TotalBindExecutions == 0,
            instrumentation.BinderReentry.GetSummary());
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);

        instrumentation.BinderReentry.Reset();
        before = instrumentation.SemanticQuery.CaptureSnapshot();

        var info = model.GetSymbolInfo(awaitExpression);

        after = instrumentation.SemanticQuery.CaptureSnapshot();
        delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.Null(info.Symbol);
        Assert.True(info.CandidateSymbols.IsDefaultOrEmpty);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);

        instrumentation.BinderReentry.Reset();
        before = instrumentation.SemanticQuery.CaptureSnapshot();

        var symbol = model.GetDeclaredSymbol(vehicleDeclarator);

        after = instrumentation.SemanticQuery.CaptureSnapshot();
        delta = SemanticQueryInstrumentation.Subtract(after, before);
        AssertSymbolName(symbol, "vehicle");
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);

        instrumentation.BinderReentry.Reset();
        before = instrumentation.SemanticQuery.CaptureSnapshot();

        var vehiclesInitializerType = model.GetTypeInfo(vehiclesDeclarator.Initializer!.Value);

        after = instrumentation.SemanticQuery.CaptureSnapshot();
        delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.Equal("List", vehiclesInitializerType.Type?.Name);
        Assert.Contains("VehicleEntity", vehiclesInitializerType.Type?.ToDisplayString() ?? string.Empty);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
        Assert.Equal(0, delta.TypeInfoBoundFallbacks);
        Assert.Equal(0, delta.TypeInfoDiagnosticFallbacks);

        instrumentation.BinderReentry.Reset();
        before = instrumentation.SemanticQuery.CaptureSnapshot();

        info = model.GetSymbolInfo(vehiclesReference);

        after = instrumentation.SemanticQuery.CaptureSnapshot();
        delta = SemanticQueryInstrumentation.Subtract(after, before);
        AssertSymbolInfoContains(info, "vehicles");
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);

        instrumentation.BinderReentry.Reset();
        before = instrumentation.SemanticQuery.CaptureSnapshot();

        var vehiclesReferenceType = model.GetTypeInfo(vehiclesReference);

        after = instrumentation.SemanticQuery.CaptureSnapshot();
        delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.Equal("List", vehiclesReferenceType.Type?.Name);
        Assert.Contains("VehicleEntity", vehiclesReferenceType.Type?.ToDisplayString() ?? string.Empty);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
        Assert.Equal(0, delta.TypeInfoBoundFallbacks);
        Assert.Equal(0, delta.TypeInfoDiagnosticFallbacks);
    }

    [Fact]
    public void OpenProject_EfCoreSample_SemanticModelReturnsHoverSymbols()
    {
        var repoRoot = FindRepositoryRoot();
        var projectPath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "VehicleCostsApi.rvnproj");
        var sourcePath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "src", "Api", "Main.rvn");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var sourceText = tree.GetText().ToString();

        AssertSymbolName(model.GetSymbolInfo(FindMemberName(root, "UseNpgsql")).Symbol, "UseNpgsql");
        AssertSymbolInfoContains(model.GetSymbolInfo(FindMemberName(root, "CreateBuilder")), "CreateBuilder");
        AssertSymbolName(model.GetSymbolInfo(FindIdentifier(root, "VehicleAppServices")).Symbol, "VehicleAppServices");

        var taskType = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier =>
                string.Equals(identifier.Identifier.ValueText, "Task", StringComparison.Ordinal) &&
                identifier.Ancestors().OfType<ArrowTypeClauseSyntax>().Any());
        AssertSymbolName(model.GetSymbolInfo(taskType).Symbol, "Task");

        var builderDeclarator = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(static declarator => declarator.Identifier.ValueText == "builder");
        AssertSymbolName(model.GetDeclaredSymbol(builderDeclarator), "builder");

        AssertSymbolName(QuerySymbolAt(model, root, sourceText, line: 15, character: 29), "UseNpgsql");
        AssertSymbolName(QuerySymbolAt(model, root, sourceText, line: 10, character: 49), "Task");
        AssertSymbolName(QuerySymbolAt(model, root, sourceText, line: 11, character: 42), "CreateBuilder");
        AssertSymbolName(QuerySymbolAt(model, root, sourceText, line: 11, character: 16), "builder");
        AssertSymbolName(QuerySymbolAt(model, root, sourceText, line: 15, character: 14), "options");
        AssertSymbolName(QuerySymbolAt(model, root, sourceText, line: 15, character: 41), "VehicleAppServices");
    }

    [Fact]
    public void OpenProject_EfCoreSample_TriviaEditThenHoverQuery_DoesNotReportAwaitableDiagnostic()
    {
        var repoRoot = FindRepositoryRoot();
        var projectPath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "VehicleCostsApi.rvnproj");
        var sourcePath = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs", "src", "Api", "Main.rvn");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document =>
            string.Equals(document.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var sourceText = document.GetTextAsync().GetAwaiter().GetResult()!.ToString();
        var updatedText = sourceText.Replace("CreateBuilder(args)", "CreateBuilder( args)", StringComparison.Ordinal);

        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(document.Id, SourceText.From(updatedText))).ShouldBeTrue();

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree =>
            string.Equals(tree.FilePath, sourcePath, StringComparison.OrdinalIgnoreCase));
        var semanticModelSetupBefore = compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
        var model = compilation.GetSemanticModel(tree);
        var semanticModelSetupDelta = CompilerSetupInstrumentation.Subtract(
            compilation.PerformanceInstrumentation.Setup.CaptureSnapshot(),
            semanticModelSetupBefore);
        Assert.Equal(0, semanticModelSetupDelta.EnsureSourceDeclarationsDeclaredCalls);
        Assert.Equal(0, semanticModelSetupDelta.DeclarationPasses);
        var root = tree.GetRoot();

        var instrumentation = compilation.PerformanceInstrumentation;
        instrumentation.BinderReentry.Reset();
        var setupBefore = instrumentation.Setup.CaptureSnapshot();
        var queryBefore = instrumentation.SemanticQuery.CaptureSnapshot();

        var createBuilderInfo = model.GetSymbolInfo(FindMemberName(root, "CreateBuilder"));

        var setupDelta = CompilerSetupInstrumentation.Subtract(
            instrumentation.Setup.CaptureSnapshot(),
            setupBefore);
        var queryDelta = SemanticQueryInstrumentation.Subtract(
            instrumentation.SemanticQuery.CaptureSnapshot(),
            queryBefore);
        AssertSymbolInfoContains(createBuilderInfo, "CreateBuilder");
        Assert.Equal(0, setupDelta.EnsureRootBinderCreatedCalls);
        Assert.Equal(0, setupDelta.RootBinderCreations);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, queryDelta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, queryDelta.BoundNodeBindFallbacks);

        var diagnostics = compilation.GetDiagnostics();

        var awaitableDiagnostics = diagnostics
            .Where(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.ExpressionIsNotAwaitable)
            .Select(diagnostic => $"{diagnostic.Location.GetLineSpan().StartLinePosition}-{diagnostic.Location.GetLineSpan().EndLinePosition}: {diagnostic.GetMessage()}")
            .ToArray();
        Assert.Empty(awaitableDiagnostics);
    }

    [Fact]
    public void OpenProject_FrameworkReference_EmitsMinimalApiProjectWithParameterizedSyncLambda()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(
            sourcePath,
            """
            import Microsoft.AspNetCore.Builder.*

            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()
            app.MapGet("/", (name: string) => "Hello ${name}")
            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var compilation = workspace.GetCompilation(projectId);

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));
    }

    private static InvocationExpressionSyntax FindMemberInvocation(SyntaxNode root, string name)
        => root.DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(invocation =>
                invocation.Expression is MemberAccessExpressionSyntax { Name: SimpleNameSyntax memberName } &&
                string.Equals(memberName.Identifier.ValueText, name, StringComparison.Ordinal));

    private static SimpleNameSyntax FindMemberName(SyntaxNode root, string name)
        => root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Select(static memberAccess => memberAccess.Name)
            .Single(memberName => string.Equals(memberName.Identifier.ValueText, name, StringComparison.Ordinal));

    private static ParameterSyntax FindFunctionParameter(SyntaxNode root, string parameterName, string enclosingInvocationName)
        => root.DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Where(function => function.Ancestors().OfType<InvocationExpressionSyntax>().FirstOrDefault() is { } invocation &&
                invocation.Expression is MemberAccessExpressionSyntax { Name: SimpleNameSyntax memberName } &&
                string.Equals(memberName.Identifier.ValueText, enclosingInvocationName, StringComparison.Ordinal))
            .SelectMany(static function => function switch
            {
                SimpleFunctionExpressionSyntax simple => [simple.Parameter],
                ParenthesizedFunctionExpressionSyntax { ParameterList: { } parameterList } => parameterList.Parameters,
                _ => Enumerable.Empty<ParameterSyntax>()
            })
            .Single(parameter => string.Equals(parameter.Identifier.ValueText, parameterName, StringComparison.Ordinal));

    private static ParameterSyntax FindFirstFunctionParameter(SyntaxNode root, string parameterName, string enclosingInvocationName)
        => root.DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Where(function => function.Ancestors().OfType<InvocationExpressionSyntax>().FirstOrDefault() is { } invocation &&
                invocation.Expression is MemberAccessExpressionSyntax { Name: SimpleNameSyntax memberName } &&
                string.Equals(memberName.Identifier.ValueText, enclosingInvocationName, StringComparison.Ordinal))
            .SelectMany(static function => function switch
            {
                SimpleFunctionExpressionSyntax simple => [simple.Parameter],
                ParenthesizedFunctionExpressionSyntax { ParameterList: { } parameterList } => parameterList.Parameters,
                _ => Enumerable.Empty<ParameterSyntax>()
            })
            .First(parameter => string.Equals(parameter.Identifier.ValueText, parameterName, StringComparison.Ordinal));

    private static IdentifierNameSyntax FindIdentifier(SyntaxNode root, string name)
        => root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(identifier => string.Equals(identifier.Identifier.ValueText, name, StringComparison.Ordinal));

    private static void AssertSymbolName(ISymbol? symbol, string expectedName)
    {
        Assert.NotNull(symbol);
        Assert.Equal(expectedName, symbol!.Name);
    }

    private static void AssertSymbolInfoContains(SymbolInfo info, string expectedName)
    {
        if (info.Symbol is not null)
        {
            Assert.Equal(expectedName, info.Symbol.Name);
            return;
        }

        Assert.Contains(info.CandidateSymbols, symbol => string.Equals(symbol.Name, expectedName, StringComparison.Ordinal));
    }

    private static ISymbol? QuerySymbolAt(
        SemanticModel model,
        SyntaxNode root,
        string sourceText,
        int line,
        int character)
    {
        var position = GetPosition(sourceText, line, character);
        var token = root.FindToken(position);
        var node = token.Parent;

        return node switch
        {
            VariableDeclaratorSyntax declarator when token == declarator.Identifier => model.GetDeclaredSymbol(declarator),
            SimpleNameSyntax name => model.GetSymbolInfo(name).Symbol,
            InvocationExpressionSyntax invocation => model.GetSymbolInfo(invocation).Symbol,
            TypeSyntax typeSyntax => model.GetSymbolInfo(typeSyntax).Symbol ?? model.GetTypeInfo(typeSyntax).Type,
            _ => node?.AncestorsAndSelf()
                .Select(ancestor => ancestor switch
                {
                    VariableDeclaratorSyntax declarator when token == declarator.Identifier => model.GetDeclaredSymbol(declarator),
                    SimpleNameSyntax name => model.GetSymbolInfo(name).Symbol,
                    InvocationExpressionSyntax invocation => model.GetSymbolInfo(invocation).Symbol,
                    TypeSyntax typeSyntax => model.GetSymbolInfo(typeSyntax).Symbol ?? model.GetTypeInfo(typeSyntax).Type,
                    _ => null
                })
                .FirstOrDefault(static symbol => symbol is not null)
        };
    }

    private static int GetPosition(string sourceText, int line, int character)
    {
        var currentLine = 0;
        var lineStart = 0;
        for (var i = 0; i < sourceText.Length && currentLine < line; i++)
        {
            if (sourceText[i] != '\n')
                continue;

            currentLine++;
            lineStart = i + 1;
        }

        return Math.Min(sourceText.Length, lineStart + character);
    }

    private static string FindRepositoryRoot()
    {
        var current = new DirectoryInfo(AppContext.BaseDirectory);
        while (current is not null)
        {
            if (File.Exists(Path.Combine(current.FullName, "Raven.sln")))
                return current.FullName;

            current = current.Parent;
        }

        throw new DirectoryNotFoundException("Could not locate Raven.sln from test base directory.");
    }

    [Fact]
    public void OpenProject_FrameworkReference_AllowsMapGetAndMapPostWithAsyncLambdas()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(
            sourcePath,
            """
            import System.Threading.Tasks.*
            import Microsoft.AspNetCore.Builder.*

            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()

            app.MapGet("/", () => "sync")
            app.MapGet("/async", async () => {
                await Task.Delay(1)
                return "async-get"
            })

            app.MapPost("/submit", () => "sync-post")
            app.MapPost("/submit-async", async () => {
                await Task.Delay(1)
                return "async-post"
            })

            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var diagnostics = workspace.GetDiagnostics(projectId);

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV1501");
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == CompilerDiagnostics.CallIsAmbiguous.Id);
    }

    [Fact]
    public void OpenProject_FrameworkReference_AllowsMapGetWithAsyncIteratorLambda()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(
            sourcePath,
            """
            import System.Collections.Generic.*
            import Microsoft.AspNetCore.Builder.*

            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()

            app.MapGet("/stream", async () -> IAsyncEnumerable<int> => {
                yield return 1
                yield return 2
                yield return 3
            })

            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var diagnostics = workspace.GetDiagnostics(projectId);

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void OpenProject_FrameworkReference_AllowsMapGetWithInferredAsyncIteratorLambda()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(
            sourcePath,
            """
            import Microsoft.AspNetCore.Builder.*

            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()

            app.MapGet("/stream", async () => {
                yield return 1
                yield return 2
                yield return 3
            })

            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var diagnostics = workspace.GetDiagnostics(projectId);

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void OpenProject_FrameworkReference_AsyncIteratorLambdaCancellationTokenWithoutEnumeratorCancellation_WarnsButBinds()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(
            sourcePath,
            """
            import Microsoft.AspNetCore.Builder.*
            import System.Threading.*
            import System.Threading.Tasks.*

            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()

            app.MapGet("/stream", async (cancellationToken: CancellationToken) => {
                yield return 1
                await Task.Delay(1, cancellationToken)
                yield return 2
            })

            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var diagnostics = workspace.GetDiagnostics(projectId);

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        Assert.Contains(diagnostics, diagnostic => diagnostic.Id == CompilerDiagnostics.EnumeratorCancellationAttributeMissing.Id);
    }

    [Fact]
    public void OpenProject_FrameworkReference_AsyncIteratorLambdaEnumeratorCancellation_SuppressesWarning()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(
            sourcePath,
            """
            import Microsoft.AspNetCore.Builder.*
            import System.Runtime.CompilerServices.*
            import System.Threading.*
            import System.Threading.Tasks.*

            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()

            app.MapGet("/stream", async ([EnumeratorCancellation] cancellationToken: CancellationToken) => {
                yield return 1
                await Task.Delay(1, cancellationToken)
                yield return 2
            })

            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var diagnostics = workspace.GetDiagnostics(projectId);

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == CompilerDiagnostics.EnumeratorCancellationAttributeMissing.Id);
    }

}
