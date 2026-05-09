using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

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
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
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
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
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
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var addDbContext = FindMemberInvocation(root, "AddDbContext");
        var useNpgsql = FindMemberInvocation(root, "UseNpgsql");

        instrumentation.BinderReentry.Reset();
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        Assert.True(model.TryGetAvailableInvocationCandidates(addDbContext, out var dbContextCandidates));
        Assert.Contains(dbContextCandidates, static method => method.Name == "AddDbContext");
        Assert.True(model.TryGetAvailableInvocationCandidates(useNpgsql, out var npgsqlCandidates));
        Assert.Contains(npgsqlCandidates, static method => method.Name == "UseNpgsql");

        var after = instrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);
        Assert.Equal(0, instrumentation.BinderReentry.TotalBindExecutions);
        Assert.Equal(0, delta.SymbolInfoBinderFallbacks);
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
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
        AssertSymbolName(model.GetSymbolInfo(FindMemberName(root, "CreateBuilder")).Symbol, "CreateBuilder");
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

    private static IdentifierNameSyntax FindIdentifier(SyntaxNode root, string name)
        => root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(identifier => string.Equals(identifier.Identifier.ValueText, name, StringComparison.Ordinal));

    private static void AssertSymbolName(ISymbol? symbol, string expectedName)
    {
        Assert.NotNull(symbol);
        Assert.Equal(expectedName, symbol!.Name);
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
