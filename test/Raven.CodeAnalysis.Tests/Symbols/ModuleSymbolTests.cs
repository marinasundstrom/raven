using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ModuleSymbolTests
{
    [Fact]
    public void SourceModule_ProjectsOwnedNamespace()
    {
        var tree = SyntaxTree.ParseText(
            """
            namespace Raven.Project
            class Widget {}
            """);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error));
        var module = Assert.Single(compilation.Assembly.Modules);
        var ravenNamespace = Assert.IsAssignableFrom<INamespaceSymbol>(
            compilation.GlobalNamespace.LookupNamespace("Raven"));
        var projectNamespace = Assert.IsAssignableFrom<INamespaceSymbol>(
            ravenNamespace.LookupNamespace("Project"));

        var projected = module.GetModuleNamespace(projectNamespace);

        Assert.NotNull(projected);
        Assert.Equal("Raven.Project", projected.ToMetadataName());
        Assert.Same(module.GlobalNamespace, module.GetModuleNamespace(compilation.GlobalNamespace));
    }

    [Fact]
    public void Modules_ProjectOnlyNamespacesTheyOwn()
    {
        var tree = SyntaxTree.ParseText(
            """
            namespace Raven.Project
            class Widget {}
            """);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error));
        var sourceModule = Assert.Single(compilation.Assembly.Modules);
        var systemNamespace = Assert.IsAssignableFrom<INamespaceSymbol>(
            compilation.GlobalNamespace.LookupNamespace("System"));
        var ravenNamespace = Assert.IsAssignableFrom<INamespaceSymbol>(
            compilation.GlobalNamespace.LookupNamespace("Raven"));
        var projectNamespace = Assert.IsAssignableFrom<INamespaceSymbol>(
            ravenNamespace.LookupNamespace("Project"));
        var coreLibrary = compilation.GetSpecialType(SpecialType.System_Object).ContainingAssembly;
        var metadataModule = Assert.Single(coreLibrary!.Modules);

        var projectedSystem = metadataModule.GetModuleNamespace(systemNamespace);
        var sourceSystem = sourceModule.GetModuleNamespace(systemNamespace);

        Assert.NotNull(projectedSystem);
        Assert.Equal("System", projectedSystem.ToMetadataName());
        Assert.NotNull(sourceSystem);
        Assert.Equal("System", sourceSystem.ToMetadataName());
        Assert.Same(sourceModule, sourceSystem.ContainingModule);
        Assert.Null(metadataModule.GetModuleNamespace(projectNamespace));
        Assert.Same(metadataModule.GlobalNamespace, metadataModule.GetModuleNamespace(compilation.GlobalNamespace));
    }

    [Fact]
    public async Task SourceGlobalNamespaces_AreStableAcrossConcurrentAccess()
    {
        var compilation = Compilation.Create(
            "test",
            [SyntaxTree.ParseText("class Widget {}")],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        _ = compilation.GetSourceGlobalNamespace();
        var module = Assert.Single(compilation.Assembly.Modules);

        var moduleTasks = Enumerable.Range(0, 16)
            .Select(_ => Task.Run(() => module.GlobalNamespace))
            .ToArray();
        var assemblyTasks = Enumerable.Range(0, 16)
            .Select(_ => Task.Run(() => compilation.Assembly.GlobalNamespace))
            .ToArray();
        await Task.WhenAll([.. moduleTasks, .. assemblyTasks]);

        Assert.All(moduleTasks, task => Assert.Same(module.GlobalNamespace, task.Result));
        Assert.All(assemblyTasks, task => Assert.Same(compilation.Assembly.GlobalNamespace, task.Result));
    }
}
