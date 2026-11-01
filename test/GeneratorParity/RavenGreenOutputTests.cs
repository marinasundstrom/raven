using System.Reflection;
using System.Runtime.Loader;

using NodesShared;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.Generators;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "DotNet")]
public class RavenGreenOutputTests
{
    [Fact]
    public void RepositoryGreenNodeAndFactoryOutputParsesAsRaven()
    {
        foreach (var node in NodeEmitterInputs.Nodes())
        {
            AssertParses(RavenGreenNodeGenerator.GenerateNode(node), node.Name);
            var factory = RavenGreenNodeGenerator.GenerateFactory(node);
            if (node.IsAbstract)
                Assert.Null(factory);
            else
                AssertParses(factory!, node.Name + " factory");
        }
    }

    [Fact]
    public void GeneratedHierarchyCompilesAndRunsConstructorsSlotsUpdatesFactoriesAndVisitors()
    {
        var expression = new SyntaxNodeModel
        {
            Name = "Expression",
            Inherits = "Node",
            IsAbstract = true,
            Slots = [new() { Name = "Keyword", Type = "Token", IsAbstract = true }]
        };
        var sample = new SyntaxNodeModel
        {
            Name = "Sample",
            Inherits = "Expression",
            HasExplicitKind = true,
            Slots = [new() { Name = "Keyword", Type = "Token", IsInherited = true },
                new() { Name = "Child", Type = "Node", IsNullable = true }]
        };
        var sources = new[] { File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "GreenOutputRuntime.rvn")),
            RavenGreenNodeGenerator.GenerateNode(expression), RavenGreenNodeGenerator.GenerateNode(sample),
            RavenGreenNodeGenerator.GenerateFactory(sample)! };
        var runtimeDirectory = Path.GetDirectoryName(typeof(object).Assembly.Location)!;
        var references = new[] { typeof(object).Assembly.Location, typeof(Enumerable).Assembly.Location,
            Path.Combine(runtimeDirectory, "System.Runtime.dll"), Path.Combine(runtimeDirectory, "System.Collections.dll") }
            .Distinct().Select(path => MetadataReference.CreateFromFile(path)).ToArray();
        var trees = sources.Select(source => SyntaxTree.ParseText(source)).ToArray();
        var compilation = Compilation.Create("GeneratedGreen" + Guid.NewGuid().ToString("N"), trees, references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithFrameworkProjectionMode(FrameworkProjectionMode.None));
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine, diagnostics));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        image.Position = 0;
        var context = new AssemblyLoadContext("GeneratedGreen", isCollectible: true);
        try
        {
            var assembly = context.LoadFromStream(image);
            var method = assembly.GetType("Raven.CodeAnalysis.Syntax.InternalSyntax.Probe")!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
            Assert.Equal(42, method.Invoke(null, null));
        }
        finally { context.Unload(); }
    }

    private static void AssertParses(string source, string name)
    {
        var errors = SyntaxTree.ParseText(source).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error);
        Assert.True(!errors.Any(), name + Environment.NewLine + string.Join(Environment.NewLine, errors));
    }
}
