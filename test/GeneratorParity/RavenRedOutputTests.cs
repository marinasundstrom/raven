using System.Reflection;
using System.Runtime.Loader;

using NodesShared;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.Generators;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "DotNet")]
public class RavenRedOutputTests
{
    [Fact]
    public void RepositoryRedNodesParseAsRaven()
    {
        var nodes = NodeEmitterInputs.Nodes();
        foreach (var node in nodes)
        {
            var subtypes = nodes.Where(n => n.Inherits == node.Name).Select(n => n.Name + "Syntax").ToArray();
            var source = RavenRedNodeGenerator.GenerateNode(node, subtypes);
            var errors = SyntaxTree.ParseText(source).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error);
            Assert.True(!errors.Any(), node.Name + Environment.NewLine + string.Join(Environment.NewLine, errors));
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GeneratedRedHierarchyPreservesPositionsCachingUpdatesAndVisitorDispatch(bool documentDiagnosticsFirst)
    {
        var expression = new SyntaxNodeModel
        {
            Name = "Expression",
            Inherits = "Node",
            IsAbstract = true,
            Slots = [new() { Name = "Keyword", Type = "Token", IsAbstract = true }]
        };
        var primary = new SyntaxNodeModel { Name = "PrimaryExpression", Inherits = "Expression", IsAbstract = true };
        var sample = new SyntaxNodeModel
        {
            Name = "Sample",
            Inherits = "PrimaryExpression",
            HasExplicitKind = true,
            Slots = [new() { Name = "Keyword", Type = "Token", IsInherited = true },
                new() { Name = "Child", Type = "Expression", IsNullable = true }, new() { Name = "Items", Type = "List<Expression>" }]
        };
        var nodes = new List<SyntaxNodeModel> { expression, primary, sample };
        var greenFixture = File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "GreenOutputRuntime.rvn"));
        greenFixture = "import System.*\nimport System.Collections.Generic.*\nimport System.Linq.*\n" +
            greenFixture[greenFixture.IndexOf("namespace Raven.CodeAnalysis.Syntax.InternalSyntax {", StringComparison.Ordinal)..];
        // Public red value-wrapper constructors expose these fixture green types.
        greenFixture = greenFixture.Replace("internal open class GreenNode", "public open class GreenNode")
            .Replace("internal abstract class SyntaxNode", "public abstract class SyntaxNode")
            .Replace("internal class SyntaxToken", "public class SyntaxToken")
            .Replace("internal class SyntaxList", "public class SyntaxList")
            .Replace("internal class DiagnosticInfo", "public class DiagnosticInfo");
        var sources = new List<string> { greenFixture, File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "RedOutputRuntime.rvn")) };
        sources.AddRange(nodes.Select(RavenGreenNodeGenerator.GenerateNode));
        sources.Add(RavenGreenNodeGenerator.GenerateFactory(sample)!);
        sources.AddRange(nodes.Select(n => RavenRedNodeGenerator.GenerateNode(n, [])));
        foreach (var ns in new[] { "Raven.CodeAnalysis.Syntax", "Raven.CodeAnalysis.Syntax.InternalSyntax" })
        {
            sources.Add(RavenVisitorGenerator.GenerateVisitor(nodes, false, !ns.Contains("InternalSyntax"), ns));
            sources.Add(RavenVisitorGenerator.GenerateVisitor(nodes, true, !ns.Contains("InternalSyntax"), ns));
        }
        sources.Add(RavenRewriterGenerator.GenerateRewriter(nodes, false, "Raven.CodeAnalysis.Syntax.InternalSyntax"));
        var runtimeDirectory = Path.GetDirectoryName(typeof(object).Assembly.Location)!;
        var references = new[] { typeof(object).Assembly.Location, typeof(Enumerable).Assembly.Location,
            Path.Combine(runtimeDirectory, "System.Runtime.dll"), Path.Combine(runtimeDirectory, "System.Collections.dll") }
            .Distinct().Select(path => MetadataReference.CreateFromFile(path)).ToArray();
        var trees = sources.Select((source, index) => SyntaxTree.ParseText(source, path: $"red-{index}.rvn")).ToArray();
        var compilation = Compilation.Create("GeneratedRed" + Guid.NewGuid().ToString("N"), trees, references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithFrameworkProjectionMode(FrameworkProjectionMode.None));
        if (documentDiagnosticsFirst)
        {
            foreach (var tree in trees)
                Assert.DoesNotContain(compilation.GetSemanticModel(tree).GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        }
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join(Environment.NewLine,
            diagnostics.Select(d => $"{d.Location.SourceTree?.FilePath}:{d.Location.SourceSpan.Start}: {d}")));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        image.Position = 0;
        var context = new AssemblyLoadContext("GeneratedRed", isCollectible: true);
        try
        {
            var assembly = context.LoadFromStream(image);
            var method = assembly.GetType("Raven.CodeAnalysis.Syntax.RedProbe")!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
            Assert.Equal(10, method.Invoke(null, null));
        }
        finally { context.Unload(); }
    }
}
