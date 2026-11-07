using BoundNodeGenerator;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "Shared")]
public sealed class BoundHierarchyOutputTests
{
    [Fact]
    public void ClosedFamiliesAreAbstractAndDoNotBecomeVisitorCases()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-bound-model-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            File.WriteAllText(Path.Combine(directory, "Nodes.cs"), """
                namespace Raven.CodeAnalysis;
                internal closed class BoundExpression : BoundNode;
                internal sealed partial class BoundLiteral : BoundExpression;
                """);
            var nodes = SpecificationLoader.LoadBoundNodes(directory);
            Assert.True(Assert.Single(nodes, node => node.Name == "BoundExpression").IsAbstract);
            var visitor = BoundTreeVisitorGenerator.GenerateVisitor(nodes);
            Assert.Contains("case BoundLiteral n:", visitor);
            Assert.DoesNotContain("case BoundExpression n:", visitor);
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void RavenPermitsIncludeIntermediateFamiliesAndOnlyPortedLeaves()
    {
        BoundNodeModel[] model =
        [
            new("BoundExpression", "Raven.CodeAnalysis", "internal", "BoundNode", false, true, []),
            new("BoundPattern", "Raven.CodeAnalysis", "internal", "BoundExpression", false, true, []),
            new("BoundA", "Raven.CodeAnalysis", "internal", "BoundPattern", true, false, []),
            new("BoundB", "Raven.CodeAnalysis", "internal", "BoundPattern", true, false, []),
            new("BoundUnported", "Raven.CodeAnalysis", "internal", "BoundPattern", true, false, [])
        ];
        var source = RavenBoundTreeGenerator.Generate(model, ["BoundB", "BoundA"]);
        Assert.Equal(source, RavenBoundTreeGenerator.Generate(model.Reverse().ToArray(), ["BoundA", "BoundB"]));
        Assert.Contains("sealed partial class BoundNode permits BoundExpression", source);
        Assert.Contains("sealed partial class BoundExpression permits BoundPattern", source);
        Assert.Contains("sealed partial class BoundPattern permits BoundA, BoundB", source);
        Assert.DoesNotContain("BoundUnported", source);
        Assert.Contains("internal open partial class BoundTreeVisitor", source);
    }
}
