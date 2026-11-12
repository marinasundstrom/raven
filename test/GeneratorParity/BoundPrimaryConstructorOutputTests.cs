using BoundNodeGenerator;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "Shared")]
public sealed class BoundPrimaryConstructorOutputTests
{
    [Fact]
    public void PrimaryConstructorDefinesCanonicalUpdateState()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-bound-primary-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            File.WriteAllText(Path.Combine(directory, "Nodes.cs"), """
                namespace Raven.CodeAnalysis;
                internal sealed partial class BoundLock(BoundExpression expression, BoundStatement body) : BoundStatement
                {
                    public BoundLock(BoundExpression expression, BoundStatement body, bool ignored) : this(expression, body) { }
                    public BoundExpression Expression { get; } = expression;
                    public BoundStatement Body { get; } = body;
                }
                """);
            var node = Assert.Single(SpecificationLoader.LoadBoundNodes(directory));
            Assert.Equal(new[] { "expression", "body" }, node.Parameters.Select(parameter => parameter.ParameterName));
            Assert.All(node.Parameters, parameter => Assert.Equal(ParameterKind.BoundNode, parameter.Kind));
            var source = RavenBoundTreeGenerator.Generate([node], [node.Name]);
            Assert.Contains("return BoundLock(@expression, @body)", source);
            Assert.DoesNotContain("@ignored", source);
            var csharpSource = BoundNodePartialGenerator.Generate(node);
            Assert.Contains("return new BoundLock(expression, body)", csharpSource);
            Assert.DoesNotContain("ignored", csharpSource);
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }
}
