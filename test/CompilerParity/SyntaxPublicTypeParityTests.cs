extern alias Seed;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
public sealed class SyntaxPublicTypeParityTests
{
    [Fact]
    public void PublicSyntaxTypes_HaveRavenOwnedCounterparts()
    {
        var bootstrap = typeof(Seed::Raven.CodeAnalysis.Syntax.SyntaxNode).Assembly;
        var port = typeof(SyntaxNode).Assembly;
        Assert.NotSame(bootstrap, port);
        var actual = port.GetExportedTypes().Select(type => type.FullName).ToHashSet();
        var missing = bootstrap.GetExportedTypes()
            .Where(type => type.Namespace == typeof(SyntaxNode).Namespace)
            .Select(type => type.FullName)
            .Where(name => !actual.Contains(name))
            .OrderBy(name => name);
        Assert.Empty(missing);
    }
}
