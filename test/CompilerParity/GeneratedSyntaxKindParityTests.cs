extern alias Seed;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
public sealed class GeneratedSyntaxKindParityTests
{
    [Fact]
    public void GeneratedSyntaxKinds_MatchSeedNamesAndNumericValues()
    {
        var seed = typeof(Seed::Raven.CodeAnalysis.Syntax.SyntaxKind);
        var port = typeof(Raven.CodeAnalysis.Syntax.SyntaxKind);
        Assert.NotSame(seed.Assembly, port.Assembly);
        var token = typeof(Raven.CodeAnalysis.Syntax.InternalSyntax.Parser.Token);
        Assert.Equal(port.Assembly, token.Assembly);
        Assert.Equal(port, token.GetProperty("Kind")!.PropertyType);
        Assert.Equal("Raven.CodeAnalysis.Raven", port.Assembly.GetName().Name);
        Assert.Equal(Enum.GetUnderlyingType(seed), Enum.GetUnderlyingType(port));
        Assert.Equal(Enum.GetNames(seed), Enum.GetNames(port));
        Assert.Equal(Enum.GetValues(seed).Cast<object>().Select(Convert.ToInt32),
            Enum.GetValues(port).Cast<object>().Select(Convert.ToInt32));
    }
}
