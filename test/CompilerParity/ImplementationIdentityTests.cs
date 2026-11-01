using System.Reflection;

using Raven.CodeAnalysis.Text;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
public sealed class ImplementationIdentityTests
{
    [Fact]
    public void DotNet_LoadsTheSelectedImplementation()
    {
        var expected = typeof(ImplementationIdentityTests).Assembly
            .GetCustomAttributes<AssemblyMetadataAttribute>()
            .Single(attribute => attribute.Key == "CompilerImplementationAssembly").Value;
        Assert.Equal(expected, typeof(TextSpan).Assembly.GetName().Name);
        Assert.Same(typeof(TextSpan).Assembly, typeof(LinePosition).Assembly);
        Assert.Same(typeof(TextSpan).Assembly, typeof(LinePositionSpan).Assembly);
        Assert.Same(typeof(TextSpan).Assembly, typeof(FileLinePositionSpan).Assembly);
        Assert.Same(typeof(TextSpan).Assembly, typeof(SourceText).Assembly);
        Assert.Same(typeof(TextSpan).Assembly, typeof(TextLine).Assembly);
        Assert.Same(typeof(TextSpan).Assembly, typeof(TextLineCollection).Assembly);
    }
}
