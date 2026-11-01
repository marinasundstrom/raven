using System.Reflection;

using Raven.CodeAnalysis.Text;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

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
        Assert.Same(typeof(TextSpan).Assembly, typeof(AnsiColor).Assembly);
        Assert.Same(typeof(TextSpan).Assembly, typeof(AnsiExtensions).Assembly);
        Assert.Same(typeof(TextSpan).Assembly, typeof(SeekableTextSource).Assembly);
        Assert.True(typeof(SeekableTextSource).IsSealed);
        Assert.False(typeof(SeekableTextSource).IsAbstract);
        Assert.Empty(typeof(SeekableTextSource).GetFields(
            BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly));
    }
}
