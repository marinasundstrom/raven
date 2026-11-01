using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Semantics.Tests;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ExplicitExtensionMetadataTests : CompilationTestBase
{
    [Theory]
    [InlineData("")]
    [InlineData("[System.Runtime.CompilerServices.ExtensionAttribute]")]
    public void AttributedMethodMarksPartialContainerExactlyOnce(string containerAttribute)
    {
        var (compilation, _) = CreateCompilation($$"""
        {{containerAttribute}}
        public static partial class Helpers {
            static func Ordinary() -> int => 0
        }
        public static partial class Helpers {
            [System.Runtime.CompilerServices.ExtensionAttribute]
            static func Twice(value: int) -> int => value * 2
        }
        public static class OrdinaryHelpers {
            static func Identity(value: int) -> int => value
        }
        """);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var helpers = loaded.Assembly.GetType("Helpers")!;
        Assert.Single(helpers.GetCustomAttributes(typeof(ExtensionAttribute), false));
        Assert.Single(helpers.GetMethod("Twice")!.GetCustomAttributes(typeof(ExtensionAttribute), false));
        Assert.Empty(helpers.GetMethod("Ordinary")!.GetCustomAttributes(typeof(ExtensionAttribute), false));
        Assert.Empty(loaded.Assembly.GetType("OrdinaryHelpers")!.GetCustomAttributes(typeof(ExtensionAttribute), false));
        Assert.Equal(42, helpers.GetMethod("Twice")!.Invoke(null, [21]));
    }
}
