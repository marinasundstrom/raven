extern alias Seed;

using System.Reflection;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
public sealed class SymbolDisplayFormatParityTests
{
    [Fact]
    public void RavenPresetsAndEnumsMatchBootstrapConfiguration()
    {
        var bootstrapType = typeof(Seed::Raven.CodeAnalysis.SymbolDisplayFormat);
        var portType = typeof(Raven.CodeAnalysis.SymbolDisplayFormat);
        Assert.NotSame(bootstrapType.Assembly, portType.Assembly);
        var options = bootstrapType.GetProperties(BindingFlags.Public | BindingFlags.Instance);
        foreach (var preset in bootstrapType.GetProperties(BindingFlags.Public | BindingFlags.Static))
        {
            var bootstrap = preset.GetValue(null);
            var port = portType.GetProperty(preset.Name)!.GetValue(null);
            foreach (var option in options)
            {
                var portOption = portType.GetProperty(option.Name)!;
                Assert.Equal(Convert.ToInt64(option.GetValue(bootstrap)), Convert.ToInt64(portOption.GetValue(port)));
                Assert.Equal(Enum.GetNames(option.PropertyType), Enum.GetNames(portOption.PropertyType));
                Assert.Equal(Enum.GetValues(option.PropertyType).Cast<object>().Select(Convert.ToInt64),
                    Enum.GetValues(portOption.PropertyType).Cast<object>().Select(Convert.ToInt64));
            }
        }
    }
}
