extern alias Seed;

using System.Reflection;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
[Collection("Compiler diagnostic catalog")]
public sealed class GeneratedDiagnosticCatalogParityTests
{
    [Fact]
    public void EveryDescriptorMatchesTheSeedCatalog()
    {
        var seedType = typeof(Seed::Raven.CodeAnalysis.DiagnosticDescriptor).Assembly
            .GetType("Raven.CodeAnalysis.CompilerDiagnostics", throwOnError: true)!;
        var portType = typeof(Raven.CodeAnalysis.CompilerDiagnostics);
        Assert.NotSame(seedType.Assembly, portType.Assembly);
        Assert.Equal("Raven.CodeAnalysis.Raven", portType.Assembly.GetName().Name);
        var seedProperties = seedType.GetProperties(BindingFlags.Public | BindingFlags.Static)
            .Where(property => property.PropertyType == typeof(Seed::Raven.CodeAnalysis.DiagnosticDescriptor))
            .OrderBy(property => property.Name, StringComparer.Ordinal).ToArray();
        var portProperties = portType.GetProperties(BindingFlags.Public | BindingFlags.Static)
            .Where(property => property.PropertyType == typeof(Raven.CodeAnalysis.DiagnosticDescriptor))
            .OrderBy(property => property.Name, StringComparer.Ordinal).ToArray();
        Assert.Equal(seedProperties.Select(property => property.Name), portProperties.Select(property => property.Name));
        for (var i = 0; i < seedProperties.Length; i++)
        {
            var seed = (Seed::Raven.CodeAnalysis.DiagnosticDescriptor)seedProperties[i].GetValue(null)!;
            var port = (Raven.CodeAnalysis.DiagnosticDescriptor)portProperties[i].GetValue(null)!;
            Assert.Equal(seed.Id, port.Id);
            Assert.Equal(seed.Title.ToString(), port.Title.ToString());
            Assert.Equal(seed.Description?.ToString(), port.Description?.ToString());
            Assert.Equal(seed.MessageFormat.ToString(), port.MessageFormat.ToString());
            Assert.Equal(seed.Category, port.Category);
            Assert.Equal(seed.HelpLinkUri, port.HelpLinkUri);
            Assert.Equal((int)seed.DefaultSeverity, (int)port.DefaultSeverity);
            Assert.Equal(seed.IsEnabledByDefault, port.IsEnabledByDefault);
        }
        var seedDescriptors = (Seed::Raven.CodeAnalysis.DiagnosticDescriptor[])seedType.GetProperty("AllDescriptors")!.GetValue(null)!;
        Assert.Equal(seedDescriptors.Select(descriptor => descriptor.Id),
            Raven.CodeAnalysis.CompilerDiagnostics.AllDescriptors.Select(descriptor => descriptor.Id));
    }
}
