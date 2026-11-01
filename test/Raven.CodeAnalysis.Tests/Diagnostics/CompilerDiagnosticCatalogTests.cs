using System.Reflection;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

[Trait("Target", "Shared")]
[Collection("Compiler diagnostic catalog")]
public sealed class CompilerDiagnosticCatalogTests
{
    [Fact]
    public void CatalogPropertiesLookupAndIdsAgree()
    {
        var descriptors = CompilerDiagnostics.AllDescriptors;
        Assert.NotEmpty(descriptors);
        Assert.Same(descriptors, CompilerDiagnostics.AllDescriptors);
        Assert.Same(CompilerDiagnostics.AllDiagnosticIds, CompilerDiagnostics.AllDiagnosticIds);
        Assert.Equal(descriptors.Length, descriptors.Select(d => d.Id).Distinct(StringComparer.Ordinal).Count());
        Assert.Equal(descriptors.Select(d => d.Id).OrderBy(id => id, StringComparer.Ordinal),
            CompilerDiagnostics.AllDiagnosticIds.OrderBy(id => id, StringComparer.Ordinal));
        var properties = typeof(CompilerDiagnostics).GetProperties(BindingFlags.Public | BindingFlags.Static)
            .Where(property => property.PropertyType == typeof(DiagnosticDescriptor)).ToArray();
        Assert.Equal(descriptors.Length, properties.Length);
        foreach (var property in properties)
        {
            var descriptor = Assert.IsType<DiagnosticDescriptor>(property.GetValue(null));
            Assert.Same(descriptor, property.GetValue(null));
            Assert.Same(descriptor, CompilerDiagnostics.GetDescriptor(descriptor.Id));
            Assert.Contains(descriptor, descriptors);
            Assert.True(CompilerDiagnostics.IsDiagnosticDefined(descriptor.Id));
        }
    }

    [Fact]
    public void OverrideMessageContainsPlainApostrophes()
    {
        Assert.Equal("'{0}': no suitable {1} found to override", CompilerDiagnostics.OverrideMemberNotFound.MessageFormat.ToString());
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("UNKNOWN")]
    [InlineData("rav0024")]
    public void MissingIdsReturnNoDescriptor(string? id)
    {
        Assert.False(CompilerDiagnostics.IsDiagnosticDefined(id!));
        Assert.Null(CompilerDiagnostics.GetDescriptor(id!));
    }
}
