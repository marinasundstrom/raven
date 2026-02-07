using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class DiagnosticPropertiesTests
{
    [Fact]
    public void Create_WithProperties_SetsProperties()
    {
        var descriptor = CreateDescriptor("RAV9998", "Name '{0}' is invalid.");
        var properties = ImmutableDictionary<string, string?>.Empty
            .Add("SuggestedName", "newVariableName")
            .Add("ReplacementText", "Console.WriteLine(\"Hello\")");

        var diagnostic = Diagnostic.Create(descriptor, Location.None, properties);

        diagnostic.Properties.ShouldBe(properties);
        diagnostic.GetMessageArgs().ShouldBeEmpty();
    }

    [Fact]
    public void Create_WithSeverityAndProperties_PreservesSeverity()
    {
        var descriptor = CreateDescriptor("RAV9997", "Name is invalid.");
        var properties = ImmutableDictionary<string, string?>.Empty
            .Add("SuggestedName", "renamed");

        var diagnostic = Diagnostic.Create(descriptor, Location.None, DiagnosticSeverity.Warning, properties);

        diagnostic.Severity.ShouldBe(DiagnosticSeverity.Warning);
        diagnostic.Properties.ShouldBe(properties);
    }

    [Fact]
    public void Equals_WithDifferentProperties_IsFalse()
    {
        var descriptor = CreateDescriptor("RAV9996", "Name is invalid.");
        var left = Diagnostic.Create(
            descriptor,
            Location.None,
            ImmutableDictionary<string, string?>.Empty.Add("SuggestedName", "alpha"));
        var right = Diagnostic.Create(
            descriptor,
            Location.None,
            ImmutableDictionary<string, string?>.Empty.Add("SuggestedName", "beta"));

        left.Equals(right).ShouldBeFalse();
        (left == right).ShouldBeFalse();
    }

    private static DiagnosticDescriptor CreateDescriptor(string id, string messageFormat)
        => DiagnosticDescriptor.Create(
            id,
            "Test title",
            "Test description",
            helpLinkUri: string.Empty,
            messageFormat,
            category: "Testing",
            defaultSeverity: DiagnosticSeverity.Error);
}
