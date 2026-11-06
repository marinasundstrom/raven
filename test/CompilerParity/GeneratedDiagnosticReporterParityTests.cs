extern alias Seed;
using System.Reflection;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
[Collection("Compiler diagnostic catalog")]
public sealed class GeneratedDiagnosticReporterParityTests
{
    [Fact]
    public void HandwrittenSeverityOverloadSharesTheGeneratedContainer()
    {
        var seedBag = new Seed::Raven.CodeAnalysis.DiagnosticBag();
        Seed::Raven.CodeAnalysis.DiagnosticBagExtensions.ReportObsoleteMember(seedBag, "method", "Old", "use New", Seed::Raven.CodeAnalysis.Location.None, Seed::Raven.CodeAnalysis.DiagnosticSeverity.Error);
        var portBag = new DiagnosticBag();
        portBag.ReportObsoleteMember("method", "Old", "use New", Location.None, DiagnosticSeverity.Error);
        var expected = Assert.Single(seedBag.AsEnumerable());
        var actual = Assert.Single(portBag.AsEnumerable());
        Assert.Equal(expected.Id, actual.Id);
        Assert.Equal(expected.GetMessage(), actual.GetMessage());
        Assert.Equal(DiagnosticSeverity.Error, actual.Severity);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EveryGeneratedReporterPreservesItsDiagnosticAndArgumentOrder(bool nullArguments)
    {
        var expected = typeof(Seed::Raven.CodeAnalysis.DiagnosticBagExtensions)
         .GetMethods(BindingFlags.Public | BindingFlags.Static).Where(m => m.Name.StartsWith("Report"))
         .Where(m => m.GetParameters().Last().ParameterType.Name == "Location" &&
          m.GetParameters().Skip(1).SkipLast(1).All(p => p.ParameterType == typeof(object)))
         .OrderBy(m => m.Name).ToArray();
        var actual = typeof(DiagnosticBagExtensions)
         .GetMethods(BindingFlags.Public | BindingFlags.Static).Where(m => m.Name.StartsWith("Report"))
         .Where(m => m.GetParameters().Last().ParameterType.Name == "Location" &&
          m.GetParameters().Skip(1).SkipLast(1).All(p => p.ParameterType == typeof(object)))
         .OrderBy(m => m.Name).ToArray();
        Assert.True(expected.Select(m => m.Name).SequenceEqual(actual.Select(m => m.Name)), $"Expected {expected.Length}, actual {actual.Length}; missing: {string.Join(",", expected.Select(m => m.Name).Except(actual.Select(m => m.Name)))}; extra: {string.Join(",", actual.Select(m => m.Name).Except(expected.Select(m => m.Name)))}");
        for (var i = 0; i < expected.Length; i++)
        {
            var seedBag = new Seed::Raven.CodeAnalysis.DiagnosticBag();
            var portBag = new DiagnosticBag();
            var parameters = expected[i].GetParameters();
            Assert.Equal(parameters.Select(p => p.Name), actual[i].GetParameters().Select(p => p.Name));
            Assert.True(actual[i].IsDefined(typeof(ExtensionAttribute), false));
            var arguments = parameters.Skip(1).Take(parameters.Length - 2).Select(p => nullArguments ? null : (object?)p.Name).ToArray();
            expected[i].Invoke(null, new object?[] { seedBag }.Concat(arguments).Append(Seed::Raven.CodeAnalysis.Location.None).ToArray());
            actual[i].Invoke(null, new object?[] { portBag }.Concat(arguments).Append(Location.None).ToArray());
            var seed = Assert.Single(seedBag.AsEnumerable());
            var port = Assert.Single(portBag.AsEnumerable());
            Assert.Equal(seed.Id, port.Id);
            Assert.Equal(seed.GetMessage(), port.GetMessage());
            Assert.Equal((int)seed.Severity, (int)port.Severity);
        }
    }
}
