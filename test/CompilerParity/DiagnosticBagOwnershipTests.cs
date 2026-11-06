using System.Reflection;

using Raven.CodeAnalysis;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
public sealed class DiagnosticBagOwnershipTests
{
    [Fact]
    public void BagAndReportersUsePortOwnedDiagnosticContracts()
    {
        var assembly = typeof(Diagnostic).Assembly;
        Assert.Equal("Raven.CodeAnalysis.Raven", assembly.GetName().Name);
        Assert.Same(assembly, typeof(DiagnosticBag).Assembly);
        Assert.Same(assembly, typeof(DiagnosticBagExtensions).Assembly);
        Assert.False(typeof(DiagnosticBag).IsSealed);
        Assert.Equal(typeof(Diagnostic), typeof(DiagnosticBag).GetMethod("Report")!.GetParameters().Single().ParameterType);
        Assert.Equal(typeof(IEnumerable<Diagnostic>), typeof(DiagnosticBag).GetMethod("AsEnumerable")!.ReturnType);
        var scope = assembly.GetType("Raven.CodeAnalysis.NonReportingScopeDisposable", throwOnError: true)!;
        Assert.False(scope.IsPublic);
        Assert.False(scope.IsSealed);
        Assert.True(typeof(IDisposable).IsAssignableFrom(scope));
        Assert.Equal(typeof(DiagnosticBag), scope.GetConstructors().Single().GetParameters().Single().ParameterType);
    }
}
