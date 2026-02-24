using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ExplicitPublicAccessibilityOptionTests : CompilationTestBase
{
    [Fact]
    public void MembersPublicByDefault_Disabled_DoesNotReportDiagnostic()
    {
        const string source = """
extension NumberExtensions for int {
    func AddOne() -> int => self + 1
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics()
            .Where(d => d.Id == CompilerDiagnostics.ExplicitPublicAccessibilityRequired.Id)
            .ToArray();

        Assert.Empty(diagnostics);
    }

    [Fact]
    public void MembersPublicByDefault_ExplicitlyDisabled_ReportsCompilationWarning()
    {
        const string source = """
extension NumberExtensions for int {
    func AddOne() -> int => self + 1
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithMembersPublicByDefault(false));

        var diagnostic = Assert.Single(compilation.GetDiagnostics()
            .Where(d => d.Id == CompilerDiagnostics.ExplicitPublicAccessibilityRequired.Id));

        Assert.Equal("RAV0907", diagnostic.Id);
        Assert.Equal(DiagnosticSeverity.Warning, diagnostic.Severity);
        Assert.Equal(Location.None, diagnostic.Location);
    }

    [Fact]
    public void MembersPublicByDefault_ExplicitlyDisabled_ReportsSingleWarning()
    {
        const string source = """
extension NumberExtensions for int {
    public func AddOne() -> int => self + 1
    private func Hidden() -> int => self
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithMembersPublicByDefault(false));

        var diagnostics = compilation.GetDiagnostics()
            .Where(d => d.Id == CompilerDiagnostics.ExplicitPublicAccessibilityRequired.Id)
            .ToArray();

        Assert.Single(diagnostics);
    }

    [Fact]
    public void MembersPublicByDefault_Enabled_ReportsRedundantPublicModifier()
    {
        const string source = """
class C {
    public func M() -> unit { return; }
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithMembersPublicByDefault(true));

        var diagnostics = compilation.GetDiagnostics()
            .Where(d => d.Id == CompilerDiagnostics.PublicModifierRedundantInPublicByDefaultMode.Id)
            .ToArray();

        Assert.Single(diagnostics);
        Assert.Equal(DiagnosticSeverity.Warning, diagnostics[0].Severity);
    }

    [Fact]
    public void MembersPublicByDefault_Enabled_DoesNotReportDisabledModeWarning()
    {
        const string source = """
class C {
    func M() -> unit { return; }
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithMembersPublicByDefault(true));

        var diagnostics = compilation.GetDiagnostics()
            .Where(d => d.Id == CompilerDiagnostics.ExplicitPublicAccessibilityRequired.Id)
            .ToArray();

        Assert.Empty(diagnostics);
    }
}
