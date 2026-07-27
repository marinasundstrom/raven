using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class RedundantPublicAccessibilityTests : CompilationTestBase
{
    [Fact]
    public void ExplicitPublicTypeMember_ReportsRedundantModifier()
    {
        const string source = """
class C {
    public func M() -> unit { return; }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics()
            .Where(diagnostic => diagnostic.Id == CompilerDiagnostics.PublicModifierRedundant.Id)
            .ToArray();

        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal(DiagnosticSeverity.Warning, diagnostic.Severity);
    }

    [Fact]
    public void ExplicitPublicNamespaceLevelType_DoesNotReportRedundantModifier()
    {
        const string source = """
public class C {}
public struct S {}
public interface I {}
public enum E { A }
public delegate D()
public union U { case A }
public extension X for int {}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics()
            .Where(diagnostic => diagnostic.Id == CompilerDiagnostics.PublicModifierRedundant.Id)
            .ToArray();

        Assert.Empty(diagnostics);
    }

    [Fact]
    public void ExplicitPublicNestedType_ReportsRedundantModifier()
    {
        const string source = """
class C {
    public class NestedClass {}
}

interface I {
    public class NestedInterfaceClass {}
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics()
            .Where(diagnostic => diagnostic.Id == CompilerDiagnostics.PublicModifierRedundant.Id)
            .ToArray();

        Assert.Equal(2, diagnostics.Length);
    }
}
