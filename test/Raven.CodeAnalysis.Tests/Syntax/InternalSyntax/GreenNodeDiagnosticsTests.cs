using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Shouldly;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Tests;

public class GreenNodeDiagnosticsTests
{
    [Fact]
    public void WithAdditionalDiagnostics_AttachesDiagnostics()
    {
        var root = SyntaxTree.ParseText("class C {}\n").GetRoot().Green;
        root.GetDiagnostics().ShouldBeEmpty();

        var diagnostic = DiagnosticInfo.Create(
            CompilerDiagnostics.IdentifierExpected,
            new TextSpan(0, 0));

        var updated = root.WithAdditionalDiagnostics(diagnostic);

        updated.ShouldNotBe(root);
        updated.GetDiagnostics().Count().ShouldBe(1);
        updated.GetDiagnostics().Single().Descriptor.ShouldBe(CompilerDiagnostics.IdentifierExpected);
    }
}
