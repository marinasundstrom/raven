using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ForStatementSemanticTests : CompilationTestBase
{
    [Fact]
    public void ForEach_PrefersGenericEnumeratorOverNonGeneric()
    {
        const string source = """
import System.*
import System.Linq.*

class C {
    Test() {
        let type = typeof(System.Object)
        let members = type.GetMembers()

        for each member in members.Where(x => x.Name.Equals("Equals")) {
            _ = member.Name
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();

        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();
        boundFor.Iteration.Kind.ShouldBe(ForIterationKind.Generic);
        boundFor.Iteration.ElementType.ToDisplayString().ShouldBe("System.Reflection.MemberInfo");
        boundFor.Local.Type.ToDisplayString().ShouldBe("System.Reflection.MemberInfo");
    }
}
