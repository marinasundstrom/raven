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
import System.Collections.Generic.*
import System.Reflection.*
import System.Linq.*

let members = List<System.Reflection.MemberInfo>()

for each member in members.Where(x => x.Name.Equals("Equals")) {
    let name = member.Name
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

        boundFor.Iteration.ElementType.Name.ShouldBe("MemberInfo");
        boundFor.Iteration.ElementType.ContainingNamespace.ShouldNotBeNull();
        boundFor.Iteration.ElementType.ContainingNamespace!.Name.ShouldBe("Reflection");
        boundFor.Iteration.ElementType.ContainingNamespace!.ContainingNamespace?.Name.ShouldBe("System");

        boundFor.Local.Type.Name.ShouldBe("MemberInfo");
        boundFor.Local.Type.ContainingNamespace.ShouldNotBeNull();
        boundFor.Local.Type.ContainingNamespace!.Name.ShouldBe("Reflection");
        boundFor.Local.Type.ContainingNamespace!.ContainingNamespace?.Name.ShouldBe("System");
    }
}
