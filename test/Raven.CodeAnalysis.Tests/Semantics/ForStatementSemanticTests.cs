using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Testing;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ForStatementSemanticTests : CompilationTestBase
{
    [Fact]
    public void ForEach_WithDiscardIdentifier_DoesNotCreateLocal()
    {
        const string source = """
import System.Collections.Generic.*

let numbers = List<System.Int32>()

for each _ in numbers {
    let value = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();

        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();
        boundFor.Local.ShouldBeNull();
        boundFor.Iteration.ElementType.SpecialType.ShouldBe(SpecialType.System_Int32);
    }

    [Fact]
    public void ForEach_WithoutIdentifier_DoesNotCreateLocal()
    {
        const string source = """
import System.Collections.Generic.*

let numbers = List<System.Int32>()

for each in numbers {
    let value = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var forStatement = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();

        var boundFor = model.GetBoundNode(forStatement).ShouldBeOfType<BoundForStatement>();
        boundFor.Local.ShouldBeNull();
        boundFor.Iteration.ElementType.SpecialType.ShouldBe(SpecialType.System_Int32);
    }
}
